(define-library (opus html)

   (export 
      html
      carry-session)     ;; sexp varname value

   (import (owl base))

   (begin

      (define (gen-tail ls ret tail gen)
         (cond
            ((pair? ls)
               (gen (car ls) ret 
                  (gen-tail (cdr ls) ret tail gen)))
            ((null? ls) 
               tail)
            (else
               (print-to stderr "bad html: " ls)
               (ret #false))))

      ;; quote \:s and ":s with \. e.g. a""b → a\"\"b → a\\\"\\\"b → ...

      (define (quote-once lst tail)
         (foldr
            (λ (char tail)
               (if (or (eq? char #\") (eq? char #\\))
                  (ilist #\\ char tail)
                  (cons char tail)))
            tail lst))

      (define (render-quoted val tail)
         (cond
            ((string? val)
               (let ((cs (string->list val)))
                  (quote-once cs tail)))
            ((number? val)
               (render val tail))
            ((symbol? val)
               (render (str val) tail))
            (else
               (error "render-quoted: cannot handle this yet: " val))))

      ;; generate HTML for the attributes, with special handling for some of them
      (define (gen-attrs pairs tail)
         (if (null? pairs)
            tail
            (lets
               ((fst (car pairs))
                (rest (cons #\" (gen-attrs (cdr pairs) tail)))) ;; after current one
               (cons #\space 
                  (if (null? (cdr fst))
                    (render (car fst) tail)
                    (render (car fst)
                       (ilist #\= #\"
                          (render-quoted (cadr fst)
                             (cond
                                ((eq? (car fst) 'href)
                                   ;; render get parameters
                                   (if (null? (cddr fst))
                                      rest
                                      (cons #\?
                                         (foldr
                                            (λ (thing tail)
                                               (render-quoted (car thing) 
                                                  (cons #\=
                                                     (render-quoted (cadr thing)
                                                        (if (eq? (car tail) #\") ;; no & after last
                                                           tail
                                                           (cons #\& tail))))))
                                            rest (cddr fst)))))
                                (else
                                   rest))))))))))

      (define open-only?
         (lets
            ((keys '(p img area base basefont br hr col embed input keygen 
               link meta param track wbr))
             (ff (list->ff (zip cons keys keys))))
            (λ (x) (get ff x #false))))

      (define closed-tag?
         (lets 
            ((keys '(a abbr acronym address applet article aside audio b
               bdi bdo big blockquote body i div html head header title h1
               h2 h3 h4 h5 h6 table tbody tr textarea tfoot th thead time
               title td iframe ul ol li button canvas caption center cite
               code colgroup datalist dl dd del details dfn dialog dir dt em
               fieldset figcaption figure font footer form pre frame frameset
               ins kbd label legend main map mark menu menuitem meter nav
               noframes noscript object optgroup select option output prep progress
               q rp rt ruby s samp script section select small source span
               strike strong style sub summary sup tt u ul var video))
             (ff (list->ff (zip cons keys keys))))
            (λ (x) (get ff x #false))))
  
      (define (external-link? str)
         (m/^[a-z0-9]+:/ str))
      
      (define (add-href-attribute attribs var val)
         (cond
            ((null? attribs)
               #false)
            ((eq? (caar attribs) 'href)
               (if (external-link? (cadar attribs))
                  attribs
                  (cons 
                     (append (car attribs) (list  (list var val)))
                     (cdr attribs))))
            ((add-href-attribute (cdr attribs) var val) =>
               (λ (tail)
                  (cons (car attribs) tail)))
            (else #false)))
   
      ;; (a ((href link (arg val) ...)) "...")
      (define (simple-link-parts sexp)
         (let ((atts (cadr sexp)) (body (cddr sexp)))
            (if (and (= (length atts) 1) (eq? (caar atts) 'href)
                     (= (length body) 1) (string? (car body)))
               (let ((href (cdar atts)))
                  (values (car href) (cdr href) (car body)))
               (values #f #f #f))))

      (define (make-link-form href args text)
         (ilist 'form 
            `((method "POST") (action ,href))
            `(input ((type "submit") (class "btn") (value ,text)))
            (map 
               (λ (pair) 
                  `(input ((type hidden) (name ,(car pair)) (value ,(cadr pair)))))
               args)))

      ;; todo: rather persist var(s)
      (define (carry-session sexp var val)
         (cond
            ((not val)
               sexp)
            ((pair? sexp)
               (let ((hd (car sexp)))
                 (cond
                    ((eq? hd 'a)
                       (lets ((href args text (simple-link-parts sexp)))
                          (cond
                             ((not href)
                                (print-to stderr "cannot carry session for link: " sexp)
                                sexp)
                             ((m/:\// href) sexp) ;; external link
                             (else
                               (make-link-form href (cons (list var val) args) text)))))
                    ((eq? hd 'form)
                       (ilist 'form 
                          (append
                            (map 
                               (λ (x) (carry-session x var val)) 
                               (cdr sexp))
                            (list
                              `(input ((type "hidden") (name ,var) (value ,val)))))))
                    ((eq? hd 'quote)
                       sexp)
                    (else
                       (map (λ (x) (carry-session x var val)) sexp)))))
            (else sexp)))

      (define (render-quoting-tags str tail)
         (str-foldr
            (λ (c tail)
               (cond
                  ((eq? c #\<) (ilist #\& #\l #\t #\; tail))
                  ((eq? c #\>) (ilist #\& #\g #\t #\; tail))
                  (else (cons c tail))))
            tail str))

      (define (generate ret)

         (define (maybe-gen-attrs lst tail)
            (cond
               ((null? lst) (cons #\> tail))
               ((and (pair? (car lst)) (pair? (caar lst)))
                  (gen-attrs (car lst)
                     (cons #\> (foldr gen tail (cdr lst)))))
               (else
                  (cons #\> 
                     (foldr gen tail lst)))))

         (define (gen exp tail)
            (cond
               ((null? exp) tail)
               ((not exp) tail)
               ((pair? exp)
                  (cond
                     ((eq? (car exp) 'cat)
                        ;; just cat(enate) the following values
                        (foldr gen tail (cdr exp)))
                     ((eq? (car exp) 'quote)
                        (foldr render tail (cdr exp)))
                     (else
                        (let ((hd (car exp)))
                           (cons #\<
                              (cond
                                 ((open-only? hd)
                                    (render hd (maybe-gen-attrs (cdr exp) tail)))
                                 ((closed-tag? hd)
                                    (render hd 
                                       (maybe-gen-attrs (cdr exp)
                                          (ilist #\< #\/ 
                                             (render hd (cons #\> tail))))))
                                 (else
                                    (print-to stderr "bad html operator: " (car exp))
                                    (ret #false))))))))
               ((string? exp)
                  (render-quoting-tags exp tail))
               (else
                  (render exp tail))))

         gen)

      (define doctype "<!DOCTYPE html>\n")

      (define (html->x exp x)
         (call/cc 
            (lambda (ret)
               (let 
                  ((bs ((generate ret) exp null)))
                  ;; <!DOCTYPE html> could be added at this point
                  (if bs 
                     (x (append (string->list doctype) bs)) 
                     bs)))))

      (define (html exp)
         (html->x exp runes->string))

))


