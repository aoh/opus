
(define-library (opus blag)

   (import (owl base))

   (export blag-parse)

   (begin

      (define fold-stops '(pre href))

      ;; fixme: do not descend to attributes
      (define (node-fold op st node)
        (cond
          ((null? node)
            (values st node))
          ((pair? node)
            (if (has? fold-stops (car node)) ;; acts as a quote
              (values st node)
              (lets
                ((st hd (node-fold op st (car node)))
                 (st tl (node-fold op st (cdr node))))
                (values st (cons hd tl)))))
          ((string? node)
            (op st node))
          (else
            (values st node))))

      (define (header-tag-of str)
        (cond
          ((equal? str "#") 'h1)
          ((equal? str "##") 'h2)
          ((equal? str "###") 'h3)
          ((equal? str "####") 'h4)
          ((equal? str "#####") 'h5)
          (else 'h6)))

      (define (parse-headings st str)
        (define (parse str)
          (let ((res (M/(.*?)\r?\n(#+)  *([^\n]*)(.*)/ str)))
            (if res
              (ilist (car res)
                 (list 
                    (header-tag-of (cadr res))
                    (caddr res))
                 (parse (cadddr res)))
              (list str))))
        (lets ((lst (parse str)))
          (values st (cons 'cat lst))))

      (define (emphasis-tag-of str)
        (cond
          ((equal? str "**") 'em)
          ((equal? str "*") 'b)
          ((equal? str "_") 'u)
          ((equal? str "`") 'code)
          ((equal? str "__") 'u)
          ((equal? str "~~") 's)
          (else
            (print "Unknown emphasis: " str)
            'u)))

      (define (parse-emphasis st str)
        (define (parse str)
          (let ((res (M/(.*?)(\*\*|\*|__|_|~~|`)(.+?)\2(.*)/ str)))
            (if res
              (ilist (car res)
                 (list 
                    (emphasis-tag-of (cadr res))
                    (caddr res))
                 (parse (cadddr res)))
              (list str))))
        (lets ((lst (parse str)))
          (values st 
            (if (= (length lst) 1)
              (car lst)
              (cons 'cat lst)))))

      (define (parse-code-blocks st str)
        (define (parse str)
          (let ((res (M/(.*?)\r?\n```(.*?)\r?\n```(.*)/ str)))
            (if res
              (ilist (car res)
                `(pre ((class "code")) ,(cadr res))
                (parse (caddr res)))
              (list str))))
        (lets ((lst (parse str)))
          (values st 
            (if (= (length lst) 1)
              (car lst)
              (cons 'cat lst)))))

      (define (parse-links st str)
        (define (parse str)
          (let ((res (M/(.*?)([a-z]+:\/\/[^ \n\t\r]+)(..*)/ str)))
            (if res
              (ilist (car res)
                `(a ((href ,(cadr res))) ,(cadr res))
                (parse (caddr res)))
              (list str))))
        (lets ((lst (parse str)))
          (values st 
            (if (= (length lst) 1)
              (car lst)
              (cons 'cat lst)))))

      (define (parse-labeled-links st str)
        (define (parse str)
          (let ((res (M/(.*?)\[([^\n]+?)\]\(([^\n]+)\)(.*)/ str)))
            (if res
              (ilist (car res)
                `(a ((href ,(caddr res))) ,(cadr res))
                (parse (cadddr res)))
              (list str))))
        (lets ((lst (parse str)))
          (values st 
            (if (= (length lst) 1)
              (car lst)
              (cons 'cat lst)))))

      (define (parse-images st str)
        (define (parse str)
          (let ((res (M/(.*?)!\[([^\n]+?)\]\(([^ \n]+)\)(.*)/ str)))
            (if res
              (ilist (car res)
                `(img ((src ,(caddr res)) (alt ,(cadr res))))
                (parse (cadddr res)))
              (list str))))
        (lets ((lst (parse str)))
          (values st 
            (if (= (length lst) 1)
              (car lst)
              (cons 'cat lst)))))

      (define (parse-horizontal-rules st str)
        (define (parse str)
          (let ((res (M/(.*?)(\*{3,}|-{3,}|_{3,})(.*)/ str)))
            (if res
              (ilist (car res)
                 '(hr)
                 (parse (caddr res)))
              (list str))))
        (lets ((lst (parse str)))
          (values st 
            (if (= (length lst) 1)
              (car lst)
              (cons 'cat lst)))))

      (define (parse-headings st str)
        (define (parse str)
          (let ((res (M/(.*?)\r?\n(#+)  *([^\n]*)(.*)/ str)))
            (if res
              (ilist (car res)
                 (list 
                    (header-tag-of (cadr res))
                    (caddr res))
                 (parse (cadddr res)))
              (list str))))
        (lets ((lst (parse str)))
          (values st (cons 'cat lst))))

      (define (parse-lpk-links st s)
        (define (parse s)
          (let ((res (M/(.*?)(LPK-[0-9]+)(.*)/ s)))
            (if res
              (ilist (car res) 
               `(a ((href ,(str "https://issues.solita.fi/browse/" (cadr res)))) ,(cadr res))
               (cddr res))
              (list s))))
        (lets ((lst (parse s)))
          (values st
            (if (= (length lst) 1)
               (car lst)
               (cons 'cat lst)))))

      (define (tag-link tag val)
        `(a ((href ,(str "/index/" tag))) ,(str "#" tag (if val (str ":" val) ""))))

      (define (add-tag st tag val)
        (put st 'tags
          (put (get st 'tags #empty) 
            tag (or val #true))))

      (define (parse-tags st str)
        (define (parse st str)
          (let ((res (M/(.*?)#([a-zåäöÅÄÖA-Z0-9-_/]+)(:[^ \r\n\t]+| ?)(.*)/ str)))
            (if res
              (apply 
                (λ (pre tags val tail)
                  (lets
                    ((tag (string->symbol tags))
                     (val (s/^[: ]// val))
                     (val (if (equal? val "") #false val))
                     (st tail (parse st tail)))
                    (values (add-tag st tag val)
                      (ilist pre (tag-link tag val) (cons " " tail)))))
                res)
              (values st (list str)))))
        (lets ((st lst (parse st str)))
          (values st 
              (if (null? (cdr lst))
                  (car lst)
                  (cons 'cat lst)))))

      (define (parse-paragraphs st str)
        (values st
          (let ((res (c/\r?\n[\r\n]+/ str)))
            (if (> (length res) 1)
              (ilist 'cat
                (car res)
                (map 
                  (lambda (x) (list 'p x))
                  (cdr res)))
              str))))
       
      ;;; ---------------------------------------------------------
      ;;; List parser
      ;;;

      (define (list-begin? x) (or (eq? x #\-) (eq? x #\+)))

      (define (list-depth line)
         (let loop ((lst (string->list line)) (n 0))
            (cond
               ((null? lst) (values #false #false))
               ((eq? (car lst) #\space)
                  (loop (cdr lst) (+ n 1)))
               ((and (list-begin? (car lst)) (pair? (cdr lst)) (eq? (cadr lst) #\space))
                  (values (car lst) (+ n 2)))
               (else
                  (values #false #false)))))

      (define (indent-depth line)
         (let loop ((lst (string->list line)) (n 0))
            (cond
               ((null? lst) n)
               ((eq? (car lst) #\space)
                  (loop (cdr lst) (+ n 1)))
               (else n))))

      (define (sdrop str n)
         (list->string (drop (string->list str) n)))

      (define (list-entry-lines lst depth seen)
         (cond
            ((null? lst) (values (reverse seen) lst))
            ((>= (indent-depth (car lst)) depth)
               (list-entry-lines (cdr lst) depth 
                  (cons (sdrop (car lst) depth) seen)))
            (else
               (values (reverse seen) lst))))

      (define (maybe-highlight type line)
         (if (eq? type #\+)
            `(b ,line)
            line))

      (define (process-list lst find-lists)
         (lets ((type depth (list-depth (car lst)))) ;; count spaces, dash and space
            (let loop ((lst lst) (entries null))
               (cond
                  ((null? lst)
                     (values (reverse entries) lst))
                  ((m/^[ \r\t]*$/ (car lst)) ;; drop empty lines
                     (loop (cdr lst) entries))
                  (else
                     (lets ((type this (list-depth (car lst))))
                        (if (equal? this depth)
                           (lets ((lines lst (list-entry-lines (cdr lst) depth (list (sdrop (car lst) depth))))) ;; belonging to this element
                              (if (null? (cdr lines))
                                 (loop lst (cons `(li ,(maybe-highlight type (car lines))) entries))
                                 (loop lst (cons `(li . ,(find-lists lines)) entries))))
                           (values (reverse entries) lst))))))))

      (define (find-lists lines)
         (cond
            ((null? lines)
               null)
            ((m/^ *[-+] / (car lines))
               (lets ((lnodes lst (process-list lines find-lists)))
                  (cons 
                     (cons 'ul lnodes)
                     (find-lists lst))))
            (else
               (let ((lst (find-lists (cdr lines))))
                  (cond
                     ((null? lst) lines)
                     ((string? (car lst))
                        (cons
                           (str (car lines) "\n" (car lst))
                           (cdr lst)))
                     (else
                        (cons (str (car lines) "\n") lst)))))))
            
      (define (parse-lists st str)
         (values st
           (let ((res (find-lists (c/\r?\n/ str))))
              (cond
                 ((null? res) str)
                 ((null? (cdr res)) str)
                 (else (cons 'cat res))))))


      ;;; ----------------------------------------------------------

      (define (blag-parser st node)
        (lets 
          ((st node (node-fold parse-headings st node))
           (st node (node-fold parse-code-blocks st node))
           (st node (node-fold parse-lists st node))            ;; ei koodiblokkien sisältä
           (st node (node-fold parse-tags st node))
           (st node (node-fold parse-paragraphs st node))
           (st node (node-fold parse-horizontal-rules st node))
           (st node (node-fold parse-images st node))           ;; ! followed by link
           (st node (node-fold parse-labeled-links st node))
           (st node (node-fold parse-lpk-links st node))
           (st node (node-fold parse-emphasis st node)))
          (values st node)))

      (define (find-title node)
        (cond
          ((string? node)
            (if (m/^[ \n\t\r]*$/ node)
              #false
              (s/^[ \n]+// (s/[ \n]+$// node))))
          ((pair? node)
            (or (find-title (car node))
                (find-title (cdr node))))
          (else #false)))

      (define (blag-parse env str)
        (lets
          ((st (-> empty (put 'text str)))
           (str (string-append "\n" str)) ;; hack
           (st node (blag-parser st str))
           (title (or (find-title node) "Untitled"))
           (st (-> st
                   (put 'title title)
                   (put 'type 'blag)
                   (put 'timestamp (time-ms)))))
          (values #true (put st 'node node))))))

