#!/usr/bin/ol --run

;; add post origin/referer check

(import
  (opus http)
  (opus html)
  (owl codec)
  (owl intern)
  (opus log)
  (opus search)
  (opus blag)
  (opus db)
  (owl args)
  (owl sys)
  (only (kal main) kal-string))

(define ssl-only #false)

;; cut-at '(a b c d e) 'c -> '(a b) '(d e)
(define (cut-at lst x)
   (let loop ((lst lst) (rout null))
      (if (null? lst)
         (values #f #f)
         (let ((hd (car lst)))
            (if (eq? hd x)
               (values (reverse rout) (cdr lst))
               (loop (cdr lst) (cons hd rout)))))))

(define (can-delete? user id)
   (and user
      (eq? user (getf (db-get id) 'owner))))
      
(define (whois str)
   (lets ((idbs tlbs (cut-at (string->list str) #\-)))
      (if idbs
         (lets
            ((id (string->symbol (list->string idbs)))
             (node (db-get id)))
            (cond
               ((not node) #f)
               ((not (equal? "user" (getf node 'type))) #f)
               ((not (equal? (get (get node 'tags empty) 'session #f)
                             (list->string tlbs)))
                  #f)
               (else id)))
         #f)))

(define favicon
   (list->byte-vector
         '(0 0 1 0 1 0 15 15 2 0 1 0 1 0 168 0 0 0 22 0 0 0 40 0 0 0 15 0 0 0 30 0 0 0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 249 62 0 0 240 30 0 0 229 78 0 0 234 174 0 0 197 70 0 0 202 166 0 0 197 70 0 0 195 134 0 0 198 198 0 0 232 46 0 0 228 78 0 0 246 222 0 0 241 30 0 0 224 14 0 0 239 238 0 0 249 62 0 0 240 30 0 0 229 78 0 0 234 174 0 0 197 70 0 0 202 166 0 0 197 70 0 0 195 134 0 0 198 198 0 0 232 46 0 0 228 78 0 0 246 222 0 0 241 30 0 0 224 14 0 0 239 238 0 0)))

(define session-id "sid")

(define opus-prefix "./")

(define blag-fasl-file 
  (string-append opus-prefix "/blag.fasl"))

;; temp, fixed
(define humans 
"Site owner: Aki Helin
Twitter: @aohelin
Email: aohelin@gmail.com, aki.helin@iki.fi
Github: https://github.com/aoh
")

(define robots-txt
"User-agent: *
Disallow: /injure-human-beings
Disallow: /disobey-orders
Disallow: /harm-to-self
")

;; temp, fixed
(define style-sheet 
"html, body { font-size: 18px; line-height: 130%; font-family: \"Verdana\", \"Helvetica\", \"Arial\", \"Sans Serif\"; background-color: #fffdfa; margin: 0px; padding: 0px; height: 100%; }
.btn       { background: none; border: none; color: #008888; text-decoration: none; font-size: 100%; cursor: pointer; }
select       { background: none; border: none; color: #008888; text-decoration: none; font-size: 100%; cursor: pointer; }
.sbtn      { visibility: hidden; background: none; border: none; color: #008888; text-decoration: none; font-size: 100%; cursor: pointer; }
.btn:hover { background: none; font-size: 100%; border: none; color: #00aaaa; text-decoration: underline; cursor: pointer; }
a          { background: none; border: none; color: #008888; text-decoration: none; cursor: pointer; }
a:hover    { background: none; border: none; color: #00aaaa; text-decoration: underline; cursor: pointer; }
.ok        { margin-top: 30%; border: solid #008800 1px; background-color: #eeffee; color: #000000; padding: 10px; }
.ok-note   { overflow: auto; margin-top: 10px; border: solid #008800 1px; background-color: #eeffee; color: #000000; width: 100%; }
.fail-note   { overflow: auto; margin-top: 10px; border: solid red 2px; background-color: #ffeedd; color: #000000; width: 100%; }
.fail      { margin-top: 30%; border: solid red    1px; background-color: #ffccaa; color: #000000; padding: 10px; }
ul         { margin-top: 0px; padding-top: 0px; }
textarea   { font-size: 18px; background-color: #fffdfa; border: solid #eeeeee 1px; padding: 20px; overflow: auto; outline: none; width:100%; }
.important { margin-top: 30%; border: solid #cccc44 1px; background-color: #ffffcc; color: #000000; padding: 10px; }
pre {
  -webkit-box-shadow: 6px 14px 32px -5px rgba(50,40,0,0.16);
  -moz-box-shadow: 6px 14px 32px -5px rgba(50,40,0,0.16);
  box-shadow: 6px 14px 32px -5px rgba(50,40,0,0.16);
}
h1         { font-size: 120%; margin-top: 10px; text-shadow: 1px 1px 2px rgba(150, 150, 150, 1); color: #0a0a06; }
h2         { font-size: 108%; margin-top: 10px; color: #080804;}
h3,h4,h5,h6 { font-size: 105%; margin-top: 10px; }
h1, h2, h3 { font-family: \"Serif\"; }
p          { margin-bottom: 0px; margin-top: 15px; }
form       { display: inline; }
.dtop      { color: #fffdfa; background-color: #000000; text-align: right; padding: 2px; padding-right: 8px; margin: 0px; }
.dmid      { padding-left: 4%; padding-right: 4%; background-color: #fffdfa; padding-top: 0px; margin-top: 0px; min-height: 100%; }
.dbot      { text-align: center; width: 100%; background-color: #000000; color: #446666; font-size: 80%; padding-bottom: 5px; padding-top: 10px; }
.code      { border: solid #dddded 1px; padding: 30px; margin-left: 30px; }
.search    { background-color: #000000; color: #888888; border: none; }
.searchbtn { background-color: #fffdfa; color: #444422; border: none; }
hr         { border: 0; height: 0; border-top: solid 2px rgba(128, 128, 128, 0.1); border-bottom: solid 2px rgba(255, 255, 255, 0.6); }
@media only screen and (min-width: 1200px) {
  .dmid      { padding-left: 20%; padding-right: 20%; }
  h1         { font-size: 250%; padding-bottom: 10px; padding-top: 40px; text-shadow: 1px 1px 4px rgba(150, 150, 150, 1); color: #0a0a06; }
  h2         { font-size: 190%; padding-bottom: 10px; padding-top: 30px; color: #080804; }
  h3         { font-size: 140%; padding-bottom: 10px; padding-top: 20px; }
  h4         { font-size: 120%; padding-top: 5px; }
  h5         { font-size: 110%; }
  h6         { font-size: 105%; }
}
")

(define separator 
  ;'(span ((style "color: #cccccc")) " | ")
  "&nbsp;"
  )

(define menu-separator 
  '(span ((style "color: #444444")) " | ")
  ;"&nbsp;"
  )

(define icon-settings "&#9881;")
(define icon-confirm "&#9888;")

(define (page-title title id)
   (let ((title (or title "opus")))
      (if id
         (str id "@" title)
         title)))

(define (unreadable? env node)
   (let ((tags (get node 'tags empty)))
      (cond
         ((getf tags 'public)
            ;; everyonecan see it
            #false)
         ((eq? (getf env 'id) (getf node 'owner))
            ;; not my documnt
            #false)
         ((getf tags 'key) =>
            (λ (key)
               ;; key given in link?
               (if (equal? (getf env 'sid) key)
                  #false
                  #true)))
         (else
            #true))))

(define (readable? env node)
   (not (unreadable? env node)))

(define (blag-pins env)
   (let ((posts (get-db)))
      (cons 'cat
         (ff-fold
            (λ (out id val)
               (let ((pin (getf (get val 'tags empty) 'pin)))
                  (cond
                     ((unreadable? env val) out)
                     (pin
                        (ilist `(a ((href ,(str "/n/" id))) ,pin) menu-separator out))
                     (else out))))
            null posts))))

(define (search-box env)
   `(form ((method "post") (action "/search"))
      (input ((type "text") (name "q") (class "search") (tabindex "1") (placeholder "search...") (size 10)))
      (input ((type "submit") (class "sbtn") (value "search")))))

(define (alist-ref al key def)
   (cond
      ((null? al) def)
      ((equal? (caar al) key) 
         (cdar al))
      (else
         (alist-ref (cdr al) key def))))

(define (post-param env val def)
   (alist-ref (get env 'post-params null) val def))

;; find val from get or post params (or maybe header)
(define (request-param env val)
   (or
      ;(alist-ref (get env 'get-params null) val #false)
      (alist-ref (get env 'post-params null) val #false)))

(define (carriables lst ret)
   (cond
      ((null? lst) null)
      ((null? (cdr lst)) 
         (ret #false))
      (else
         (cons
            (list (car lst) (cadr lst))
            (carriables (cddr lst) ret)))))

;; linketxt x url x [variable x value]* X ....
(define (parse-dialog-options str)
   (lets/cc ret ((opts (map c/x/ (c/X/ str))))
      (cons 'ul
         (map
            (lambda (option)
               (let ((os (map hex-decode option)))
                  (cond
                     ((not (all (λ (x) x) os))
                        (ret "broken dialog"))
                     ((= (length os) 2)
                        `(li (a ((href ,(cadr os))) ,(car os))))
                     ((> (length os) 2)
                        `(li (a ((href ,(cadr os) . ,(carriables (cddr os) ret))) ,(car os))))
                     (else
                        (ret "bad")))))
            opts))))

(define (render-dialog env)
   (let ((icon (request-param env "i"))
         (header (request-param env "h"))
         (txt  (request-param env "t"))
         (class  (request-param env "c"))
         (os   (parse-dialog-options (or (request-param env "o") ""))))
        `(div ((class ,class))
            (h2 ,icon " " ,header)
            (hr)
            ,(or txt "")
            ,os)))

(define (dialog link-text class icon header text . opts)
   `(form ((action "/dialog") (method "POST"))
      (input ((class "btn") (type "submit") (name "op") (value ,link-text)))
      (input ((type "hidden") (name "i") (value ,icon)))
      (input ((type "hidden") (name "h") (value ,header)))
      (input ((type "hidden") (name "t") (value ,text)))
      (input ((type "hidden") (name "c") (value ,class)))
      (input ((type "hidden") (name "o")
         (value ,(foldr string-append "" (interleave "X" opts)))))))

(define (dialog-option text url . vars)
   (foldr string-append ""
      (interleave "x"
         (map (o hex-encode str) (ilist text url vars)))))

(define (subdialog lt class ico head txt . opts)
   (dialog-option lt "/dialog"
      'i ico 'h head 'c class 't txt 'o 
         (foldr string-append "" (interleave "X" opts))))

(define (opus-page env x title msg)
   `(html
       (head
          (title ,(page-title title (getf env 'id)))
          (style ,style-sheet)
          (meta ((name "viewport") (content "width=device-width, initial-scale=1")))
          (meta ((http-equiv "Content-Type") (content "text/html;charset=UTF-8")))
          (link ((type "text/plain") (rel "author") (href "http://opus.org/humans.txt"))))
       (body 
           (div ((class "dtop"))
              (span ((float "left") (style "color: #888888;") (id "hnote")) "")
              " "
              ,(blag-pins env)
              (a ((href "/")) "front")
              ,menu-separator
              (a ((href "/index")) "idx")
              ,menu-separator
              (a ((href "/new")) "new")
              ,menu-separator
              ,(let ((id (getf env 'id)))
               (if id `(a ((href ,(str "/n/" id))) ,(str id))
                      '(a ((href "/login")) "login")))
              ,menu-separator
              ,(search-box env))

           (div ((class "dmid"))
             ,(or msg "")
             ,x)
           (div ((class "dbot"))
              (a ((href "https://twitter.com/aohelin")) "@aohelin")
               " | " 
               (a ((href "https://github.com/aoh")) "aoh@github")))))

(define (success node url)
  `(div ((class "ok"))
      (h2 "&#x2714; Success")
      (hr)
      ,node
      (p (a ((href ,url)) "Continue journey"))))

(define (success-note node url)
   `(div ((class "ok-note"))
      (span ((style "float: left; padding: 6px;")) ,node) 
      (span ((style "float: right; padding: 6px;")) (a ((href ,(str "/n/" url))) "X"))))

(define (failure-note node url)
   `(div ((class "fail-note"))
      (span ((style "float: left; padding: 6px;")) ,node) 
      (span ((style "float: right; padding: 6px;")) (a ((href ,(str "/n/" url))) "X"))))

(define (failure node url)
  `(div ((class "fail"))
      (h2 "&#x2718; Failure")
      (hr)
      (p ,node)
      (p (a ((href ,url)) "Continue journey"))))

(define (confirm node url)
   `(div ((class "important"))
      (h2 "&#9888; Confirm")
      (hr)
      (p ,node)
      (ul
        (li (a ((href ,url)) "Yes"))
        (li (a ((href "/"))  "No")))))

(define (post-options env id post)
  (dialog icon-settings "ok" icon-settings "options" 
       (str "What would you like to do to '" (get post 'title "untitled") "'?")
       (dialog-option "read it" (str "/n/" id))
       (dialog-option "edit it" (str "/edit/" id))
       ;(dialog-option "ledit it" (str "/n/ledit/" id))
       (dialog-option "get the source" (str "/n/" id ".txt"))
       (dialog-option "see all metadata" (str "/n/" id ".owl"))
       (subdialog "remove it" "important" icon-confirm "Confirmation required" 
         "Are you sure you want to remove it?" 
         (dialog-option "no" (str "/n/" id))
         (dialog-option "yes" (str "/delete/" id)))
       ))

(define (post-buttons env id post link?)
    `(div ((style "text-align: right; float: right;"))
       ;,(get post 'owner "no owner")
       ;,(if link?
       ;   `(cat
       ;      ,separator
       ;      (a ((href ,(str "/n/" id))) "&#x21b1;")) ; link &#x21b1; &#128279;
       ;    "")
       ;,separator
       (a ((href ,(str "/edit/" id))) "&#x270e;") ; edit &#x270e;
       ,separator
       ;,(if (getf env 'id)
       ;  '(a ((href "/")) "&#x2605;") ; start (logged in)
       ;  '(a ((href "/login")) "&#x2606;")) ; login (key)
       ;,separator
       ;(a ((href ,(str "/delete/" id))) "&#x2718;")
       ,(post-options env id post)))
   
(define (format-post env id post)
  `(div
     ((style "border-top: solid #eeeedd 1px; padding-bottom: 50px;"))
     ,(post-buttons env id post #true)
     ,(get post 'node "(no node)")
     ))

(define (list-posts env posts keys n)
   (if (or (null? keys) (= n 10))
      null
      (let ((post (getf posts (car keys))))
         (if (unreadable? env post)
            (list-posts env posts (cdr keys) n)
            (lets
              ((a (format-post env (car keys) post))
               (bs (list-posts env posts (cdr keys) (+ n 1))))
              (cons a bs))))))

(define (front-page env)
  (lets 
    ((posts (get-db))
     (keys (reverse (keys posts))))
    (if (null? keys)
      '(a ((href "/new")) "create something")
      (cons 'div 
         (list-posts env posts keys 0)))))

(define (get-post-param env key def)
   (alist-ref (get env 'post-params null) key def))

;;;
;;; Blag 
;;;

; blag node = ff with
;   text   → actual text content
;   node   → renderable content
;   serial → version number mod #xffff 

(define (maybe-carry-session env node sidname sid)
   ;(if (getf env 'x-sid)
   ;   node
   ;   (carry-session node sidname sid))
   node)

(define (blag-entry env id title node msg)
   (put env 'content
      (html
        (maybe-carry-session env
           (opus-page env node title msg)
           session-id
           (getf env 'sid)))))

(define (opus-handler env node)
   (put env 'content
      (html
         (maybe-carry-session env
            (opus-page env node #f #f)
            session-id 
            (getf env 'sid)))))

(define (opus-handler-nosession env node)
   (put env 'content
      (html (opus-page env node #f #f))))

(define (front-handler env)
   (put env 'content
      (html
         (maybe-carry-session env
            (opus-page env
              (let ((front (front-page env)))
                ; (print "made front " front)
                front)
              #f #f)
            session-id 
            (getf env 'sid)))))

;; node to post a new blag entry
(define blag-post-new
   `(div
      (form ((method "POST") (action "/save"))
        (table ((width "100%"))
          (tr
            (td "Title:" 
              (input ((type "text") (name "id") (placeholder "node id") (size 64)))
              )
            (td "Type:"
               (select ((name "type"))
                  (option ((value "blag") (default 1)) "blag")
                  (option ((value "user")) "user")
                  (option ((value "kal")) "kal")))
            (td ((style "text-align: right;"))
              (input ((type "hidden") (name "serial") (value 0)))
              (input ((type "submit") (class "btn") (name "op") (value "save")))))
          (tr
            (td ((colspan 3))
              (textarea ((rows 22) (id "ed") (name "body") (autofocus)))))))))

(define opus-login
   `(div
      (form ((method "POST") (action "/login"))
        ;(p (input ((type "text") (name ,session-id) (placeholder "mellon") (size 96))))
        ;(p (input ((class "btn") (type "submit") (name "op") (value "whisper"))))
        (p (input ((type "text") (name "login") (placeholder "login") (size 48))))
        (p (input ((type "password") (name "password") (placeholder "password") (size 48))))
        (p (input ((class "btn") (type "submit") (name "op") (value "login"))))
        )))

(define (opus-greeting id)
   (success (str "Hello " id "!") "/"))

(define (add-subject lst)
  (cond
    ((null? lst) lst)
    ((string? (car lst))
      (cons (list 'h1 (car lst)) (cdr lst)))
    (else lst)))

(define (cut-lines str)
  (cons 'div
    (add-subject 
      (interleave '(br)
        (c/\r?\n/ str)))))

(define (opus-parse env str type)
   (lets ((ok val (blag-parse env str)))
      (if ok
         (values ok (put val 'type (get (get val 'tags empty) 'type type)))
         (values ok val))))

(define (opus-parse-with-semantics env id type str)
   (cond
      ((equal? type "kal")
         (let ((res (kal-string str)))
            (if res
               (opus-parse env res type)
               (values #false "kal fail"))))
      ((equal? type "blag")
         (opus-parse env str type))
      ((equal? type "user")
         (lets ((ok val (opus-parse env str type)))
            (if (and ok 
                  (getf (get val 'tags empty) 'hash)
                  (getf (get val 'tags empty) 'session))
               (values ok 
                  ;; owner is the new user
                  (put val 'owner id))
               (values #false "bad user node, need #hash and #session"))))
      (else
         (print "Unknown semantics: " type)
         (values #false "Weird type"))))

(define (blag-edit env id value)
   `(div
      (form ((method "POST") (action "/save"))
        (table ((width "100%") (height "100%"))
          (tr
            (td ""
              )
            (td ((style "text-align: right;"))
              (input ((class "btn") (type "submit") (name "op") (value "save")))
              ;,separator
              ;(input ((class "btn") (type "submit") (name "op") (value "peek")))
              (input ((type "hidden") (name "id") (value ,(str id))))
              (input ((type "hidden") (name "type") (value ,(get value 'type "blag"))))
              (input ((type "hidden") (name "serial") (value ,(get value 'serial 0))))))
          (tr
            (td ((colspan 2))
              (textarea ((rows 22) (name "body") (autofocus))
                "\n"
                ,(str (get value 'text "")))))))))

(define (sorted-nodes posts)
   (sort 
      (lambda (a b) (> (get (cdr a) 'timestamp 0) (get (cdr b) 'timestamp 0))) 
      posts))

(define (post-index posts)
   (cons 'ul
      (foldr
         (lambda (node out)
            (cons
               `(li (a ((href ,(str "/n/" (car node)))) ,(get (cdr node) 'title "untitled")))
               out))
         null
         (sorted-nodes posts))))

(define (tag-link tags)
   (str "/index/" 
      (foldr
         (lambda (tag rest)
            (if (equal? rest "")
               (str tag)
               (str tag "/" rest)))
         "" tags)))
            
(define (tag-index posts selected-tags)
   (lets 
      ((tags
         (fold
            (λ (tags post)
                 (ff-fold
                    (λ (tags tag _)
                       (put tags tag (+ 1 (get tags tag 0))))
                    tags (get (cdr post) 'tags empty)))
            #empty posts))
       (tags
         (sort (λ (a b) (< (cdr a) (cdr b))) 
            (ff->list tags)))
       (tags
          (keep (lambda (tag) (not (mem eq? selected-tags (car tag)))) tags))
       (selectable
         (fold
            (λ (out p)
               (cons 
                  `(li (a ((href ,(tag-link (cons (car p) selected-tags)))) ,(str (car p))) ": " ,(cdr p))
                  out))
            null tags))
       (selected
          (map
             (lambda (tag)
                `(li (a ((href ,(tag-link (remove (lambda (x) (eq? x tag)) selected-tags)))) ,(str tag))))
             selected-tags)))
    (list 'cat
       (if (null? selectable) ""
          (list 'p (list 'b "Tags:") 
             (cons 'ul selectable)))
       (if (null? selected) ""
          (list 'p (list 'b "Selected:") 
             (cons 'ul selected))))))
 
(define (parse-tags tag-str)
   (map string->symbol (c/\// tag-str)))

(define (tags-match? tags post)
   (let ((tag-ff (get post 'tags #empty)))
      (fold
         (lambda (ok? tag)
            (cond
               ((not ok?) ok?)
               ((getf tag-ff tag) ok?)
               (else #f)))
         #true tags)))

;; db env (tag ...) -> ((id . node) ...)
(define (filter-posts posts env tags)
   (ff-fold
      (lambda (posts id post)
         (cond
            ((unreadable? env post)
               posts)
            ((tags-match? tags post)
               (cons (cons id post) posts))
            (else
               posts)))
      null posts))
               
(define (opus-index env tags)
   (lets 
      ((tags (parse-tags tags))
       (posts (filter-posts (get-db) env tags)))
      `(table ((width "100%"))
         (tr (td ((valign "top") (width "70%")) (p (b "Blag index: ") ,(post-index posts)))
             (td ((valign "top")) 
                ,(tag-index posts tags))))))

;; (n . (id . node))
(define (allowed-results env ress)
   (keep
      (lambda (res) (not (unreadable? env (cdr (cdr res)))))
      ress))

(define (car> a b)
   (> (car a) (car b)))

(define (format-search-result res)
   (lets
      ((count (car res))
       (id (cadr res))
       (node (cddr res)))
      `(li (a ((href ,(str "/n/" id))) ,(get node 'title "untitled")) " (" ,count ")")))

(define (content-search env)
   (lets 
      ((q (get-post-param env "q" #f))
       (ress (if q (search q) null))
       (allowed (allowed-results env ress)))
      (log "searching " q)
      (cond
         ((null? allowed)
            '(p "I found nothing suitable :("))
         (else
            (let ((count (fold + 0 (map car allowed))))
               `(cat
                  (p 
                     ,(if (= count 1) 
                        "I found a match"
                        (str "I found a total of " count " matches"))
                      " for '" ,q "'."
                     ,(cons 'ul
                        (map format-search-result 
                           (sort car> allowed))))))))))

;; may add 'sid and corresponding 'id to env
(define (add-identity env)
   (let ((val (or (getf env 'x-sid) 
                  ;(request-param env session-id)
                  (let ((cookie (getf env 'cookie)))
                     (if cookie (s/^sid=// cookie) #f)))))
     (if val
        (let ((id (whois val)))
           (if id
             (-> env 
               (put 'id id)
               (put 'sid val))
             (put env 'sid val))) ;; can be a valid guest id
        env)))


(define localhost (vector 127 0 0 1))

(define (localhost? ip) (equal? ip localhost))

(define (http-status env status)
   (put env 'status status))

(define (no-permission env rpath)
   (http-status
      (opus-handler env
         (failure '(p "You have no permission to do that you silly person.") rpath))
      401))

(define (not-there env rpath)
   (http-status
      (opus-handler env
         (failure '(p "There is no such anything.") rpath))
      404))

(define (render-node node)  
  (let ((ser (make-serializer empty)))
    (list->vector
      (force-ll (ser node null)))))

(define (has-key? env id)
   (lets
      ((node (db-get id))
       (tags (get (or node empty) 'tags empty))
       (key (get tags 'key #false)))
      (and key (equal? (getf env 'sid) key))))

(define (string->id x)
  (if (equal? x "") 
    #false)
    (or
      (string->integer x)
      (string->symbol x)))

(define (mime-type-of path)
   (let ((suf (s/.*\.// path)))
      (cond
         ((m/jpe?g/ suf) "image/jpeg")
         ((m/te?xt/ suf) "text/plain")
         ((m/png/ suf) "image/png")
         ((m/html?/ suf) "text/html")
         (else "application/octet-stream"))))
            
(define (maybe-upload opts env path)
   (cond
      ((m/\.\./ path)
         (not-there env "/"))
      ((getf opts 'webroot) =>
         (lambda (webroot)
            (if-lets ((data (file->vector (str webroot "/" path))))
               (-> env
                  (put 'status 200)
                  (put 'response-type (mime-type-of path))
                  (put 'content data))
               (not-there env "/"))))
      (else
         (not-there env "/"))))

(define (salty-hash a b)
   (sha256 (str "%n" a b)))

(define (try-login sname shash)
   (let ((id (string->symbol sname))
         (node (db-get (string->symbol sname))))
      (if node
         (begin
            (if (and (equal? "user" (getf node 'type))
                     ;; minor risk of side channel
                     (equal? shash (get (get node 'tags #empty) 'hash #false)))
               (values id (str id "-" (get (get node 'tags empty) 'session "42")))
               (values #false #false)))
         (values #f #f))))

(define (respond-opus-login env)
  (lets ((login (post-param env "login" #f))
         (pass  (post-param env "password" #f)))
     (if (and login pass)
        (lets ((hash  (salty-hash login pass))
               (maybe-id maybe-session (try-login login hash)))
           (if maybe-id
              (begin
                 (log "user " maybe-id " logs in, session " maybe-session)
                 (if (post-param env "getsession" #false)
                    (-> env
                       (put 'content maybe-session)
                       (put 'response-type "text/plain"))
                    (opus-handler 
                       (-> env
                          (add-response-header "Set-Cookie" (str "sid=" maybe-session "; HttpOnly" (if ssl-only "; Secure" "")))
                          (put 'sid maybe-session) ;; session id string
                          (put 'id maybe-id)) ;; user id, check 
                       (opus-greeting maybe-id))))
              (opus-handler-nosession env opus-login)))
        (opus-handler-nosession env opus-login))))

(define (opus-save env)
   (lets 
       ((data (getf env 'post-params))
        (body (if data (alist-ref data "body" #false) #false))
        (op (if data (alist-ref data "op" #false) #false))
        (type (if data (alist-ref data "type" #false) #false))
        (id (string->id (if data (alist-ref data "id" "") "")))
        (serial (string->number (if data (alist-ref data "serial" "") "")))
        (old-node (db-get id)))
      (if (or (and (not old-node) (getf env 'id)) ;; any logged in user can create
              (and old-node (eq? (getf env 'id) (getf old-node 'owner))) ;; owner can write
              (and old-node (has-key? env id))) ;; user has a key for this particular node
         (if (and data body op serial)
           (lets ((ok? val (opus-parse-with-semantics env id type body)))
             (cond
               ((not ok?)
                 (opus-handler env
                   (failure 
                     `(p "The computer says no. " ,(str val))
                     "/")))
               ((equal? op "save")
                 (if id
                    (lets
                       ;; user node adds the owner itself
                       ((me (getf env 'id))
                        (val (if (getf val 'owner)
                                 val
                                 (put val 'owner (if old-node (get old-node 'owner me) me))))
                        (val (put val 'serial serial))
                        (res (db-put id val)))
                       (if res
                          (blag-entry env id (getf val 'title)
                             `(cat ,(post-buttons env id val #false) ,(getf val 'node))
                             (success-note `(cat "Entry " (i ,id) " updated.") id))
                          (let ((node (db-get id)))
                             (blag-entry env id (getf node 'title)
                                `(cat ,(post-buttons env id val #false) ,(getf val 'node))
                                (failure-note `(cat "Edit collision - this is the current version") id)))))
                    ;; no id, take next free
                    (let ((id (db-add (put val 'owner (getf env 'id)))))
                       (opus-handler env
                         (success (str "Entry " id " created.") 
                            (str "/n/" id))))))
               (else
                 (opus-handler env
                   (failure '(p "What?") "/")))))
           (opus-handler env
             (failure '(p "The computer says " (code "no") ".") "/")))
       (no-permission env "/"))))

(define (make-router opts)
   (lambda (env)
      (lets 
        ((env
           (-> env
             (add-response-header 'x-frame-options "SAMEORIGIN")
             (add-response-header 'X-XSS-Protection "1; mode=block")
             (add-response-header 'X-Content-Type-Options "nosniff")
             (add-response-header 'Content-Security-Policy "default-src 'self'; style-src 'unsafe-inline'; script-src 'unsafe-inline'")))
         (env
            (if (and ssl-only (localhost? (getf env 'ip)))
               (add-response-header env 'Strict-Transport-Security "max-age=31536000; includeSubDomains")
               env)))
        (query-case (getf env 'query)
           (M/\/favicon\.ico/ ()
              (-> env
                 (put 'content favicon)
                 (put 'response-type "image/x-icon")))
           (M/\// ()
              (front-handler env))
           (M/\/index/ ()
              (opus-handler env (opus-index env "")))
           (M/\/index\/(.*)/ (tags)
              (opus-handler env (opus-index env tags)))
           (M/\/humans\.txt/ ()
              (-> env
                 (put 'content humans)
                 (put 'response-type "text/plain")))
           (M/\/new/ ()
              (opus-handler env blag-post-new))
           (M/\/save/ ()
               (opus-save env))
           (M/\/n\/([0-9a-zA-Z-]*)/ (ids)
              (lets ((n (string->id ids))
                     (node (db-get n)))
                  (cond
                     ((not node)
                        (not-there env "/"))
                     ((unreadable? env node)
                        (no-permission env "/"))
                     (else
                        (blag-entry env n (getf node 'title) 
                          `(cat ,(post-buttons env n node #false) ,(getf node 'node))
                          #f)))))
           (M/\/n\/([0-9a-zA-Z-]*)\.([a-z0-9]+)/ (ids type)
              (lets
                  ((n (string->id ids))
                   (node (db-get n)))
                  (cond
                     ((not node)
                        (not-there env "/"))
                     ((unreadable? env node)
                        (no-permission env "/"))
                     ((equal? type "txt")
                        (-> env
                           (put 'content (get node 'text ""))
                           (put 'response-type "text/plain; charset=utf-8")))
                     ((equal? type "owl")
                        (-> env
                           (put 'content (render-node node))
                           (put 'response-type "text/plain; charset=utf-8")))
                     (else
                        (fail env 418 "Semantics lacking.")))))
           (M/\/delete\/([0-9a-zA-Z-]*)/ (ids)
              (lets ((n (string->id ids)))
                  (if (can-delete? (getf env 'id) n)
                    (let ((node (db-del n)))
                       (if node
                          ;; should go to index instead
                          (opus-handler env
                             (success '(p "Entry annihilated with success") "/"))
                          (not-there env "/")))
                     (no-permission env "/"))))
           (M/\/edit\/([0-9a-zA-Z-]*)/ (ids)
              (lets
                 ((id (string->id ids))
                  (node (db-get id)))
                 (cond
                    ((not node)
                       (not-there env "/"))
                    ((unreadable? env node)
                       (no-permission env "/"))
                    (else
                       (opus-handler env
                          (blag-edit env id node))))))
           (M/\/login\/?/ ()
              (respond-opus-login env))
           (M/\/search/ ()
              (opus-handler env
                 (content-search env)))
           (M/\/dialog/ ()
              (opus-handler env
                 (render-dialog env)))
           (M/\/robots\.txt/ ()
              (-> env
                  (put 'content robots-txt)
                  (put 'response-type "text/plain")))
           (M/\/crash/ ()
              (car 'mr-crasher))
           (M/\/f\/([a-zA-Z0-9._-/]+)/ (path)
               (maybe-upload opts env path))
           (M/\/logout/ ()
              (-> env
                 (del 'id)
                 (add-response-header "Set-Cookie" (str "sid="))
                 (opus-handler '(p "bye bye"))))
           (M/(.*)/ (all)
              (put 
                 (opus-handler env 
                    (failure '(p "404 WAT") "/"))
                 'status 404))))))

(define (match-prefix lst pre)
   (cond
      ((null? pre) #true)
      ((null? lst) #false)
      ((eq? (car pre) (car lst))
         (match-prefix (cdr lst) (cdr pre)))
      (else #false)))

;; check that origin (or referer if not present) has as prefix one of the given values
(define (wrap-csrf settings handler)
   (let ((origins (map string->list (get settings 'origin null))))
      (lambda (env)
         (if (eq? (getf env 'http-method) 'post)
            (let ((origin (get env 'origin (getf env 'referer))))
               (if (and origin (let ((cs (string->list origin))) (some (lambda (x) (match-prefix cs x)) origins)))
                  (handler env)
                  (begin
                     (log "CSRF: dropping POST to " (getf env 'query) " from source " origin)
                     (no-permission env "/"))))
            (handler env)))))
         
(define (add-user id pass)
   (lets
      ((hash (salty-hash id pass))
       (session (sha256 (str salty-hash (time-ms))))
       (id (if (string? id) (string->symbol id) id))
       (page (str "#hash:" hash "\n#type:user\n#session:" session "\n"))
       (ok? val (opus-parse-with-semantics #empty 'aoh "user" page)))
      (if ok?
         (begin
            (db-put id (put val 'owner id))
            session)
         (begin
            (print "failed to add user " id)
            #false))))

(define (ip->str ip)
   (list->string
      (foldr
         (λ (thing tl) (render thing (if (null? tl) tl (cons #\. tl))))
         null
         (vector->list ip))))

(define (log-request-info handler env)
   (lets
      ((start (time-ms))
       (env (handler env))
       (elapsed (- (time-ms) start)))
      (log (ip->str (getf env 'ip)) " [" (getf env 'fd) "]: " (getf env 'http-method) " " (getf env 'query) " -> " (get env 'status 200) " (" elapsed "ms)")
      env))

(define (opus opts)
   (let ((router (make-router opts)))
      (lambda (env)
         (lets ((env (add-identity env)))
            (log-request-info router env)))))

(define (backupper path last)
   (let ((db (get-db)))
      (if (not (eq? db last))
         (if path
            (begin
               (log "backupper noticed changes")
               (log " - backup: " (db-save (str "backup/" (time) ".fasl")))
               (log " - fasl: " (db-save path)))
            (log "backupper would have stored changes")))
      (sleep 360000)
      (backupper path db)))

;; todo: should run this in a separate thread to kickstart parts as needed
(define (start opts port logfile fasl-file user pass)
   ;; blag db
   (start-logging logfile)
   (log "logger started")
   (db-start)
   (if fasl-file
      (db-load fasl-file))
   (fork-linked-server 'backupper (lambda () (backupper fasl-file empty)))
   (start-search)
   (let ((res (if (and user pass) (add-user user pass) #true)))
      (server 'serveri (wrap-csrf opts (opus opts)) port)
      res))

(define (restart)
   
   (log "restart")
   ;; keep the posts
   (mail 'blag (tuple 'update db-handler))

   (mail 'serveri
      (tuple 'update opus 
         (λ (state) state))))

(define (stop)
  (stop-logging)
  (if (db-save blag-fasl-file)
    (begin
      (log "db saved, stopping server")
      (interact 'serveri stop))
    (log "ERROR: FAILED TO SAVE DATA AT STOP")))

(print "Collecting symbols...")
;; all currently interned symbols
(define initial-symbols 
   (define (collect node tail)
      (if node
         (cons (ref node 2)
            (collect (ref node 1)
               (collect (ref node 3) tail)))
         tail))
   (collect (ref (interact 'intern null) 2) null))

(print "Initial symbols: " (length initial-symbols))

; (print "And now, (start 80 \"opus.log\" \"blag.fasl\" #f #f) or (restart)")
; (print "Dev run (start 9000 #f #f \"test\" \"pass\") and (restart)")

(define (string->port str)
   (let ((n (string->integer str)))
      (if (and n (> n 0) (< n 65536))
         n
         #false)))

(define (readable-directory path)
   (if (dir->list path)
      path
      #false))

;;; 
;;; Command Line
;;; 

(define usage-text "opus [args]")

(define command-line-rule-exp
   `((help "-h" "--help" comment "get help")
     (user "-U" "--user" has-arg comment "initial user name")
     (password "-P" "--password" has-arg comment "initial user password")
     (origin "-o" "--origin" plural has-arg comment "origin or referer from which to accept POST requests, e.g. http://myserver.com/")
     (port "-p" "--port" cook ,string->port
        default "80"
        comment "specify port to run on")
     (webroot "-w" "--webroot" cook ,readable-directory
        comment "webroot to serve files from (optional)")
     (ephemereal "-E" "--ephemereal"
        comment "start with empty db and do not persist anything")))

(define command-line-rules
   (cl-rules command-line-rule-exp))

(define (print-usage)
   (print usage-text)
   (print (format-rules command-line-rules)))

(define (start-opus dict args)
   (start-symbol-interner initial-symbols) ;; (re)start symbol interning
   (cond
      ((getf dict 'help)
         (print-usage))
      ((not (null? args))
         (print "What are these? -> " args)
         1)
      ((getf dict 'ephemereal)
         ;; note: should be made O(1)
         (lets ((user (getf dict 'user))
                (pass (getf dict 'password))
                (port (getf dict 'port)))
            (if (and user pass port)
               (begin
                  (print "Starting ephemereal server on port " port ", user " user ", pass " pass)
                  (start dict port #f #f user pass)
                  (let loop () (print (wait-mail)) (loop)))
               (begin
                  (print "You need to specify user, password and port")
                  1))))
      (else
         (print "No persisted mode for now. Use -E to test.")
         1)))

(lambda (args) 
   (process-arguments (cdr args)
       command-line-rules usage-text start-opus))


