(define-library (opus search)

   (export
      start-search
      search)

   (import
      (owl base)
      (opus log))

   (begin
     
      (define search-server-name 'search)

      (define (stop-logging)
         (log "logger stopping")
         (mail 'log (tuple 'stop)))

      (define (search str)
         (print "Search running, str " str)
         (interact search-server-name
            (tuple 'search str)))

      (define empty-cache null)
      (define max-cache-size 50)
    
      (define (cache-push cache str res)
         (cons (cons str res)
            (if (< (length cache) max-cache-size)
               cache
               (reverse (cdr (reverse cache))))))
               
      ;; find and remove from list
      (define (cache-find lst str)
         (cond
            ((null? lst)
               (values lst #false))
            ((equal? (caar lst) str)
               (values (cdr lst) (cdar lst)))
            (else
               (lets ((tl res (cache-find (cdr lst) str)))
                  (values (cons (car lst) tl) res)))))

      ;; find and lift to top if match
      (define (cache-lookup cache str)
         (lets ((cache res (cache-find cache str)))
            (if res 
               (values (cons (cons str res) cache) res)
               (values cache res))))

      (define (match-prefix pat lst)
         (cond
            ((null? pat) #true)
            ((null? lst) #false)
            ((eq? (car pat) (car lst))
               (match-prefix (cdr pat) (cdr lst)))
            (else #false)))

      (define (count-matches pat lst n)
         (if (null? lst)
            n
            (count-matches pat (cdr lst)
               (+ n (if (match-prefix pat lst) 1 0)))))

      (define upcase-offset (- #\A #\a))

      (define (downcase char)
         (if (and (<= #\A char) (<= char #\Z))
            (- char upcase-offset)
            char))

      ;; search from node database
      (define (db-lookup db str)
         (print "db lookup for '" str "'")
         (let ((pat (string->list str)))
            (ff-fold
               (λ (found id node)
                  (let ((txt (get node 'text "")))
                     (let ((count (count-matches pat (map downcase (string->list txt)) 0)))
                        (if (eq? count 0)
                           found
                           (cons (cons count (cons id node)) found)))))
               null db)))
    
      (define (search-cached db cache str)
         (lets ((cache res (cache-lookup cache str)))
            (if res
               (begin
                  (print "search cache hit for '" str "'")
                  (values db cache res))
               (lets ((res (db-lookup db str)))
                  (print "adding '" str "' to search cache")
                  (values db (cache-push cache str res) res)))))
    
      (define (do-search db cache str)
         (let ((dbp (interact 'blag 'everything)))
            (if (eq? dbp db)
               ;; cache valid
               (search-cached db cache str)
               ;; recomputation needed
               (search-cached dbp empty-cache str))))
      
      ;; → ((n . (id . node)) ...)
      (define (searcher db cache)
         (lets ((env (wait-mail))
                (from msg env))
            (tuple-case msg
               ((search str)
                  (lets 
                     ((str (list->string (map downcase (string->list str))))
                      (db cache res (do-search db cache str)))
                     (print "search for '" str "' found " (length res) " matches.")
                     (mail from res)
                     (searcher db cache)))
               ((cache)
                  (mail from (map car cache))
                  (searcher db cache))
               (else
                  (print "bad search message")
                  (mail from 'wat)
                  (searcher db cache)))))

      (define (start-search)
         (fork-server search-server-name
            (lambda ()
               (searcher #false null))))

))




