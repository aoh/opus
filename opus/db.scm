(define-library (opus db)

   (import 
      (owl base)
      (opus log))

   (export get-db db-get db-del db-add db-put db-save db-load db-start db-handler)

  (begin

    (define (get-db)
      (interact 'blag 'everything))
      
    (define (db-get id)
      (interact 'blag (tuple 'get id)))

    (define (db-del id)
      (interact 'blag (tuple 'delete id)))

    (define (db-add val)
      (interact 'blag (tuple 'add val)))

    (define (db-put key val)
      (interact 'blag (tuple 'put key val)))

    (define (db-save path)
      (interact 'blag (tuple 'save path)))

    (define (db-load path)
      (interact 'blag (tuple 'load path)))

    (define (db-handler posts)
       (lets
          ((env (wait-mail))
           (from msg env))
          (cond
             ((eq? msg 'everything)
                (mail from posts)
                (db-handler posts))
             ((tuple? msg)
                (tuple-case msg
                   ((get what)
                      (let ((val (getf posts what)))
                         (if val
                            (if (getf val 'serial)
                               (mail from val)
                               (mail from (put val 'serial 0)))
                            (mail from #false)))
                      (db-handler posts))
                   ((delete what)
                      (mail from 'ok)
                      (db-handler (del posts what)))
                   ((save path)
                      (let ((res (fasl-save posts path)))
                         (mail from res)
                         (db-handler posts)))
                   ((put what value)
                      (let ((old (get posts what empty)))
                         (cond
                            ((eq? (get old 'serial 0) (get value 'serial 0))
                               (mail from 'ok)
                               (db-handler 
                                  (put posts what 
                                     (put value 'serial (band #xffff (+ (get old 'serial 0) 1))))))
                            (else
                               (mail from #false)
                               (db-handler posts)))))
                   ((add value)
                      (let ((k (+ 1 (ff-max posts 0))))
                         (mail from k)
                         (db-handler (put posts k (put value 'serial 0)))))
                   ((update handler)
                      (print "Updating blag handler")
                      (handler posts))
                   ((load path)
                      (let ((val (fasl-load path #false)))
                        (if val
                          (begin
                            (mail from #true)
                            (db-handler val))
                          (begin
                            (mail from #false)
                            (db-handler posts)))))
                   (else
                      (mail from 'wat)
                      (db-handler posts))))
             (else
                (mail from 'wat)
                (db-handler posts)))))

    (define (db-start)
       (fork-linked-server 'blag (λ () (db-handler empty))))))
