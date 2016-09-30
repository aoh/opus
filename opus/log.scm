(define-library (opus log)

   (export
      start-logging
      stop-logging
      log warn error)

   (import
      (owl base))

   (begin

      (define (logger fd)
         (let loop ()
            (lets
               ((env (wait-mail))
                (from msg env))
               (tuple-case msg
                  ((log msgs)
                     (print-to fd (time) ": " (apply str msgs))
                     (loop))
                  ((stop)
                     (print "logger stop")
                     (close-port fd))
                  (else is foo
                     (print-to stderr "bad logger message: " foo))))))

      (define (stop-logging)
         (log "logger stopping")
         (mail 'log (tuple 'stop)))

      (define (log . strs)
         (mail 'log (tuple 'log strs)))

      (define (warn . strs)
         (apply log (cons "WARNING: " strs)))
      
      (define (error . strs)
         (apply log (cons "ERROR: " strs))
         (car 'exit-via-error))
               
      (define (start-logging path)
         (let
            ((fd (if (or (not path) (equal? path "-"))
                     stdout
                     (open-append-file path))))
            (if fd
               (begin
                  (fork-server 'log (lambda () (logger fd)))
                  (log "logger started"))
               (begin
                  (print-to stderr "Failed to start logger on " path)
                  #false))))))






