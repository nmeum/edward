(define-syntax define-command
  (syntax-rules ()
    ((define-command (NAME HANDLER) BODY ...)
      (define NAME
       (parse-map
         BODY ...
         (lambda (args) (cons HANDLER args)))))))

(define-command (parse-append handle-append)
  (parse-map
    (parse-seq
      (parse-default parse-addr (make-addr '(current-line)))
      (parse-char #\a))
    car))

(define-command (parse-write handle-write)
  (parse-seq
    (parse-default parse-addr-range
                   (list
                     (make-addr '(nth-line . 1))
                     #\,
                     (make-addr '(last-line))))
    (parse-ignore (parse-char #\w))
    (parse-ignore parse-blanks)
    (parse-string (parse-repeat parse-anything))))

;;;;

(define parse-cmd
  ;; TODO: Autogenerate from list created by define-command.
  (parse-or
    parse-append
    parse-write
    (parse-fail "unknown command")))
