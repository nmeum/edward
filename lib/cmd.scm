(define-syntax define-command
  (syntax-rules ()
    ((define-command (NAME HANDLER) BODY ...)
      (define NAME
       (parse-map
         BODY ...
         (lambda (args) (list HANDLER args)))))))

(define-command (parse-append handle-append)
  (parse-map
    (parse-seq
      (parse-default parse-addr (make-addr '(current-line)))
      (parse-char #\a))
    car))

(define handle-append
  (case-lambda
    ((addr)
     (begin
       (display "appending text at ") (display addr) (newline)))))

;;;;

(define parse-command
  ;; TODO: Autogenerate from list created by define-command.
  (parse-seq
    parse-append))
