(define command-parsers '())
(define (register-command proc)
  (set! command-parsers (cons proc command-parsers)))

(define-syntax define-command
  (syntax-rules ()
    ((define-command (DESC HANDLER) BODY ...)
      (register-command
        (parse-map
          BODY ...
          (lambda (args) (cons HANDLER args)))))))

(define-command ("Append Command" handle-append)
  (parse-map
    (parse-seq
      (parse-default parse-addr (make-addr '(current-line)))
      (parse-char #\a))
    init))

(define-command ("Write Command" handle-write)
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
  (apply parse-or command-parsers))
