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
    (parse-blanks-seq
      (parse-default parse-addr (make-addr '(current-line)))
      (parse-ignore (parse-char #\a)))
    ;; XXX: For some reason parse-ignore doesn't work for the last
    ;; element in a parse-seq sequence, thus we remove it using init.
    init))

(define-command ("Write Command" handle-write)
  (parse-blanks-seq
    (parse-default parse-addr-range
                   (list
                     (make-addr '(nth-line . 1))
                     #\,
                     (make-addr '(last-line))))
    (parse-ignore (parse-char #\w))
    (parse-string (parse-repeat parse-anything))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define parse-cmd
  (apply parse-or command-parsers))
