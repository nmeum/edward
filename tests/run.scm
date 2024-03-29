(import r7rs test (edward parse))

(define (%test-parse parser input)
  (define (parse-with-error parser stream)
    (call-with-parse parser stream 0
                     (lambda (r s i fk)
                       (if (parse-stream-end? s i)
                         r
                         (fk s i "incomplete parse")))
                     (lambda (s i reason) (error reason))))

  (let* ((stream (string->parse-stream input))
         (result (parse-with-error parser stream)))
    result))

(define (test-parse expected parser input)
  (test expected (%test-parse parser input)))

(define (test-parse-error desc expected parser input)
  (let ((r (call-with-current-continuation
             (lambda (k)
               (with-exception-handler
                 (lambda (e) (k e))
                 (lambda ( ) (k (%test-parse parser input))))))))
    (test desc expected
      (if (error-object? r)
        (error-object-message r))))) ;; (not (error-object? r)) → undefined

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(include-relative "parse-addr.scm")
(include-relative "parse-cmd.scm")
(include-relative "replace.scm")
(include-relative "buffer.scm")
(include-relative "util.scm")

;; Exit with non-zero exit status if some test failed.
(test-exit)
