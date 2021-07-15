(import r7rs test (edward))

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

(define (test-parse-error expected parser input)
  (let ((r (call-with-current-continuation
             (lambda (k)
               (with-exception-handler
                 (lambda (e) (k e))
                 (lambda ( ) (k (%test-parse parser input))))))))
    (test expected
      (if (error-object? r)
        (error-object-message r))))) ;; (not (error-object? r)) → undefined

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-parse 'current-line  parse-addr ".")
(test-parse 'last-line     parse-addr "$")

(test-group "parse nth line"
  (test-parse '(nth-line . 42) parse-addr "42")
  (test-parse-error "unknown address format" parse-addr "4x2")
  (test-parse-error "unknown address format" parse-addr "42."))

(test-group "parse mark"
  (test-parse '(marked-line . "foo") parse-addr "'foo")
  (test-parse-error "unknown address format" parse-addr "'FOO")
  (test-parse-error "unknown address format" parse-addr "'F23"))

(test-group "parse-bre"
  (test-parse "foo" parse-addr "/foo/")
  (test-parse "" parse-addr "//")
  (test-parse "foo/bar" parse-addr "/foo\\/bar/"))
