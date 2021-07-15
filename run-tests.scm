#!/usr/bin/env chibi-scheme

(import (scheme base) (chibi test) (chibi parse) (edward))

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

(test-group "Address parser"
  (include "tests/addr.scm"))
