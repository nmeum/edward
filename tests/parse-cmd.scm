(define (test-parse-cmd expected input)
  (test expected (cdr (%test-parse parse-cmd input))))

(test-group "append command"
  (test-parse-cmd (list (make-addr '(current-line))) "a")
  (test-parse-cmd (list (make-addr '(current-line))) "  a")
  (test-parse-cmd (list (make-addr '(nth-line . 2342))) "2342    a")
  (test-parse-cmd (list (make-addr '(last-line . ()) '(42))) "$+42 a"))
