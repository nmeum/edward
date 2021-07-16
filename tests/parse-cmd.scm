(define (test-parse-cmd expected input)
  (test expected (cdr (%test-parse parse-cmd input))))

(test-group "append command"
  (test-parse-cmd (list (make-addr '(current-line))) "a")
  (test-parse-cmd (list (make-addr '(current-line))) "  a")
  (test-parse-cmd (list (make-addr '(nth-line . 2342))) "2342    a")
  (test-parse-cmd (list (make-addr '(last-line . ()) '(42))) "$+42 a"))

(test-group "write command"
  (test-parse-cmd (list
                    (list
                      (make-addr '(nth-line . 1))
                      #\,
                      (make-addr '(last-line)))
                    "") "w")

  (test-parse-cmd (list
                    (list
                      (make-addr '(current-line))
                      #\,
                      (make-addr '(current-line) '(10)))
                    "foobar") ".,.+10wfoobar"))
