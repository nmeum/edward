(define (test-parse-cmd desc expected input)
  (test desc expected (cdr (%test-parse parse-cmd input))))

(test-group "append command"
  (test-parse-cmd "no arguments"
    (list (make-addr '(current-line))) "a")
  (test-parse-cmd "preceeding whitespaces"
    (list (make-addr '(current-line))) "  a")
  (test-parse-cmd "custom address without offset"
    (list (make-addr '(nth-line . 2342))) "2342    a")
  (test-parse-cmd "custom address with offset"
    (list (make-addr '(last-line . ()) '(42))) "$+42 a"))

(test-group "write command"
  (test-parse-cmd "no arguments"
    (list
      (list
        (make-addr '(nth-line . 1))
        #\,
        (make-addr '(last-line)))
      "") "w")

  (test-parse-cmd "custom address and offset, no whitespaces"
    (list
      (list
        (make-addr '(current-line))
        #\,
        (make-addr '(current-line) '(10)))
      "foobar") ".,.+10wfoobar"))
