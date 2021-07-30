(define (test-parse-cmd desc expected input)
  (test desc expected (cdr (%test-parse parse-cmds input))))

(test-group "append command"
  (test-parse-cmd "no arguments"
    (list (make-addr '(current-line))) "a")
  (test-parse-cmd "preceeding whitespaces"
    (list (make-addr '(current-line))) "  a")
  (test-parse-cmd "custom address without offset"
    (list (make-addr '(nth-line . 2342))) "2342    a")
  (test-parse-cmd "custom address with offset"
    (list (make-addr '(last-line . ()) '(42))) "$+42 a"))

(test-group "read command"
  (test-parse-cmd "no arguments"
    (list
      (make-addr '(last-line))
      "") "r")

  (test-parse-cmd "custom address"
    (list
      (make-addr '(nth-line . 42))
      "") "42r")

  (test-parse-cmd "custom address and file"
    (list
      (make-addr '(regex-backward . "foo") '(23 -42))
      "foobar") "?foo? +23 -42 r foobar"))

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

(test-group "miscellaneous"
  (test-parse-cmd "parse command with trailing blanks"
    (list (make-addr '(nth-line . 2342)))
    "2342a     ")

  (test-parse-cmd "append command with suffixed printing command"
    (list (make-addr '(nth-line . 42)))
     "42an")

  (test-parse-error "Unknown command" parse-cmds "Qn")
  (test-parse-error "Unknown command" parse-cmds "a n"))
