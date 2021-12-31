(define (test-parse-cmd desc expected input)
  (test desc expected
        (cdr (%test-parse parse-cmds (string-append input "\n")))))

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
      "foobar") "?foo? +23 -42 r foobar")

  (test-parse-error "unknown command" parse-cmds "rfoo"))

(test-group "write command"
  (test-parse-cmd "no arguments"
    (list
      (make-range
        (make-addr '(nth-line . 1))
        (make-addr '(last-line)))
      "") "w")

  (test-parse-cmd "custom address and offset, no whitespaces"
    (list
      (make-range
        (make-addr '(current-line))
        (make-addr '(current-line) '(10)))
      "foobar") ".,.+10w foobar")

  (test-parse-error "unknown command" parse-cmds "wfoo"))

(test-group "shell command"
  (test-parse-cmd "no replacements"
    (list (list "echo foobar")) "!echo foobar")
  (test-parse-cmd "command with replacement"
    (list (list "echo " 'current-file)) "!echo %")
  (test-parse-cmd "previous command"
    (list '(previous-command)) "!!")
  (test-parse-cmd "previous command appended"
    (list '(previous-command "foobar")) "!!foobar")
  (test-parse-cmd "previous command syntax not start"
    (list '("foobar !! barfoo")) "!foobar !! barfoo")
  (test-parse-cmd "escaped replacement"
    (list (list "echo %")) "!echo \\%")
  (test-parse-cmd "multiple replacements"
    (list (list "echo " 'current-file " " 'current-file)) "!echo % %"))

(test-group "global command"
  (test-parse-cmd "single command no newline"
    (list
      (make-range
        (make-addr '(nth-line . 1))
        (make-addr '(last-line)))
      "p\n") "1,$gp")

  (test-parse-cmd "single trailing whitespace"
    (list
      (make-range
        (make-addr '(nth-line . 1))
        (make-addr '(last-line)))
      "p \n") "1,$gp ")

  (test-parse-cmd "single command no newline"
    (list
      (make-range
        (make-addr '(nth-line . 23))
        (make-addr '(nth-line . 42)))
      "p \np\n") "23,42gp \\\np"))

(test-group "miscellaneous"
  (test-parse-cmd "parse command with trailing blanks"
    (list (make-addr '(nth-line . 2342)))
    "2342a     ")

  (test-parse-cmd "append command with suffixed printing command"
    (list (make-addr '(nth-line . 42)))
     "42an")

  (test-parse-error "unknown command" parse-cmds "Qn")
  (test-parse-error "unknown command" parse-cmds "a n"))
