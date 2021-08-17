(test-parse '((current-line) ()) parse-addr ".")
(test-parse '((last-line) ())    parse-addr "$")

(test-group "parse nth line"
  (test-parse '((nth-line . 42) ()) parse-addr "42")
  (test-parse-error "unknown address format" parse-addr "4x2")
  (test-parse-error "unknown address format" parse-addr "42."))

(test-group "parse mark"
  (test-parse '((marked-line . #\x) ()) parse-addr "'x")
  (test-parse-error "unknown address format" parse-addr "'FOO")
  (test-parse-error "unknown address format" parse-addr "'F23"))

(test-group "parse-forward-bre"
  (test-parse '((regex-forward . "foo") ()) parse-addr "/foo/")
  (test-parse '((regex-forward . "") ()) parse-addr "//")
  (test-parse '((regex-forward . "foo/bar") ()) parse-addr "/foo\\/bar/")
  (test-parse '((regex-forward . "f.*") ()) parse-addr "/f.*")
  (test-parse '((regex-forward . "f??") ()) parse-addr "/f??/"))

(test-group "parse-backward-bre"
  (test-parse '((regex-backward . "foo") ()) parse-addr "?foo?")
  (test-parse '((regex-backward . "") ()) parse-addr "??")
  (test-parse '((regex-backward . "foo?bar") ()) parse-addr "?foo\\?bar?")
  (test-parse '((regex-backward . "fo\\([a-z]\\)") ()) parse-addr "?fo\\([a-z]\\)?"))

(test-group "parse-relative"
  (test-parse '((relative . 5) ())   parse-addr "+5")
  (test-parse '((relative . -42) ()) parse-addr "-42")
  (test-parse '((relative . 1) ())   parse-addr "+")
  (test-parse '((relative . -1) ())  parse-addr "-"))

(test-group "parse offsets"
  (test-parse '((nth-line . 2342) (1)) parse-addr "2342 +1")
  (test-parse '((relative . 5) (1 2 3)) parse-addr "+5 1 2 3")
  (test-parse '((marked-line . #\f) (23 42)) parse-addr "'f 23    42")
  (test-parse '((regex-forward . "foo") (-1 2 -3)) parse-addr "/foo/ -1 2 -3")
  (test-parse '((regex-backward . "bar") (+2342)) parse-addr "?bar?+2342")
  (test-parse '((nth-line . 23) (-5 +5)) parse-addr "23-5+5"))

(test-group "address ranges"
  (test-parse
    (list
      (make-addr '(nth-line . 1))
      #\,
      (make-addr '(last-line)))
    parse-addr-range ",")

  (test-parse
    (list
      (make-addr '(nth-line . 1))
      #\,
      (make-addr '(nth-line . 2342)))
    parse-addr-range ",2342")

  (test-parse
    (list
      (make-addr '(nth-line . 4223))
      #\,
      (make-addr '(nth-line . 4223)))
    parse-addr-range "4223,")

  (test-parse
    (list
      (make-addr '(current-line))
      #\;
      (make-addr '(last-line)))
    parse-addr-range ";")

  (test-parse
    (list
      (make-addr '(current-line))
      #\;
      (make-addr '(nth-line . 9001)))
    parse-addr-range ";9001")

  (test-parse
    (list
      (make-addr '(nth-line . 42))
      #\;
      (make-addr '(nth-line . 42)))
    parse-addr-range "42;")

  (test-parse
    (list
      (make-addr '(nth-line . 2342))
      #\;
      (make-addr '(nth-line . 4223)))
    parse-addr-range "2342;4223")

  (test-parse
    (list
      (make-addr '(nth-line . 9000))
      #\,
      (make-addr '(nth-line . 9001)))
    parse-addr-range "9000,9001")

  (test-parse
    (list
      (make-addr '(nth-line . 23))
      #\,
      (make-addr '(nth-line . 42)))
    parse-addr-range "23    ,   42")

  (test-parse
    (list
      (make-addr '(nth-line . 23) '(23 42))
      #\,
      (make-addr '(nth-line . 42) '(42 23)))
    parse-addr-range "23  +23 +42,  42 +42   +23"))
