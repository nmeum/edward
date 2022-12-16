(import (edward ed addr))

(test-parse (addr->range '((current-line) ())) parse-addrs ".")
(test-parse (addr->range '((last-line) ()))    parse-addrs "$")

(define (test-addr-error desc expected input)
  (test-parse-error desc expected parse-addrs input))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-group "parse nth line"
  (test-parse (addr->range '((nth-line . 42) ())) parse-addrs "42")
  (test-addr-error "character in address" "unknown address format" "4x2")
  (test-addr-error "chained address without seperator" "unknown address format" "42."))

(test-group "parse mark"
  (test-parse (addr->range '((marked-line . #\x) ())) parse-addrs "'x")
  (test-addr-error "multi-character mark" "unknown address format" "'FOO")
  (test-addr-error "mark with digit" "unknown address format" "'F23"))

(test-group "parse-forward-bre"
  (test-parse (addr->range '((regex-forward . "foo") ())) parse-addrs "/foo/")
  (test-parse (addr->range '((regex-forward . "") ())) parse-addrs "//")
  (test-parse (addr->range '((regex-forward . "foo/bar") ())) parse-addrs "/foo\\/bar/")
  (test-parse (addr->range '((regex-forward . "f.*") ())) parse-addrs "/f.*")
  (test-parse (addr->range '((regex-forward . "f??") ())) parse-addrs "/f??/"))

(test-group "parse-backward-bre"
  (test-parse
    (addr->range '((regex-backward . "foo") ()))
    parse-addrs
    "?foo?")
  (test-parse
    (addr->range '((regex-backward . "") ()))
    parse-addrs
    "??")
  (test-parse
    (addr->range '((regex-backward . "foo?bar") ()))
    parse-addrs
    "?foo\\?bar?")
  (test-parse
    (addr->range '((regex-backward . "fo\\([a-z]\\)") ()))
    parse-addrs
    "?fo\\([a-z]\\)?"))

(test-group "parse-relative"
  (test-parse (addr->range '((relative . 5) ()))   parse-addrs "+5")
  (test-parse (addr->range '((relative . -42) ())) parse-addrs "-42")
  (test-parse (addr->range '((relative . 1) ()))   parse-addrs "+")
  (test-parse (addr->range '((relative . -1) ()))  parse-addrs "-"))

(test-group "parse offsets"
  (test-parse (addr->range '((nth-line . 2342) (1))) parse-addrs "2342 +1")
  (test-parse (addr->range '((relative . 5) (1 2 3))) parse-addrs "+5 1 2 3")
  (test-parse (addr->range '((marked-line . #\f) (23 42))) parse-addrs "'f 23    42")
  (test-parse (addr->range '((regex-forward . "foo") (-1 2 -3))) parse-addrs "/foo/ -1 2 -3")
  (test-parse (addr->range '((regex-backward . "bar") (+2342))) parse-addrs "?bar?+2342")
  (test-parse (addr->range '((nth-line . 23) (-5 +5))) parse-addrs "23-5+5"))

(test-group "address ranges"
  (test-parse
    (list #\,)
    parse-addrs ",")

  (test-parse
    (list
      #\,
      (make-addr '(nth-line . 2342)))
    parse-addrs ",2342")

  (test-parse
    (list
      (make-addr '(nth-line . 4223))
      #\,)
    parse-addrs "4223,")

  (test-parse
    (list #\;)
    parse-addrs ";")

  (test-parse
    (list
      #\;
      (make-addr '(nth-line . 9001)))
    parse-addrs ";9001")

  (test-parse
    (list
      (make-addr '(nth-line . 42))
      #\;)
    parse-addrs "42;")

  (test-parse
    (make-range
      (make-addr '(nth-line . 2342))
      #\;
      (make-addr '(nth-line . 4223)))
    parse-addrs "2342;4223")

  (test-parse
    (make-range
      (make-addr '(nth-line . 9000))
      #\,
      (make-addr '(nth-line . 9001)))
    parse-addrs "9000,9001")

  (test-parse
    (make-range
      (make-addr '(nth-line . 23))
      #\,
      (make-addr '(nth-line . 42)))
    parse-addrs "23    ,   42")

  (test-parse
    (make-range
      (make-addr '(nth-line . 23) '(23 42))
      #\,
      (make-addr '(nth-line . 42) '(42 23)))
    parse-addrs "23  +23 +42,  42 +42   +23"))

(test-group "multiple addresses"
  (test-parse
    (list
      (make-addr '(nth-line . 1))
      #\,
      (make-addr '(nth-line . 2))
      #\,
      (make-addr '(nth-line . 3)))
    parse-addrs "1,2,3")

  (test-parse
    (list #\, #\,)
    parse-addrs ",,")

  (test-parse
    (list #\, #\;)
    parse-addrs ",;")

  (test-parse
    (list
      (make-addr '(nth-line . 7))
      #\,
      (make-addr '(nth-line . 5))
      #\,)
    parse-addrs "7,5,"))
