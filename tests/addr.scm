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
