(import posix-regex (edward replace))

(define (test-re str pattern replacement . o)
  (regex-replace
    (make-regex pattern)
    (parse (parse-replace #\/) replacement)
    str
    (if (null? o) 0 (car o))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-group "backreferences"
  (test "single backreference"
        "faz" (test-re "foo" "\\([a-z]\\)oo" "\\1az"))
  (test "multiple backreferences"
        "bar foo" (test-re "foo bar" "\\([a-z]*\\) \\([a-z]*\\)" "\\2 \\1"))
  (test "unused backreferences"
        "2" (test-re "123" "\\([0-9]\\)\\([0-9]\\)\\([0-9]\\)" "\\2"))
  (test "escape subexpression"
        "bar" (test-re "\\(foo\\)" "\\\\(foo\\\\)" "bar")))

(test-group "ampersand"
  (test "append text"
        "foobar" (test-re "foo" "foo" "&bar"))
  (test "append in middle"
        "foo123bar" (test-re "foo1bar" "1" "&23"))
  (test "escape ampersand"
        "&bar" (test-re "foo" "foo" "\\&bar")))

(test-group "replacement amount"
  (test "replace first match only"
        "testbarfoo" (test-re "foobarfoo" "foo" "test" 1))
  (test "replace all matches"
        "testbartest" (test-re "foobarfoo" "foo" "test" 0))
  (test "replace second match only"
        "foobartest" (test-re "foobarfoo" "foo" "test" 2)))

(test-group "miscellaneous"
  (test "input string with multibyte character"
        "foo|bar" (test-re "fooλbar" "λ" "|"))
  (test "replacement with multibyte character"
        "fooλbar" (test-re "foo|bar" "|" "λ"))
  (test "replace with newline"
        "foo\nbar" (test-re "foo|bar" "|" "\\\n"))
  (test "non-participating submatch"
        "baz" (test-re "foo  baz" "foo \\(..*\\)* \\(..*\\)" "\\2\\1"))
  (test "empty submatch"
        "matched: " (test-re "foo <> baz"
                             "foo <\\(.*\\)> baz"
                             "matched: \\1")))
