(import posix-regex (edward replace))

(define (test-re str pattern replacement . o)
  (let-values (((result modified)
                (regex-replace
                  (make-regex pattern)
                  (parse (parse-replace #\/) replacement)
                  str
                  (if (null? o) 0 (car o)))))
    (cons result modified)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-group "backreferences"
  (test "single backreference"
        '("faz" . #t)
        (test-re "foo" "\\([a-z]\\)oo" "\\1az"))
  (test "multiple backreferences"
        '("bar foo" . #t)
        (test-re "foo bar" "\\([a-z]*\\) \\([a-z]*\\)" "\\2 \\1"))
  (test "unused backreferences"
        '("2" . #t)
        (test-re "123" "\\([0-9]\\)\\([0-9]\\)\\([0-9]\\)" "\\2"))
  (test "escape subexpression"
        '("bar" . #t)
        (test-re "\\(foo\\)" "\\\\(foo\\\\)" "bar")))

(test-group "ampersand"
  (test "append text"
        '("foobar" . #t)
        (test-re "foo" "foo" "&bar"))
  (test "append in middle"
        '("foo123bar" . #t)
        (test-re "foo1bar" "1" "&23"))
  (test "escape ampersand"
        '("&bar" . #t)
        (test-re "foo" "foo" "\\&bar")))

(test-group "replacement amount"
  (test "replace first match only"
        '("testbarfoo" . #t)
        (test-re "foobarfoo" "foo" "test" 1))
  (test "replace all matches"
        '("testbartest" . #t)
        (test-re "foobarfoo" "foo" "test" 0))
  (test "replace second match only"
        '("foobartest" . #t)
        (test-re "foobarfoo" "foo" "test" 2)))

(test-group "miscellaneous"
  (test "input string with multibyte character"
        '("foo|bar" . #t)
        (test-re "fooλbar" "λ" "|"))
  (test "replacement with multibyte character"
        '("fooλbar" . #t)
        (test-re "foo|bar" "|" "λ"))
  (test "replace with newline"
        '("foo\nbar" . #t)
        (test-re "foo|bar" "|" "\\\n"))
  (test "non-participating submatch"
        '("baz" . #t)
        (test-re "foo  baz" "foo \\(..*\\)* \\(..*\\)" "\\2\\1"))
  (test "empty submatch"
        '("matched: " . #t)
        (test-re "foo <> baz"
                 "foo <\\(.*\\)> baz"
                 "matched: \\1")))

(test-group "modified"
  (test "no match"
        '("foo" . #f)
        (test-re "foo" "bar" "test"))

  (test "replace second match"
        '(" foo foo bar " . #t)
        (test-re " foo foo foo " " foo " " bar " 2)))
