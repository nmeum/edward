(import (edward util))

(test-group "ports->lines"
  (test "multiple"
        '(("foo" "bar" "baz") . 12)
        (let ((port (open-input-string "foo\nbar\nbaz\n")))
          (port->lines port)))

  (test "empty"
        '(() . 0)
        (let ((port (open-input-string "")))
          (port->lines port))))

(test-group "count-bytes"
  (test "ascii string" 6 (count-bytes "foobar"))
  (test "multibyte string" 2 (count-bytes "λ")))

(test-group "path-join"
  (test "empty" "" (path-join))
  (test "single" "foo" (path-join "foo"))
  (test "multiple" "foo/bar/baz" (path-join "foo" "bar" "baz")))
