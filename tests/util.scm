(import (edward util))

(test-group "sublist"
  (test "full list" '(1 2 3) (sublist '(1 2 3) 0 3))
  (test "sublist" '(2 3 4) (sublist '(1 2 3 4 5) 1 4))
  (test "single element" '(1) (sublist '(1 2 3) 0 1))
  (test-error "upper out-of-bounds" (sublist '(1 2) 0 3))
  (test-error "lower out-of-bounds" (sublist '(1 2) -1 2)))

(test-group "lines->string"
  (test "empty lines" "" (lines->string '()))
  (test "multiple lines" "foo\nbar\n" (lines->string '("foo" "bar"))))

(test-group "path-join"
  (test "empty" "" (path-join))
  (test "single" "foo" (path-join "foo"))
  (test "multiple" "foo/bar/baz" (path-join "foo" "bar" "baz")))
