(import (edward util))

(test-group "sublist"
  (test "full list" '(1 2 3) (sublist '(1 2 3) 0 3))
  (test "sublist" '(2 3 4) (sublist '(1 2 3 4 5) 1 4))
  (test "single element" '(1) (sublist '(1 2 3) 0 1))
  (test-error "upper out-of-bounds" (sublist '(1 2) 0 3))
  (test-error "lower out-of-bounds" (sublist '(1 2) -1 2)))

(test-group "lines->string"
  (test "empty lines" "" (lines->string '()))
  (test "sinlge line" "foo\n" (lines->string '("foo")))
  (test "multiple lines" "foo\nbar\n" (lines->string '("foo" "bar"))))

(test-group "for-each-index"
  (test "increment index from start"
        '((0 . "foo") (1 . "bar") (2 . "baz"))
        (let ((ret '()))
          (for-each-index
            (lambda (idx elem)
              (set! ret (append ret (list (cons idx elem)))))
            inc #("foo" "bar" "baz") 0)
          ret))

  (test "decrement index from middle"
        '((1 . "bar") (0 . "foo") (2 . "baz"))
        (let ((ret '()))
          (for-each-index
            (lambda (idx elem)
              (set! ret (append ret (list (cons idx elem)))))
            dec #("foo" "bar" "baz") 1)
          ret))

  (test "zero list"
        '()
        (let ((ret '()))
          (for-each-index
            (lambda (idx elem)
              (set! ret idx))
            inc #() 0)
          ret))

  (test-error "invalid start index"
              (for-each-index
                (lambda (idx elem) elem)
                inc #(1 2 3) 3)))

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
