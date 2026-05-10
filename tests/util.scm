(import (edward util)
        (only (srfi 1) iota)
        (only (chicken file posix) port->fileno))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (call-with-input-fileno path proc)
  (call-with-input-file
    path
    (lambda (port)
      (proc (port->fileno port)))))

(test-group "file-read-char"
  (test "read single ascii character"
        #\f
        (call-with-input-fileno "testdata/ascii.txt"
          (lambda (fileno)
            (file-read-char fileno))))
  (test "read end-of-file"
        #f
        (call-with-input-fileno "testdata/empty.txt"
          (lambda (fileno)
            (file-read-char fileno))))
  ;; TODO: Test reading past EOF.
  (test "read multiple characters"
        (list #\f #\o #\o #\newline #f)
        (call-with-input-fileno "testdata/ascii.txt"
          (lambda (fileno)
            (map (lambda (n)
                   (file-read-char fileno))
                   (iota 5)))))
  (test "read multibyte sequence"
        #\λ
        (call-with-input-fileno "testdata/lambda.txt"
          (lambda (fileno)
            (file-read-char fileno))))
  (test-error "invalid multibyte sequence"
              (call-with-input-fileno "testdata/invalid-utf8-multibyte.txt"
                (lambda (fileno)
                  (file-read-char fileno)))))
