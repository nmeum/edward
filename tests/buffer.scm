(define (test-buffer name expected proc)
  (test name
        expected
        (let ((b (make-buffer)))
          (proc b)
          (buffer->list b))))

(test-group "append"
  (test-buffer "append start"
               '("foo" "bar")
               (lambda (b)
                 (buffer-append! b 0 '("foo" "bar"))))

  (test-buffer "append between"
               '("foo" "baz" "bar")
               (lambda (b)
                 (buffer-append! b 0 '("foo" "bar"))
                 (buffer-append! b 1 '("baz"))))

  (test-buffer "append at end"
               '("foo" "123")
               (lambda (b)
                 (buffer-append! b 0 '("foo"))
                 (buffer-append! b 1 '("123")))))

(test-group "remove"
  (test-buffer "remove start full"
               '()
               (lambda (b)
                 (buffer-append! b 0 '("foo" "bar"))
                 (buffer-remove! b 0 2)))

  (test-buffer "remove start partial"
               '("bar")
               (lambda (b)
                 (buffer-append! b 0 '("foo" "bar"))
                 (buffer-remove! b 0 1)))

  (test-buffer "remove between"
               '("foo" "bar")
               (lambda (b)
                 (buffer-append! b 0 '("foo" "baz" "bar"))
                 (buffer-remove! b 2 2)))

  (test-buffer "remove last"
               '("foo" "baz")
               (lambda (b)
                 (buffer-append! b 0 '("foo" "baz" "bar"))
                 (buffer-remove! b 3 3))))

(test-group "replace command"
  (test-buffer "replace single line"
               '("foo" "345" "bar")
               (lambda (b)
                 (buffer-append! b 0 '("foo" "123" "bar"))
                 (buffer-replace! b 2 2 '("345"))))

  (test-buffer "replace multiple lines"
               '("foo" "bar")
               (lambda (b)
                 (buffer-append! b 0 '("1" "2"))
                 (buffer-replace! b 1 2 '("foo" "bar"))))

  (test-buffer "replace add lines"
               '("foo" "bar" "baz")
               (lambda (b)
                 (buffer-append! b 0 '("foo" "test test"))
                 (buffer-replace! b 2 2 '("bar" "baz"))))

  (test-buffer "buffer replace everything"
               '()
               (lambda (b)
                 (buffer-append! b 0 '("foo" "bar" "baz"))
                 (buffer-replace! b 1 3 '()))))

(test-group "join command"
  (test-buffer "join entire buffer"
               '("foobar")
               (lambda (b)
                 (buffer-append! b 0 '("foo" "bar"))
                 (buffer-join! b 0 2)))

  (test-buffer "join keep last"
               '("foobar" "baz")
               (lambda (b)
                 (buffer-append! b 0 '("foo" "bar" "baz"))
                 (buffer-join! b 1 2)))

  (test-buffer "join keep first"
               '("foo" "barbaz")
               (lambda (b)
                 (buffer-append! b 0 '("foo" "bar" "baz"))
                 (buffer-join! b 2 3)))

  (test-buffer "join betwen"
               '("foo" "123" "bar")
               (lambda (b)
                  (buffer-append! b 0 '("foo" "1" "2" "3" "bar"))
                  (buffer-join! b 2 4))))

(test-group "move command"
  (test-buffer "move to end"
               '("bar" "baz" "foo")
               (lambda (b)
                 (buffer-append! b 0 '("foo" "bar" "baz"))
                 (buffer-move! b 2 3 0)))

  (test-buffer "move to start"
               '("baz" "foo" "bar")
               (lambda (b)
                 (buffer-append! b 0 '("foo" "bar" "baz"))
                 (buffer-move! b 1 2 3)))

  (test-buffer "move single to middle"
               '("foo" "123" "bar")
               (lambda (b)
                 (buffer-append! b 0 '("foo" "bar" "123"))
                 (buffer-move! b 3 3 1))))

(test-group "undo command"
  (test-buffer "undo append"
               '()
               (lambda (b)
                 (buffer-append! b 0 '("foo" "bar"))
                 (buffer-undo! b)))

  (test-buffer "undo remove"
               '("foo" "bar" "baz")
               (lambda (b)
                 (buffer-append! b 0 '("foo" "bar" "baz"))
                 (buffer-remove! b 2 3)
                 (buffer-undo! b)))

  (test-buffer "undo undo"
               '("foo" "bar")
               (lambda (b)
                 (buffer-append! b 0 '("foo" "bar"))
                 (buffer-undo! b)   ;; undo append
                 (buffer-undo! b))) ;; undo undo

  (test-buffer "undo replace"
               '("foo" "bar")
               (lambda (b)
                 (buffer-append! b 0 '("foo" "bar"))
                 (buffer-replace! b 1 2 '("first" "second"))
                 (buffer-undo! b)))

  (test-buffer "undo append last"
               '("foo" "bar")
               (lambda (b)
                 (buffer-append! b 0 '("foo" "bar"))
                 (buffer-append! b 2 '("second line"))
                 (buffer-undo! b)))

  (test-buffer "undo replace last"
               '("foo" "bar")
               (lambda (b)
                 (buffer-append! b 0 '("foo" "bar"))
                 (buffer-replace! b 2 2 '("second line"))
                 (buffer-undo! b)))

  (test-buffer "undo replace undo"
               '("test" "bar")
               (lambda (b)
                 (buffer-append! b 0 '("foo" "bar"))
                 (buffer-replace! b 1 1 '("test"))
                 (buffer-undo! b)   ;; undo replace
                 (buffer-undo! b))) ;; undo undo

  (test-buffer "undo replace nothing"
               '("foo" "bar")
               (lambda (b)
                 (buffer-append! b 0 '("foo" "bar"))
                 (buffer-replace! b 0 0 '())
                 (buffer-undo! b)))

  (test-buffer "undo join"
               '("foo" "bar")
               (lambda (b)
                 (buffer-append! b 0 '("foo" "bar"))
                 (buffer-join! b 0 2)
                 (buffer-undo! b)))

 (test-buffer "undo move"
               '("foo" "bar" "baz")
               (lambda (b)
                 (buffer-append! b 0 '("foo" "bar" "baz"))
                 (buffer-move! b 2 3 0)
                 (buffer-undo! b))))
