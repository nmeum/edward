(import (edward ed editor)
        (edward ed addr))

(define-bench (editor-regex-match-line)
  (let ((e (make-text-editor #f "" "" #f))
        (n 100000))
    (editor-append! e 0 (generate-list random-string n))
    (editor-append! e n '("bla bla 23foobar42 foo bar"))

    (range->lpair e
      (make-range (make-addr '(regex-forward . "[0-9][0-9]*foobar[0-9][0-9]* foo"))))))

(define-bench (editor-get-lines-partial)
  (let ((e (make-text-editor #f "" "" #f))
        (n 100000))
    (editor-append! e 0 (generate-list random-string n))
    (editor-get-lines e (cons 0 (/ n 2)))))

(define-bench (editor-get-line-number-last)
  (let ((e (make-text-editor #f "" "" #f))
        (n 100000))
    (editor-append! e 0 (generate-list random-string n))
    (let ((p (editor-get-lines e (cons n n))))
      (editor-get-lnum e (car p)))))
