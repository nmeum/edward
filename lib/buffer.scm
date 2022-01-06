(import (r7rs) (test) (srfi 1) (scheme base))

(define (dec n) (- n 1))
(define (inc n) (+ n 1))

;; TODO: Is there an SRFI which has this?
(define (sublist lst start end)
  (if (> start end)
    (error "invalid sublist specification")
    (let ((l (drop lst start)))
      (drop-right l (- (length lst) end)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type Stack
  (%make-stack store)
  stack?
  (store stack-store stack-store-set!))

(define (make-stack)
  (%make-stack '()))

(define (stack-push stack elem)
  (stack-store-set!
    stack
    (cons elem (stack-store stack))))

(define (stack-pop stack)
  (let* ((lst (stack-store stack))
         (top (car lst)))
    (stack-store-set! stack (cdr lst))
    top))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type Line-Buffer
  (%make-buffer lines undo-stack)
  line-buffer?
  (lines buffer-lines buffer-lines-set!)
  (undo-stack buffer-undo-stack))

(define (make-buffer)
  (%make-buffer '() (make-stack)))

(define (buffer-undo buffer proc)
  (stack-push (buffer-undo-stack buffer) proc))

(define (buffer-append! buffer line text)
  (let ((lines (buffer-lines buffer)))
    (buffer-lines-set!
      buffer
      (append
        (take lines line)
        text
        (drop lines line)))
    (buffer-undo buffer
      (lambda (buffer)
        (buffer-remove! buffer line (length text))))))

(define (buffer-remove! buffer line amount)
  (let ((lines (buffer-lines buffer)))
    (buffer-lines-set!
      buffer
      (append
        (sublist lines 0 (max (dec line) 0))
        (sublist lines (+ line amount) (length lines))))
    (buffer-undo buffer
      (lambda (buffer)
        (let ((sline (max (dec line) 0)))
          (buffer-append! buffer sline
                          (sublist
                            lines
                            sline
                            (+ line amount))))))))

(define (buffer-undo! buffer)
  (let* ((stk (buffer-undo-stack buffer))
         (undo-proc (stack-pop stk)))
    (undo-proc buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-buffer name expected proc)
  (test name
        expected
        (let ((b (make-buffer)))
          (proc b)
          (buffer-lines b))))

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
                 (buffer-remove! b 2 0)))

  (test-buffer "remove last"
               '("foo" "baz")
               (lambda (b)
                 (buffer-append! b 0 '("foo" "baz" "bar"))
                 (buffer-remove! b 3 0))))

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
                 (buffer-remove! b 2 0)
                 (buffer-undo! b))))

;; TODO: Add tests
;; TODO: Attempt to migrate from list to vector
;; TODO: Add more operations
;; TODO: Track current line number in undo stack for ed undo command
