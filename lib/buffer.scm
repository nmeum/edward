(import (r7rs) (test) (srfi 1) (scheme base))

(define (fprintln port . objs)
  (for-each (lambda (obj) (display obj port)) objs)
  (newline port))

(define (println . objs)
  (apply fprintln (current-output-port) objs))

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

(define (stack-reverse! stack)
  (stack-store-set!
    stack
    (reverse (stack-store stack))))

(define (stack-size stack)
  (length (stack-store stack)))

(define (stack-push stack elem)
  (stack-store-set!
    stack
    (cons elem (stack-store stack))))

(define (stack-pop stack)
  (let* ((lst (stack-store stack))
         (top (car lst)))
    (stack-store-set! stack (cdr lst))
    top))

(define (stack-pops stack amount)
  (if (<= amount 1)
    (list (stack-pop stack))
    (cons
      (stack-pop stack)
      (stack-pops stack (dec amount)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Line-Buffer implements a text buffer for line-based data with undo
;; support. Operations on the buffer address lines, the first line
;; starts at index 1. The special index 0 can be used with the append
;; command to insert text before the first line. For other commands,
;; index 0 is equivalent to index 1. Targeting a line outside the
;; current buffer bounds causes an error to be raised.
;;
;; All provided procedures for modifying the data stored in the buffer
;; are implemented on top of the primitive append and remove procedure.

(define-record-type Line-Buffer
  (%make-buffer lines undo-stack)
  line-buffer?
  (lines buffer-lines buffer-lines-set!)
  (undo-stack buffer-undo-stack))

(define (make-buffer)
  (%make-buffer '() (make-stack)))

(define (buffer-length buffer)
  (length (buffer-lines buffer)))

(define (buffer-register-undo buffer proc)
  (stack-push (buffer-undo-stack buffer) proc))

(define (buffer-append! buffer line text)
  (let ((lines (buffer-lines buffer)))
    (buffer-lines-set!
      buffer
      (append
        (take lines line)
        text
        (drop lines line)))
    (buffer-register-undo buffer
      (lambda (buffer)
        ;; Will add an undo procedure to the stack, thus making
        ;; the undo of the append operation itself reversible.
        (buffer-remove! buffer (inc line) (+ line (length text)))))))

;; Removes all lines within the given inclusive range (reversible).

(define (buffer-remove! buffer start end)
  (let* ((lines (buffer-lines buffer))
         (sline (max (dec start) 0)))
    (buffer-lines-set!
      buffer
      (append
        (sublist lines 0 sline)
        (sublist lines end (length lines))))
    (buffer-register-undo buffer
      (lambda (buffer)
        ;; Will add an undo procedure to the stack, thus making
        ;; the undo of the remove operation itself reversible.
        (buffer-append! buffer sline
                        (sublist
                          lines
                          sline
                          end))))))

;; Undo last operation on the buffer (itself reversible).

(define (buffer-undo! buffer)
   (let* ((stk (buffer-undo-stack buffer))
          (undo-proc (stack-pop stk)))
     (undo-proc buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Perform given body of commands as an atomic unit in respect to the
;; undo stack. That is, the following invocation of buffer-undo! will
;; undo all commands executed in the body at once.

(define-syntax with-atomic-undo
  (syntax-rules ()
    ((with-atomic-undo BUF BODY ...)
     (let* ((stack (buffer-undo-stack BUF))
            (oldsiz (stack-size stack)))

       BODY ...

       ;; Combine undo procedures of operations executed in BODY
       ;; to a single undo procedure and pop them from the stack.
       (let* ((newsiz (stack-size stack))
              (diff (- newsiz oldsiz))
              (procs (stack-pops stack diff)))
         (buffer-register-undo BUF
           (lambda (buffer)
             (for-each (lambda (proc)
                         (proc buffer))
                       procs)

             ;; Executed undo procedures add new procedures to the undo
             ;; stack to undo them, these need to be combined into a
             ;; single procedure again.
             (let ((procs (stack-pops stack diff)))
               (buffer-register-undo BUF
                (lambda (buffer)
                  (for-each (lambda (proc)
                              (proc buffer))
                            procs)))))))))))

(define (buffer-replace! buffer line text)
  (let* ((sline (max (dec line) 0))
         (cap (- (buffer-length buffer) sline)))
    (with-atomic-undo buffer
      (buffer-remove! buffer line (+ sline (min cap (length text))))
      (buffer-append! buffer sline text))))

(define (buffer-join! buffer start end)
  (let* ((lines  (buffer-lines buffer))
         (sindex (max (dec start) 0))
         (joined (apply string-append (sublist lines sindex end))))
  (with-atomic-undo buffer
    (buffer-remove! buffer start end)
    (buffer-append!
      buffer
      sindex
      (list joined)))))

(define (buffer-move! buffer start end dest)
  (let* ((lines (buffer-lines buffer))
         (sindex (max (dec start) 0))
         (move  (sublist lines sindex end)))
    (with-atomic-undo buffer
      (buffer-remove! buffer start end)
      (buffer-append!
        buffer
        (min dest (length (buffer-lines buffer)))
        move))))

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
                 (buffer-replace! b 2 '("345"))))

  (test-buffer "replace multiple lines"
               '("foo" "bar")
               (lambda (b)
                 (buffer-append! b 0 '("1" "2"))
                 (buffer-replace! b 0 '("foo" "bar"))))

  (test-buffer "replace add lines"
               '("foo" "bar" "baz")
               (lambda (b)
                 (buffer-append! b 0 '("foo" "test test"))
                 (buffer-replace! b 2 '("bar" "baz")))))

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
                 (buffer-replace! b 1 '("first" "second"))
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
                 (buffer-replace! b 2 '("second line"))
                 (buffer-undo! b)))

  (test-buffer "undo replace undo"
               '("test" "bar")
               (lambda (b)
                 (buffer-append! b 0 '("foo" "bar"))
                 (buffer-replace! b 1 '("test"))
                 (buffer-undo! b)   ;; undo replace
                 (buffer-undo! b))) ;; undo undo

  (test-buffer "undo replace nothing"
               '("foo" "bar")
               (lambda (b)
                 (buffer-append! b 0 '("foo" "bar"))
                 (buffer-replace! b 0 '())
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

;; TODO: Clear stack
;; TODO: Add more operations
;; TODO: Track current line number in undo stack for ed undo command
