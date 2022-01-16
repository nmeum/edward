(define-record-type Stack
  (%make-stack store)
  stack?
  (store stack-store stack-store-set!))

(define (make-stack)
  (%make-stack '()))

(define (stack-clear! stack)
  (stack-store-set! stack '()))

(define (stack-size stack)
  (length (stack-store stack)))

(define (stack-empty? stack)
  (zero? (stack-size stack)))

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

(define (buffer->list buffer)
  (buffer-lines buffer))

(define (buffer-length buffer)
  (length (buffer-lines buffer)))

(define (buffer-empty? buffer)
  (zero? (buffer-length buffer)))

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

;; Create a snapshot of the current editor state. The editor can be
;; reverted back to this snapshot using the buffer-undo! procedure.

(define (buffer-snapshot buffer)
  (stack-clear! (buffer-undo-stack buffer)))

;; Revert back to the last snapshot.

(define (buffer-undo! buffer)
  (define (%buffer-undo! buffer procs)
    (for-each (lambda (proc)
                (proc buffer))
              procs))

  (let* ((stk (buffer-undo-stack buffer))
         (stksiz (stack-size stk))
         (procs (stack-pops stk stksiz)))
    (%buffer-undo! buffer procs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Perform given body of commands as an atomic unit in respect to the
;; undo stack. That is, the following invocation of buffer-undo! will
;; undo all commands executed in the body at once.

(define (buffer-replace! buffer start end text)
  (let* ((sline (max (dec start) 0))
         (cap (- (buffer-length buffer) sline)))
    (buffer-remove! buffer start end)
    (buffer-append! buffer sline text)))

(define (buffer-join! buffer start end)
  (let* ((lines  (buffer-lines buffer))
         (sindex (max (dec start) 0))
         (joined (apply string-append (sublist lines sindex end))))
    (buffer-remove! buffer start end)
    (buffer-append!
      buffer
      sindex
      (list joined))))

(define (buffer-move! buffer start end dest)
  (let* ((lines (buffer-lines buffer))
         (sindex (max (dec start) 0))
         (move  (sublist lines sindex end)))
    (buffer-remove! buffer start end)
    (buffer-append!
      buffer
      (min dest (length (buffer-lines buffer)))
      move)))
