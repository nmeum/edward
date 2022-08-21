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
  (%make-buffer lines undo? undo-stack)
  line-buffer?
  (lines buffer-lines buffer-lines-set!)
  (undo? buffer-undo? buffer-undo-set!)
  (undo-stack buffer-undo-stack buffer-undo-stack-set!))

(define (make-buffer)
  (%make-buffer '() #f (make-stack)))

(define (buffer->list buffer)
  (buffer-lines buffer))

(define (buffer-length buffer)
  (length (buffer-lines buffer)))

(define (buffer-empty? buffer)
  (zero? (buffer-length buffer)))

(define (buffer-register-undo buffer proc)
  (when (buffer-undo? buffer)
    (stack-push (buffer-undo-stack buffer) proc)))

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

;; Execute the given thunk and make all buffer operations performed
;; in thunk undoable via the buffer-undo! procedure.

(define (buffer-with-undo buffer thunk)
    (stack-clear! (buffer-undo-stack buffer)) ;; no multi-level undo
    (buffer-undo-set! buffer #t)

    (guard
      (eobj
        (else
          (buffer-undo-set! buffer #f)
          (raise eobj)))
      (let ((r (thunk)))
        (buffer-undo-set! buffer #f)
        r)))

;; Predicate to check if undo stack is empty, returns false if it is empty.

(define (buffer-has-undo? buffer)
  (not (stack-empty? (buffer-undo-stack buffer))))

;; Revert last operation tracked by buffer-with-undo. The revert is
;; itself always reversible via buffer-undo!.

(define (buffer-undo! buffer)
  (define (%buffer-undo! buffer procs)
    (buffer-with-undo buffer
      (lambda ()
        (for-each (lambda (proc)
                    (proc buffer))
                  procs))))

  (let* ((stk (buffer-undo-stack buffer))
         (stksiz (stack-size stk)))
    (unless (zero? stksiz)
      (let ((procs (stack-pops stk stksiz)))
        (%buffer-undo! buffer procs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  ;; Assumption: dest is always outside [start, end].
  (let* ((lines (buffer-lines buffer))
         (sindex (max (dec start) 0))
         (move  (sublist lines sindex end))

         (remove! (lambda () (buffer-remove! buffer start end)))
         (append! (lambda () (buffer-append! buffer dest move))))
    (if (> dest start)
      (begin (append!) (remove!))
      (begin (remove!) (append!)))))
