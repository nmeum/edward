;;>| Buffer Interface
;;>
;;> Procedures for creating new buffers and related accessors.

(define-record-type Line-Buffer
  (%make-buffer lines undo? undo-stack)
  line-buffer?
  (lines buffer-lines buffer-lines-set!)
  (undo? buffer-undo? buffer-undo-set!)
  (undo-stack buffer-undo-stack buffer-undo-stack-set!))

;;> Create a new, initially empty, line buffer.

(define (make-buffer)
  (%make-buffer #() #f (make-stack)))

;;> Convert the line buffer to a list of lines.

(define (buffer->list buffer)
  (vector->list (buffer-lines buffer)))

;;> Length of the buffer, i.e. amount of lines currently stored in it.

(define (buffer-length buffer)
  (vector-length (buffer-lines buffer)))

;;> Predicate which returns true if the buffer is empty.

(define (buffer-empty? buffer)
  (zero? (buffer-length buffer)))

(define (buffer-register-undo buffer proc)
  (when (buffer-undo? buffer)
    (stack-push (buffer-undo-stack buffer) proc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;>| Undo Stack
;;>
;;> Procedures for managing the undo stack of the line buffer.
;;> The undo stack does not support multilevel undo.
;;> That is, the last undo can itself be undone using [buffer-undo!](#buffer-undo!).

;;> Execute the given `thunk` and make all `buffer` operations performed
;;> in thunk undoable.

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

;;> Predicate to check if the undo stack is empty, returns false if it is.

(define (buffer-has-undo? buffer)
  (not (stack-empty? (buffer-undo-stack buffer))))

;;> Revert last operation tracked by [buffer-with-undo](#buffer-with-undo).
;;> The undo is itself reversible via [buffer-undo!](#buffer-undo!).

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

;;>| Buffer Operations
;;>
;;> Procedures which modify the buffer content.
;;> All operations can be undone using [buffer-undo!](#buffer-undo!).

;;> Append the given `text` to the `buffer` after the given `line` number.
;;> The special line number 0 can be used here to add lines to the
;;> beginning of the buffer.

(define (buffer-append! buffer line text)
  (let ((lines (buffer-lines buffer))
        ;; TODO: Require text to be a vector in the future.
        (invec (if (vector? text)
                 text
                 (list->vector text)))
        (inlen (if (vector? text)
                 (vector-length text)
                 (length text))))
    (buffer-lines-set!
      buffer
      (vector-append
        (subvector lines 0 line)
        invec
        (subvector lines line)))
    (buffer-register-undo buffer
      (lambda (buffer)
        (buffer-remove! buffer (inc line) (+ line inlen))))))

;;> Removes all lines within the `buffer` at the given inclusive range
;;> range between `start` and `end`.

(define (buffer-remove! buffer start end)
  (let* ((lines (buffer-lines buffer))
         (sline (max (dec start) 0)))
    (buffer-lines-set!
      buffer
      (vector-append
        (subvector lines 0 sline)
        (subvector lines end)))
    (buffer-register-undo buffer
      (lambda (buffer)
        ;; Will add an undo procedure to the stack, thus making
        ;; the undo of the remove operation itself reversible.
        (buffer-append! buffer sline
                        (subvector
                          lines
                          sline
                          end))))))

;; The following operations are all implemented in terms of
;; buffer-append! and buffer-undo! and are therefore reversible.

;;> Replace lines in the inclusive range between `start` and `end`
;;> with the data given by `text` which must be a list of lines
;;> (i.e. strings).

(define (buffer-replace! buffer start end text)
  (let* ((sline (max (dec start) 0))
         (cap (- (buffer-length buffer) sline)))
    (buffer-remove! buffer start end)
    (buffer-append! buffer sline text)))

;;> Join lines in the inclusive range between `start` and `end`
;;> into a single line by removing all newline characters within
;;> the specified range.

(define (buffer-join! buffer start end)
  (let* ((lines  (buffer-lines buffer))
         (sindex (max (dec start) 0))
         (joined (apply string-append
                        (vector->list (subvector lines sindex end)))))
    (buffer-remove! buffer start end)
    (buffer-append!
      buffer
      sindex
      (list joined))))

;;> Move lines in the inclusive range between `start` and `end`
;;> to the destination line number `dest`. The destination *must*
;;> always be outside the specified inclusive range.

(define (buffer-move! buffer start end dest)
  ;; Assumption: dest is always outside [start, end].
  (let* ((lines (buffer-lines buffer))
         (sindex (max (dec start) 0))
         (move (subvector lines sindex end))

         (remove! (lambda () (buffer-remove! buffer start end)))
         (append! (lambda () (buffer-append! buffer dest move))))
    (if (> dest start)
      (begin (append!) (remove!))
      (begin (remove!) (append!)))))
