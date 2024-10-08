;;> A text buffer for line-based data with [undo support][undo section].
;;> Operations on the buffer address lines, the first line starts at
;;> index 1. The special index 0 can be used with the [append command][append command]
;;> to insert text before the first line. For other commands, index 0 is
;;> equivalent to index 1. Targeting a line outside the current buffer
;;> bounds causes an error to be raised.
;;>
;;> [undo section]: #section-undo-stack
;;> [append command]: #buffer-append!

(define-library (edward.buffer)
  (import (scheme base)
          (scheme case-lambda)

          (srfi 1)

          (only (chicken base) vector-resize)

          (edward util))

  (export make-buffer buffer->list buffer-length buffer-empty? buffer-ref
          buffer-append! buffer-remove! buffer-with-undo buffer-has-undo?
          buffer-undo! buffer-replace! buffer-join! buffer-move!)

  (include "buffer/stack.scm"
           "buffer/srfi214-minimal.scm"
           "buffer/buffer.scm"))
