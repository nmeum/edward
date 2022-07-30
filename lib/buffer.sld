(define-library edward.buffer
  (import (scheme base)

          (srfi 1)

          (edward util))

  (export make-buffer buffer->list buffer-length buffer-empty?
          buffer-append! buffer-remove! buffer-snapshot buffer-has-undo?
          buffer-undo!  buffer-replace! buffer-join! buffer-move!)

  (include "buffer/stack.scm"
           "buffer/buffer.scm"))
