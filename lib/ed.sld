(define-library edward.ed
  (import (scheme base)
          (scheme write)
          (scheme file)
          (scheme case-lambda)
          (scheme process-context)

          (srfi 1)
          (srfi 14)

          (matchable)
          (posix-regex)

          (chicken process)
          (chicken process signal)
          (chicken port)

          (edward util)
          (edward parse)
          (edward replace)
          (edward buffer)
          (edward ed addr))

  (export make-text-editor editor-start)

  ;; XXX: Only exported for unit tests
  (export parse-cmd cmd-args)

  ;; XXX: Silence "indirect export of syntax binding" warnings
  (export define-edit-cmd  define-input-cmd define-print-cmd define-file-cmd)

  (include "lib/ed/cmd.scm"
           "lib/ed/editor.scm"))
