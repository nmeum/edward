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
          (edward buffer))

  (export make-text-editor editor-start)

  (include "lib/ed/addr.scm"
           "lib/ed/editor.scm"
           "lib/ed/cmd.scm"))
