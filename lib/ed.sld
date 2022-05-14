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

  ;; XXX: Only exported for unit tests
  ;; Might split (edward ed addr) and (edward ed cmd) to address that.
  (export parse-cmd cmd-args)
  (export make-addr make-range range? addr->range range->addr
          parse-addrs parse-addr-with-off)

  (include "lib/ed/addr.scm"
           "lib/ed/editor.scm"
           "lib/ed/cmd.scm"))
