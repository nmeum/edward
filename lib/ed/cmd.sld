(define-library edward.ed.cmd
  (import (scheme base)
          (scheme write)
          (scheme file)
          (scheme process-context)

          (srfi 1)
          (srfi 14)

          (matchable)
          (posix-regex)

          (chicken process)
          (chicken port)
          (chicken string)

          (edward util)
          (edward parse)
          (edward replace)
          (edward buffer)
          (edward ed addr)
          (edward ed editor))

  (export parse-cmd)

  ;; XXX: Silence "indirect export of syntax binding" warnings
  (export define-edit-cmd define-input-cmd define-print-cmd define-file-cmd)

  (include "cmd.scm"))
