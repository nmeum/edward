(define-library edward.ed.posix
  (import (scheme base)
          (scheme write)
          (scheme process-context)

          (srfi 1)
          (srfi 14)

          (posix-regex)

          (chicken process)
          (chicken string)

          (edward util)
          (edward parse)
          (edward replace)
          (edward buffer)
          (edward ed cmd)
          (edward ed addr)
          (edward ed editor))

  (export %exec-edit %exec-quit exec-append exec-change exec-command
          exec-copy exec-delete exec-edit exec-filename exec-global
          exec-help exec-insert exec-interactive exec-join
          exec-line-number exec-list exec-mark exec-move exec-null
          exec-number exec-print exec-prompt exec-quit exec-read
          exec-subst exec-undo exec-write)

  (include "posix.scm"))
