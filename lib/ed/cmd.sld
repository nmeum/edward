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

  ;; Utility procedures to define custom editor commands.
  (export define-edit-cmd define-input-cmd define-print-cmd define-file-cmd
          parse-cmd-char parse-cmd parse-re parse-re-pair unwrap-command-list
          exec-command-list parse-filename parse-file-cmd)

  ;; Editor command executors mandated by POSIX.
  (export %exec-edit %exec-quit exec-append exec-change exec-command
          exec-copy exec-delete exec-edit exec-filename exec-global
          exec-help exec-insert exec-interactive exec-join
          exec-line-number exec-list exec-mark exec-move exec-null
          exec-number exec-print exec-prompt exec-quit exec-read
          exec-subst exec-undo exec-write)

  (include "cmd.scm"))
