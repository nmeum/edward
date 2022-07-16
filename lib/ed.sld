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
          (chicken string)

          (edward util)
          (edward parse)
          (edward replace)
          (edward buffer)
          (edward ed addr))

  ;; editor.scm
  (export make-text-editor text-editor? editor-filename
          editor-start cmd-args editor-raise editor-goto!
          editor-interactive editor-restr editor-verbose
          editor-reset! editor-get-lnum editor-get-lines
          editor-in-range editor-snapshot editor-undo!
          editor-lines editor-append! editor-replace!
          editor-join! editor-remove! editor-move!
          addr->line line-numbers)

  ;; cmd.scm
  (export define-edit-cmd define-input-cmd define-print-cmd define-file-cmd
          parse-cmd-char parse-cmd parse-re parse-re-pair unwrap-command-list
          exec-command-list parse-filename parse-file-cmd)

  (include "lib/ed/cmd.scm"
           "lib/ed/editor.scm"))
