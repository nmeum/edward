(define-library (edward.ed.editor)
  (import (scheme base)
          (scheme file)
          (scheme case-lambda)
          (scheme process-context)

          (srfi 1)

          (matchable)
          (posix-regex)

          (chicken process signal)
          (chicken port)

          (edward util)
          (edward parse)
          (edward buffer)
          (edward ed addr))

  (export make-text-editor text-editor? text-editor-prevcmd
          text-editor-modified? text-editor-filename
          text-editor-filename-set! text-editor-modified-set!
          text-editor-buffer text-editor-help? text-editor-help-set!
          text-editor-line text-editor-repl text-editor-error
          text-editor-last-cmd-set!)

  (export editor-start editor-error editor-filename editor-make-regex
          editor-exec-cmdlist editor-mark-line editor-shell-cmd
          editor-xexec editor-exec make-cmd cmd-args editor-raise
          editor-goto! editor-interactive editor-restr editor-verbose
          editor-reset! editor-get-lnum editor-get-lines editor-in-range
          editor-undo! editor-lines editor-append! editor-replace!
          editor-join! editor-remove! editor-move! editor-line-numbers
          addr->line range->lpair)

  (include "editor.scm"))
