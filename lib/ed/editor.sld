;;> This library implements a [text editor object][section text-editor]
;;> which tracks an internal state that can be modified through defined
;;> [editor operations][section operations]. Defined operations operate
;;> on a pair of line numbers. POSIX [ed addresses][edward ed addr] can
;;> be translated to a pair of line numbers using [range->lpair][range->lpair],
;;> please refer to the [address translation section][section addr-translate]
;;> for more information.
;;>
;;> [section text-editor]: #section-text-editor-object
;;> [section operations]: #section-editor-operations
;;> [section addr-translate]: #section-address-translation
;;> [edward ed addr]: edward.ed.addr.html
;;> [range->lpair]: #range->lpair

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
          text-editor-help? text-editor-help-set!
          text-editor-line text-editor-error
          text-editor-last-cmd-set!)

  (export editor-start editor-error editor-filename editor-make-regex
          editor-exec-cmdlist editor-mark-line editor-shell-cmd
          editor-xexec editor-exec make-cmd editor-cmd? cmd-args editor-raise
          editor-goto! editor-interactive editor-restr editor-verbose
          editor-reset! editor-get-lnum editor-get-lines editor-in-range?
          editor-undo! editor-lines editor-append! editor-replace!
          editor-join! editor-remove! editor-move! editor-line-numbers
          addr->line range->lpair editor-toggle-prompt! addrlst->lpair)

  (include "editor.scm"))
