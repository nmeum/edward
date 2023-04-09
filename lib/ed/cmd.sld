;;> Library for defining custom ed editor commands through provided
;;> macros. For each command, a parser and an executor needs to be defined.
;;> The parser is defined using edward [parser combinators][edward parse].
;;> The executor receives an [editor object][editor object] as well as the
;;> return value of the parser combinator as procedure arguments and modifies
;;> the editor state accordingly. Additionally, this library defines several
;;> utility procedures that are useful for defining [executor procedures][executor util]
;;> and ed [parser combinators][parser util].
;;>
;;> [edward parse]: edward/parse.html
;;> [editor object]: edward/ed/editor.html#section-text-editor-object
;;> [executor util]: #section-executor-utilities
;;> [parser util]: #section-parser-utilities

(define-library (edward ed cmd)
  (import (scheme base)
          (scheme file)
          (scheme lazy)

          (srfi 1)
          (srfi 14)

          (chicken process)

          (edward util)
          (edward parse)
          (edward ed addr)
          (edward ed editor))

  ;; Utility procedures to define custom editor commands.
  (export define-edit-cmd define-input-cmd define-print-cmd define-file-cmd
          parse-cmd-char parse-cmd parse-re parse-re-pair unwrap-command-list
          exec-command-list parse-filename parse-file-cmd write-to
          read-from call-when-confirmed subst-nomatch-handler
          filename-cmd? register-command exec-command-list-interactive)

  ;; Editor command executors mandated by POSIX.
  (include "cmd.scm"))
