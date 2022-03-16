(define-library (edward)
  (import (scheme base)
          (scheme file)
          (scheme char)
          (scheme case-lambda)
          (scheme process-context)
          (scheme write)
          (scheme lazy)

          (srfi 1)
          (srfi 14)

          (matchable)   ;; TODO: Replace with SRFI 204 (not final yet)
          (posix-regex) ;; https://github.com/nmeum/posix-regex

          (chicken foreign)
          (chicken process)
          (chicken process signal)
          (chicken gc))

  (export parse call-with-parse parse-stream-end? string->parse-stream)
  (export make-addr make-range parse-addr parse-addr-range)
  (export parse-cmd)
  (export make-text-editor editor-start)
  (export parse-replace regex-replace)

  (export editor-cmd? cmd-symbol cmd-proc cmd-args)

  (export make-buffer buffer->list buffer-append! buffer-remove!
          buffer-undo! buffer-replace! buffer-join! buffer-move!
          buffer-snapshot)

  ;; Export these macros to avoid a "indirect export" compiler warning
  (export define-file-cmd define-print-cmd define-input-cmd
          define-edit-cmd)

  (include "lib/util.scm"
           "lib/ffi.scm"
           "lib/buffer.scm"
           "lib/parse.scm"
           "lib/parse-util.scm"
           "lib/parse-addr.scm"
           "lib/replace.scm"
           "lib/editor.scm"
           "lib/cmd.scm"))
