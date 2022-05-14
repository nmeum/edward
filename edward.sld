(define-library (edward)
  (import (scheme base)
          (scheme file)
          (scheme char)
          (scheme case-lambda)
          (scheme process-context)
          (scheme write)
          (scheme lazy)

          (edward util)
          (edward parse)
          (edward buffer)

          (srfi 1)
          (srfi 14)

          (matchable)   ;; TODO: Replace with SRFI 204 (not final yet)
          (posix-regex) ;; https://github.com/nmeum/posix-regex

          (chicken process)
          (chicken process signal)
          (chicken process-context)
          (chicken port))

  (export parse parse-fully call-with-parse parse-stream-end? string->parse-stream)
  (export make-addr make-range addr->range range->addr parse-addrs)
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

  (include "lib/parse-addr.scm"
           "lib/replace.scm"
           "lib/editor.scm"
           "lib/cmd.scm"))
