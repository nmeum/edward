(define-library (edward)
  (import (scheme base)
          (scheme file)
          (scheme char)
          (scheme case-lambda)
          (scheme process-context)
          (scheme write)

          (srfi 1)
          (srfi 14)

          ;; TODO: Replace with SRFI 204 when/if it reaches final status.
          (matchable)
          ;; TODO: Support different FFI backends (e.g. via cond-expand).
          (chicken foreign) (chicken gc))

  (export parse call-with-parse parse-stream-end? string->parse-stream)
  (export make-addr parse-addr parse-addr-range)
  (export parse-cmds)
  (export make-text-editor editor-start)

  (include "lib/util.scm"
           "lib/ffi.scm"
           "lib/parse.scm"
           "lib/parse-util.scm"
           "lib/parse-addr.scm"
           "lib/regex.scm"
           "lib/editor.scm"
           "lib/cmd.scm"))
