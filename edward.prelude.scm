(import (scheme base)
        (scheme file)
        (scheme char)
        (scheme case-lambda)
        (scheme process-context)
        (scheme write)
        (scheme lazy)

        (srfi 1)
        (srfi 14)
        (srfi 37)

        ;; TODO: Replace with SRFI 204 when/if it reaches final status.
        (matchable)
        ;; TODO: Support different FFI backends (e.g. via cond-expand).
        (chicken foreign) (chicken gc))

(include "src/util.scm"
         "src/ffi.scm"
         "src/parse.scm"
         "src/parse-util.scm"
         "src/parse-addr.scm"
         "src/replace.scm"
         "src/editor.scm"
         "src/cmd.scm")
