(define-library (edward)
  (import (scheme base)
          (scheme file)
          (scheme char)
          (scheme case-lambda)
          (scheme process-context)
          (scheme write)

          (srfi 1)
          (srfi 14)

          (matchable))

  (export parse call-with-parse parse-stream-end? string->parse-stream)
  (export make-addr parse-addr parse-addr-range)
  (export parse-cmd)
  (export make-text-editor editor-start)

  (include "lib/util.scm"
           "lib/parse.scm"
           "lib/parse-util.scm"
           "lib/parse-addr.scm"
           "lib/editor.scm"
           "lib/cmd.scm"))
