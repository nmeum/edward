(define-library (edward)
  (import (scheme base) (scheme file) (scheme char) (scheme case-lambda)
          (srfi 14)
          (matchable)

          (scheme write))

  (export parse call-with-parse parse-stream-end? string->parse-stream)
  (export make-addr parse-addr parse-addr-range)
  (export parse-cmd)
  (export make-text-editor)

  (include "lib/editor.scm"
           "lib/parse.scm"
           "lib/parse-util.scm"
           "lib/parse-addr.scm"
           "lib/parse-cmd.scm"))
