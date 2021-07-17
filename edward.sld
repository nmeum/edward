(define-library (edward)
  (import (scheme base) (scheme file) (scheme char) (scheme case-lambda)
          (srfi 1)
          (srfi 14)
          (matchable)

          (scheme write))

  (export parse call-with-parse parse-stream-end? string->parse-stream)
  (export make-addr parse-addr parse-addr-range)
  (export parse-cmd)
  (export start-editor)

  (include "lib/util.scm"
           "lib/parse.scm"
           "lib/parse-util.scm"
           "lib/parse-addr.scm"
           "lib/editor.scm"
           "lib/cmd.scm"))
