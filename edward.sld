(define-library (edward)
  (import (scheme base) (scheme file) (scheme char) (srfi 14) (scheme write))

  (export call-with-parse parse-stream-end? string->parse-stream parse-addr)

  (include "lib/parse.scm"
           "lib/addr.scm"))
