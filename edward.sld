(define-library (edward)
  (import (scheme base) (srfi 14) (chibi parse))

  (export parse-addr)

  (include "lib/addr.scm"))
