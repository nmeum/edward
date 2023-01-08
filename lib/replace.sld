(define-library (edward.replace)
  (import (scheme base)
          (scheme lazy)

          (srfi 1)
          (srfi 14)

          (matchable)
          (posix-regex)

          (edward parse))

  (export parse-replace regex-replace)

  (include "replace.scm"))
