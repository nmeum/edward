(define-library edward.ed.addr
  (import (scheme base)
          (scheme case-lambda)

          (srfi 1)
          (srfi 14)

          (matchable)

          (edward parse))

  (export make-addr make-range range? addr->range range->addr
          parse-addrs parse-addr-with-off)

  (include "addr.scm"))
