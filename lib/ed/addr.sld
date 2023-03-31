;;> This library provides an abstraction for parsing, representing, and
;;> operating on [ed addresses][ed addresses]. Conceptually, the library
;;> distinguishes single addresses and ranges. The latter consisting of a
;;> start and end address as well as a address separator (as defined in
;;> POSIX).
;;>
;;> [ed addresses]: https://pubs.opengroup.org/onlinepubs/9699919799/utilities/ed.html#tag_20_38_13_02

(define-library (edward.ed.addr)
  (import (scheme base)
          (scheme case-lambda)

          (srfi 1)
          (srfi 14)

          (matchable)

          (edward parse))

  (export make-addr make-range range? addr->range range->addr
          parse-addrs parse-addr-with-off expand-addr address-separator?)

  (include "addr.scm"))
