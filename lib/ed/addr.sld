;;> This library provides an abstraction for parsing, representing, and
;;> operating on [ed addresses][ed addresses]. Conceptually, the library
;;> distinguishes single addresses and ranges. The latter consisting of a
;;> start and end address as well as a address separator (as defined in
;;> POSIX). The [parse-addrs][parse-addrs] procedure returns a list of
;;> address ranges.  The editor implementation is capable of converting
;;> this list to a pair of line numbers using the [addrlst->lpair][addrlst->lpair]
;;> procedure. Command implementations expecting a range address receive
;;> this pair, commands which only expect a single address only receive
;;> the last element of the pair as an argument.
;;>
;;> [parse-addrs]: #parse-addrs
;;> [addrlst->lpair]: edward/ed/editor.html#addrlst->lpair
;;> [ed addresses]: https://pubs.opengroup.org/onlinepubs/9699919799/utilities/ed.html#tag_20_38_13_02

(define-library (edward ed addr)
  (import (scheme base)
          (scheme case-lambda)

          (srfi 1)
          (srfi 14)

          (matchable)

          (edward parse))

  (export make-addr make-range range? addr->range range->addr
          parse-addrs parse-addr-with-off expand-addr address-separator?)

  (include "addr.scm"))
