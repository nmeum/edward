;;> This library implementes [BRE][bre posix] regex replacements on top
;;> of the existing [posix-regex][posix-regex egg] library.
;;>
;;> [posix-regex egg]: https://wiki.call-cc.org/eggref/5/posix-regex
;;> [bre posix]: https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap09.html

(define-library (edward replace)
  (import (scheme base)
          (scheme lazy)

          (srfi 1)
          (srfi 14)

          (matchable)
          (posix-regex)

          (edward parse))

  (export parse-replace regex-replace)

  (include "replace.scm"))
