(define-library edward.util
  (import (scheme base)
          (scheme write)
          (scheme file)

          (srfi 1)

          (matchable)
          (chicken port)
          (chicken process-context))

  (export inc dec id alist-values fprintln println empty-string?
          pad-string string->human-readable for-each-index sublist
          string-split path-join user-home count-bytes lines->string
          write-file port->lines)

  (include "util.scm"))