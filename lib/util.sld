(define-library edward.util
  (import (scheme base)
          (scheme write)
          (scheme file)
          (scheme process-context)

          (srfi 1)

          (matchable)
          (chicken port))

  (export inc dec id alist-values fprintln println empty-string?
          pad-string string->human-readable for-each-index sublist
          path-join user-home count-bytes lines->string write-file
          with-io-error-handler port->lines)

  (include "util.scm"))
