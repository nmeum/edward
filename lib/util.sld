;;> This library provides various generic utility procedures.

(define-library (edward.util)
  (import (scheme base)
          (scheme write)
          (scheme file)
          (scheme process-context)

          (srfi 1)

          (chicken port)
          (chicken io))

  (export inc dec id alist-values fprintln println empty-string?
          pad-string string->human-readable path-join user-home
          count-bytes lines->port port->lines)

  (include "util.scm"))
