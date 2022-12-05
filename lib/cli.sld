(define-library edward.cli
  (import (scheme base)
          (scheme write)
          (scheme process-context)

          (srfi 37)

          (matchable)

          (edward ed cmd)
          (edward ed posix)
          (edward ed editor))

  (export edward-main)

  (include "cli.scm"))
