;;> Library implementing the edward command line interface.
;;> Most importantly, this library is responsible for parsing
;;> the command line options mandated by [POSIX][ed options]
;;> and afterward starts the read-eval-print loop of the editor.
;;>
;;> [ed options]: https://pubs.opengroup.org/onlinepubs/9699919799/utilities/ed.html#tag_20_38_04

(define-library (edward.cli)
  (import (scheme base)
          (scheme write)
          (scheme process-context)

          (srfi 37)

          (edward ed cmd)
          (edward ed posix)
          (edward ed editor))

  (export edward-main)

  (include "cli.scm"))
