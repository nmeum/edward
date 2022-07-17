(import (scheme base)
        (edward cli))

(cond-expand
  ((or chicken-script compiling)
   (edward-main))
  (else #t))
