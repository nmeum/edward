(foreign-declare "
  #include <unistd.h>

  int
  stdin_isatty(void)
  {
    /* TODO: Error handling */
    return isatty(STDIN_FILENO);
  }
  ")

;; XXX: This is basically the terminal? procedure from SRFI-170.
;; See: https://srfi.schemers.org/srfi-170/srfi-170.html#node_sec_3.12
(define (stdin-tty?)
  (define %stdin-tty?
    (foreign-lambda int "stdin_isatty"))

  (eqv? (%stdin-tty?) 1))
