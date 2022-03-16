(foreign-declare "
  #include <stdio.h>
  #include <regex.h>
  #include <stdlib.h>
  #include <unistd.h>
  #include <stddef.h>
  #include <string.h>

  int
  stdin_isatty(void)
  {
    /* TODO: Error handling */
    return isatty(STDIN_FILENO);
  }

  char*
  user_home(void)
  {
    static char *home;

    if (!home)
      home = getenv(\"HOME\"); /* may be NULL */
    return home;
  }
")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; XXX: This is basically the terminal? procedure from SRFI-170.
;; See: https://srfi.schemers.org/srfi-170/srfi-170.html#node_sec_3.12

(define (stdin-tty?)
  (define %stdin-tty?
    (foreign-lambda int "stdin_isatty"))

  (eqv? (%stdin-tty?) 1))

;; Return home directory for current user.

(define (user-home)
  (define %user-home
    (foreign-lambda c-string "user_home"))

  (let ((home (%user-home)))
    (if home
      home
      (error "getenv failed"))))
