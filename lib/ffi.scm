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

;; Return home directory for current user.

(define (user-home)
  (define %user-home
    (foreign-lambda c-string "user_home"))

  (let ((home (%user-home)))
    (if home
      home
      (error "getenv failed"))))
