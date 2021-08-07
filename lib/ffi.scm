(foreign-declare "
  #include <regex.h>
  #include <unistd.h>

  int
  stdin_isatty(void)
  {
    /* TODO: Error handling */
    return isatty(STDIN_FILENO);
  }

  int
  string_matches(char *string, char *regex)
  {
    regex_t re;
    int ret;

    if (regcomp(&re, regex, REG_NOSUB))
      return -1;

    ret = regexec(&re, string, 0, NULL, 0);
    regfree(&re);
    return ret != REG_NOMATCH;
  }
  ")

;; XXX: This is basically the terminal? procedure from SRFI-170.
;; See: https://srfi.schemers.org/srfi-170/srfi-170.html#node_sec_3.12

(define (stdin-tty?)
  (define %stdin-tty?
    (foreign-lambda int "stdin_isatty"))

  (eqv? (%stdin-tty?) 1))

;; True if the given string matches the given Basic Regular Expression.
;;
;; TODO: Allow re-using underlying regex_t object instead of
;; re-compiling regex everytime.

(define (string-matches? str bre)
  (define %string-matches?
    (foreign-lambda int "string_matches"
                    nonnull-c-string nonnull-c-string))

  (match (%string-matches? str bre)
    (-1 (error "invalid regular expression"))
    (0  #f)
    (1  #t)))
