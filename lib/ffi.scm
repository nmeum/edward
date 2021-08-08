(foreign-declare "
  #include <regex.h>
  #include <stdlib.h>
  #include <unistd.h>

  int
  stdin_isatty(void)
  {
    /* TODO: Error handling */
    return isatty(STDIN_FILENO);
  }

  regex_t *
  make_bre(char *regex)
  {
    regex_t *re;

    if (!(re = malloc(sizeof(*re))))
      return NULL;
    if (regcomp(re, regex, REG_NOSUB))
      return NULL;

    return re;
  }

  int
  bre_match(regex_t *re, char *string)
  {
    int ret;

    ret = regexec(re, string, 0, NULL, 0);
    return ret != REG_NOMATCH;
  }

  void
  bre_free(regex_t *re)
  {
    regfree(re);
    free(re);
  }
  ")

;; XXX: This is basically the terminal? procedure from SRFI-170.
;; See: https://srfi.schemers.org/srfi-170/srfi-170.html#node_sec_3.12

(define (stdin-tty?)
  (define %stdin-tty?
    (foreign-lambda int "stdin_isatty"))

  (eqv? (%stdin-tty?) 1))

;; Allocate a new data structure for matching strings with the given
;; Basic Regular Expression (BRE).

(define (make-bre bre)
  (define %make-bre
    (foreign-lambda c-pointer "make_bre" nonnull-c-string))

  (let ((r (%make-bre bre)))
    (if r r (error "make-bre failed"))))

;; Check if a given string matches the given BRE.

(define (bre-match? bre str)
  (define %bre-match?
    (foreign-lambda int "bre_match" nonnull-c-pointer  nonnull-c-string))

  (eqv? (%bre-match? bre str) 1))

;; Free resources allocated for a given BRE.

(define (bre-free bre)
  (define %bre-free
    (foreign-lambda void "bre_free" nonnull-c-pointer))

  (%bre-free bre))
