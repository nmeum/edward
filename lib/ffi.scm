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

  int
  pipe_to(char *cmd, char *input)
  {
    FILE *stream;
    int r;

    if (!(stream = popen(cmd, \"w\")))
      return -1;

    if (fputs(input, stream) == EOF) {
      pclose(stream);
      return -2;
    }

    if ((r = pclose(stream)) == -1)
      return -3;

    return r;
  }

  static size_t numbytes = 0;

  size_t
  pipe_from_numbytes(void)
  {
    return numbytes;
  }

  char **
  pipe_from(char *cmd)
  {
    char **ret;
    FILE *stream;
    ssize_t n;
    size_t nlines = 0;
    char **newret;
    char *line = NULL;
    size_t llen = 0;
    static const size_t allocstep = 64;

    if (!(stream = popen(cmd, \"r\")))
      return NULL;
    if (!(ret = malloc(sizeof(char*) * allocstep)))
      goto ret0;

    numbytes = nlines = 0;
    while ((n = getline(&line, &llen, stream)) != -1) {
      if (nlines && (nlines % allocstep == 0)) {
        size_t newsiz = (nlines + allocstep) * sizeof(char*);
        if (!(newret = realloc(ret, newsiz)))
          goto ret1;
        ret = newret;
      }

      numbytes += n;
      if (line[n - 1] == '\\n')
        line[n - 1] = '\\0';
      if (!(ret[nlines] = strdup(line)))
        goto ret1;

      nlines++;
    }

    /* reshrink to actual size and leave space for terminator */
    if (!(newret = realloc(ret, ++nlines * sizeof(char*))))
      goto ret1;
    ret = newret;
    ret[nlines-1] = NULL;

    free(line);
    goto ret0;

  ret1:
    for (size_t i = 0; i < nlines; i++)
      free(ret[i]);

    free(line);
    free(ret);
    ret = NULL;
  ret0:
    pclose(stream);
    return ret;
  }

  regex_t *
  make_bre(char *regex)
  {
    regex_t *re;

    if (!(re = malloc(sizeof(*re))))
      return NULL;
    if (regcomp(re, regex, 0))
      return NULL;

    return re;
  }

  int
  bre_match(regex_t *re, char *string, size_t nmatch, regmatch_t *pmatch)
  {
    int ret;

    ret = regexec(re, string, nmatch, pmatch, 0);
    return ret != REG_NOMATCH;
  }

  void
  bre_free(regex_t *re)
  {
    regfree(re);
    free(re);
  }

  regmatch_t *
  make_submatches(size_t n)
  {
    regmatch_t *r;

    if (!(r = malloc(sizeof(*r) * n)))
      return NULL;

    return r;
  }

  void
  submatches_free(regmatch_t *pmatch)
  {
    free(pmatch);
  }

  regmatch_t *
  submatches_get(size_t nmatch, regmatch_t *pmatch, size_t idx)
  {
    if (idx >= nmatch)
      return NULL;

    return &pmatch[idx];
  }

  ssize_t
  submatch_start(regmatch_t *m)
  {
    return (ssize_t)m->rm_so;
  }

  ssize_t
  submatch_end(regmatch_t *m)
  {
    return (ssize_t)m->rm_eo;
  }
  ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; XXX: This is basically the terminal? procedure from SRFI-170.
;; See: https://srfi.schemers.org/srfi-170/srfi-170.html#node_sec_3.12

(define (stdin-tty?)
  (define %stdin-tty?
    (foreign-lambda int "stdin_isatty"))

  (eqv? (%stdin-tty?) 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Spawn the given command and pipe the given string via stdin to it.

(define (pipe-to command input)
  (define %pipe-to
    (foreign-lambda int "pipe_to" nonnull-c-string nonnull-c-string))

  (match (%pipe-to command input)
    (-1 (error "popen failed"))
    (-2 (error "fputs failed"))
    (-3 (error "pclose failed"))
    (r r)))

;; Spawn the given command and return the amount of bytes read and the
;; list of lines representing its output.

(define (pipe-from command)
  (define %pipe-from-numbytes
    (foreign-lambda size_t "pipe_from_numbytes"))
  (define %pipe-from
    (foreign-lambda c-string-list* "pipe_from" nonnull-c-string))

  (match (%pipe-from command)
    (#f (error "pipe_from failed"))
    (lst (cons lst (%pipe-from-numbytes)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Allocate a new data structure for matching strings with the given
;; Basic Regular Expression (BRE).

(define (make-bre bre)
  (define %make-bre
    (foreign-lambda c-pointer "make_bre" nonnull-c-string))

  (let ((r (%make-bre bre)))
    (if r
      (begin
        (set-finalizer! r bre-free)
        r)
      (error "make-bre failed"))))

;; Check if a given string matches the given BRE.

(define %bre-match?
  (foreign-lambda int "bre_match" nonnull-c-pointer nonnull-c-string size_t c-pointer))

(define (bre-match? bre str)
  (eqv? (%bre-match? bre str 0 #f) 1))

;; Free resources allocated for a given BRE.

(define (bre-free bre)
  (define %bre-free
    (foreign-lambda void "bre_free" nonnull-c-pointer))

  (%bre-free bre))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type Submatches
  (%make-submatches ptr count)
  submatches?
  (ptr %submatches-ptr)
  (count submatches-count))

(define (make-submatches n)
  (define %%make-submatches
    (foreign-lambda c-pointer "make_submatches" size_t))

  (let* ((%n (inc n)) ;; reserve space for zero subexpression
         (p  (%%make-submatches %n)))
    (if p
        (begin
          (set-finalizer! p submatches-free)
          (%make-submatches p %n))
      (error "make-submatches failed"))))

(define (submatches-free pointer)
  (define %submatches-free
    (foreign-lambda void "submatches_free" nonnull-c-pointer))

  (%submatches-free pointer))

(define (submatches-get subm idx)
  (define %submatches-get
    (foreign-lambda c-pointer "submatches_get" size_t nonnull-c-pointer size_t))

  (let ((r (%submatches-get (submatches-count subm) (%submatches-ptr subm) idx)))
    (if r
      r
      (error "out of bounds submatch"))))

(define (submatch-start match)
  (define %submatch-start
    (foreign-lambda ssize_t "submatch_start" nonnull-c-pointer))

  (%submatch-start match))

(define (submatch-end match)
  (define %submatch-end
    (foreign-lambda ssize_t "submatch_end" nonnull-c-pointer))

  (%submatch-end match))

;; Variant of bre-match which also tracks submatches.

(define (bre-match*? bre obj subm)
  (cond
    ((bytevector? obj)
     (bre-match*? bre (utf8->string obj) subm))
    ((string? obj)
     (eqv? (%bre-match? bre obj
                        (submatches-count subm)
                        (%submatches-ptr subm))
           1))
    (else (error "unsupported match type"))))
