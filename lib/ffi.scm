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
