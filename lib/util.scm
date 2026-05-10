;;>| String Procedures
;;>
;;> Utility procedures which operate on strings.

;;> Return true if the given string `str` is the empty string.

(define (empty-string? str)
  (zero? (string-length str)))

;;> Pad given string `str` with given padding string `pad` to `length`.

(define (pad-string str pad length)
  (if (>= (string-length str) length)
    str
    (pad-string (string-append pad str) pad length)))

;;> Convert string to a human readable representation as mandated
;;> by the ed [list command][ed list].
;;>
;;> [ed list]: https://pubs.opengroup.org/onlinepubs/9799919799/utilities/ed.html#tag_20_38_13_17

(define (string->human-readable str)
  ;; Length at which lines are folded.
  (define fold-length
    (let*-values (((padding) 8)
                  ((port) (current-output-port))
                  ((_ cols) (if (terminal-port? port)
                              (terminal-size port)
                              (values 0 0))))
      (if (> cols padding)
        (- cols padding)
        72)))

  (define (byte->human-readable byte)
    (case byte
      ;; Mapping according to Table 5-1 in POSIX-1.2008.
      ((#x5C) "\\\\")
      ((#x07) "\\a")
      ((#x08) "\\b")
      ((#x0C) "\\f")
      ((#x0D) "\\r")
      ((#x09) "\\t")
      ((#x0B) "\\v")

      ;; End of each line shall be marked with a `$` character.
      ((#x0A) "$\n")
      ;; `$` character within the line should be escaped.
      ((#x24) "\\$")

      ;; Non-printable characters are represented in octal.
      (else
        (if (ascii-printable? byte)
          (string (integer->char byte))
          (string-append "\\" (pad-string (number->string byte 8) "0" 3))))))

  ;; Fold lines at fold-length and convert bytes according to procedure above.
  (let ((bv (string->utf8 str)))
    (fold (lambda (idx out)
            (let* ((byte (bytevector-u8-ref bv idx))
                   (ret (string-append out (byte->human-readable byte))))
              (if (and (not (zero? idx))
                       (zero? (modulo idx fold-length)))
                (string-append ret "\\\n")
                ret)))
          "" (iota (bytevector-length bv)))))

;;> Join a list of path elements (i.e. strings) using `/` as a path separator.

(define (path-join . elems)
  (fold-right
    (lambda (elem path)
      (if (empty-string? path)
        elem
        (string-append elem "/" path)))
    "" elems))

;;> Return amount of bytes in a string.

(define (count-bytes str)
  ;; The implementation here is taken from the internal implementation of
  ;; string->utf8. It calculates the amount of bytes required by a unicode
  ;; string without performing utf-8 validation or memory allocations.
  (fx- (##sys#size (##sys#slot str 0)) 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;>| IO Procedures
;;>
;;> Procedures which deal with input/output.

;;> If `port` is a file or socket, return the underlying file descriptor.
;;> If `port` is not backed by a file it returns a false value and throws
;;> an exception otherwise.

(define (%port->fileno port)
  ;; If the port is not backed by a POSIX file (e.g., because it was
  ;; created using open-input-string), then port->fileno raises a
  ;; type-error using the standard library's posix-error function.
  ;; The posix-error function then converts it to an (exn type) error.
  ;;
  ;; See:
  ;;   * https://code.call-cc.org/githtml/chicken-core/chicken-6/files/posix-common.scm.html#L195
  ;    * https://code.call-cc.org/githtml/chicken-core/chicken-6/files/library.scm.html#L6066
  ;;
  ;; Other exception raised by port->fileno are just re-raised then.
  (condition-case (port->fileno port)
    ((exn type) #f)))

;;> Write `lines`, i.e. a list of non-newline terminated strings to a
;;> given `port`. Returns the amount of bytes written to the port
;;> (including any newline characters).

(define (lines->port lines port)
  (fold (lambda (line num)
          (let ((line (string-append line "\n")))
            ;; TODO: Make write-string return the amount of bytes written.
            (write-string line port)
            (+ num (count-bytes line))))
        0 lines))

;;> Read from given `port` as a list of lines. Returns pair of retrieved
;;> lines and total amount of bytes read from the port (including
;;> newlines).

(define (port->lines port)
  ;; TODO: This should be provided directly by (chicken file posix).
  (define is-reg?
    ;; XXX: Technically, we expect a `mode_t` and not an `unsigned-int`
    ;; here. However, this is just a type annotation for CHICKEN itself.
    (foreign-lambda* bool ((unsigned-int mode))
      "C_return(S_ISREG(mode));"))

  ;; Slow path used if port is not backed by a regular POSIX file.
  ;;
  ;; TODO: Ideally, we don't want to never count bytes manually but
  ;; instead re-use the return value of the underlying read(2) syscall.
  ;; Unfortunately, this value is not exposed by read-lines.
  (define (count-each-line lines)
    (fold (lambda (l n)
            ;; +1 for newline stripped by read-lines.
            ;; XXX: Buggy if last line is not not terminated with \n.
            (+ 1 n (count-bytes l))) 0 lines))

  (let ((fileno (%port->fileno port))
        ;; TODO: make read-lines return the amount of bytes read.
        (lines (read-lines port)))
    (cons
      lines
      (if fileno
        (let ((stat (file-stat fileno)))
          (if (is-reg? (vector-ref stat 1))
            (vector-ref stat 5)
            (count-each-line lines)))
        (count-each-line lines)))))

;;> Read a single UTF-8 character from a `fileno`. Return a false value if
;;> end-of-file is reached. If EOF is reached within a multibyte sequence,
;;> an exception is raised. This procedure uses `file-read` internally and
;;> can thus—contrary to `read-char`—read beyond EOF.

;; TODO: Use buffering here instead of emitting ~1 syscall per character.
(define (file-read-char fileno)
  ;; Use an internal CHICKEN function to check for multibyte sequences.
  (define (bytes-needed byte)
    (##core#inline "C_utf_bytes_needed" byte))

  ;; UTF-8 multibyte sequences consists of a maximum of 4 bytes. Hence,
  ;; a bytevector of size four will suffice. We first read a single byte.
  ;; If it is a multibyte sequence, we read the remaining bytes afterward.
  (let* ((buf (make-bytevector 4))
         (ret (file-read fileno 1 buf))
         (num (cadr ret)))
    (if (zero? num)
      #f
      (let* ((last-byte (bytevector-u8-ref (car ret) (dec num)))
             (num-needed (bytes-needed last-byte)))
        (assert (and (> num-needed 0)
                     (< num-needed (bytevector-length buf))))
        (if (> num-needed 1)
          (let* ((to-read (dec num-needed))
                 (ret (file-read fileno to-read)))
            (if (eqv? (cadr ret) to-read)
              (begin
                (bytevector-copy! buf 1 (car ret))
                (string-ref (utf8->string buf) 0))
              (error "unexpected short read in multibyte sequence")))
          (integer->char last-byte))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;>| Miscellaneous
;;>
;;> Miscellaneous utility procedures.

;;> Syntactic sugar to increment a number by one.
(define (inc n) (+ n 1))
;;> Syntactic sugar to decrement a number by one.
(define (dec n) (- n 1))

;;> Identity function, always returns the given value.

(define (id x) x)

;;> Returns all values of an `alist`, discarding the keys.

(define (alist-values alist)
  (map cdr alist))

;;> Like `display` but prints multiple objects and adds a trailing newline.

(define (println . objs)
  (apply fprintln (current-output-port) objs))

;;> Like [println](#println) but allows specification of a custom output `port`.

(define (fprintln port . objs)
  (for-each (lambda (obj) (display obj port)) objs)
  (newline port))

;;> Whether the given `integer` does not represent an ASCII control character.

(define (ascii-printable? integer)
  (and (>= integer #x20) (<= integer #x7e)))

;;> Return path to home directory of current user.
;;> This procedure emits an error if the environment variable `HOME` is unset.

(define (user-home)
  (let ((home (get-environment-variable "HOME")))
    (if home
      home
      (error "environment variable 'HOME' not set"))))
