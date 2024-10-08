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
;;> [ed list]: https://pubs.opengroup.org/onlinepubs/9699919799/utilities/ed.html#tag_20_38_13_17

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
  ;; Technically, we would have to convert the string to a bytevector here and
  ;; then count the length of that bytevector to obtain the number of bytes
  ;; and not the number of characters. However, CHICKEN 5 is not fully unicode
  ;; aware and hence string-length actually counts bytes and not characters.
  ;;
  ;; Additionally a string->utf8 conversion is very expensive especially when
  ;; loading large files using edward. Therefore, ideally, we would obtain the
  ;; amount of bytes directly through the read procedure in the future.
  (string-length str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;>| IO Procedures
;;>
;;> Procedures which deal with input/output.

;;> Write `lines`, i.e. a list of non-newline terminated strings to a
;;> given `port`. Returns the amount of bytes written to the port
;;> (including any newline characters).

(define (lines->port lines port)
  (fold (lambda (line num)
          (let ((line (string-append line "\n")))
            (write-string line port)
            (+ num (count-bytes line))))
        0 lines))

;;> Read from given `port` as a list of lines. Returns pair of retrieved
;;> lines and total amount of bytes read from the port (including
;;> newlines).

(define (port->lines port)
  (let ((lines (read-lines port)))
    (cons
      lines
      (fold (lambda (l n)
              ;; +1 for newline stripped by read-lines.
              ;; XXX: Buggy if last line is not not terminated with \n.
              (+ 1 n (count-bytes l))) 0 lines))))

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
