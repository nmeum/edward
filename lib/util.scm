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
      ((#x5C) "\\")
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
  (bytevector-length (string->utf8 str)))

;;> Converts list of lines to newline-separated string.

(define (lines->string buffer)
  (fold-right (lambda (x ys)
                (string-append x "\n" ys))
              "" buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;>| IO Procedures
;;>
;;> Procedures which deal with input/output.

;;> Write given data to a given file, returns `#t` if write was
;;> successful, `#f` otherwise. If the file doesn't exist it is created,
;;> otherwise it is truncated.

(define (write-file filename data)
  (guard
    (eobj
      ((file-error? eobj) #f))
    ;; TODO: If file exists behavior is unspecified
    (call-with-output-file filename
      (lambda (port)
        (write-string data port)))
    #t))

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

;;> Call `proc` for each element in `lst` starting at the `start` index.
;;> For each element, the procedure `proc` is passed both the index as
;;> well as the element value itself as a procedure parameter.

(define (for-each-index proc cont-proc lst start)
  (define (%for-each-index vector index rem)
    (unless (zero? rem)
      (proc index (vector-ref vector index))
      (%for-each-index
        vector
        (modulo (cont-proc index) (vector-length vector))
        (dec rem))))

  (unless (null? lst)
    (let* ((vec (list->vector lst))
           (len (vector-length vec)))
      (if (and (>= start 0)
               (< start len))
        (%for-each-index vec start len)
        (error "invalid start index")))))

;;> Return `sublist` with `start` inclusive and `end` exclusive.

(define (sublist lst start end)
  (if (> start end)
    (error "invalid sublist specification")
    (let ((l (drop lst start)))
      (drop-right l (- (length lst) end)))))

;;> Return path to home directory of current user.
;;> This procedure emits an error if the environment variable `HOME` is unset.

(define (user-home)
  (let ((home (get-environment-variable "HOME")))
    (if home
      home
      (error "environment variable 'HOME' not set"))))
