(define (inc n) (+ n 1))
(define (dec n) (- n 1))

;; Return a procedure executing proc and then returing ret.

(define (with-ret proc ret)
  (lambda (arg . args)
    (apply proc arg args)
    ret))

;; Like display but prints multiple objects and adds trailing newline.

(define (fprintln port . objs)
  (for-each (lambda (obj) (display obj port)) objs)
  (newline port))

(define (println . objs)
  (apply fprintln (current-output-port) objs))

;; Return true if the given string is the empty string.

(define (empty-string? str)
  (zero? (string-length str)))

;; Fold for bytevector, starts at most significant byte.

(define (bytevector-fold proc seed bv)
  (define (%bytevector-fold n)
    (if (zero? n)
      seed
      (let ((idx (dec n)))
        (proc (bytevector-u8-ref bv idx)
              (%bytevector-fold idx)))))

  (let ((len (bytevector-length bv)))
    (if (zero? len)
      seed
      (%bytevector-fold len))))

;; Pad string with given string to given length.

(define (pad-string str pad length)
  (if (>= (string-length str) length)
    str
    (pad-string (string-append pad str) pad length)))

;; Whether the given integer does not represent an ASCII control character.

(define (ascii-printable? int)
  (and (>= int #x20) (<= int #x7e)))

;; Convert string to human readable representation as mandated by the
;; POSIX.1-2008 ed(1) list command.

(define (string->human-readable str)
  (define (byte->human-readable byte)
    (match byte
      ;; Mapping according to Table 5-1 in POSIX-1.217.
      (#x5C "\\")
      (#x07 "\\a")
      (#x08 "\\b")
      (#x0C "\\f")
      (#x0D "\\r")
      (#x09 "\\t")
      (#x0B "\\v")

      ;; End of each line shall be marked with a `$` character.
      (#x0A "$\n")
      ;; `$` character within the line should be escaped.
      (#x24 "\\$")

      ;; Non-printable characters are represented in octal.
      (_
        (if (ascii-printable? byte)
          (string (integer->char byte))
          (string-append "\\" (pad-string (number->string byte 8) "0" 3))))))

  (bytevector-fold
    (lambda (byte out)
      (string-append out (byte->human-readable byte)))
    "" (string->utf8 str)))

;; Call proc for each element in lst starting at the start index.

(define (%for-each-index proc cont-proc lst start)
  (define (%%for-each-index vector index rem)
    (unless (zero? rem)
      (proc index (vector-ref vector index))
      (%%for-each-index
        vector
        (modulo (cont-proc index) (vector-length vector))
        (dec rem))))

  (unless (null? lst)
    (let* ((vec (list->vector lst))
           (len (vector-length vec)))
      (if (and (>= start 0)
               (< start len))
        (%%for-each-index vec start len)
        (error "invalid start index")))))

(define (for-each-index proc lst start)
  (%for-each-index proc inc lst start))

(define (for-each-index-right proc lst start)
  (%for-each-index proc dec lst start))

;; Return sublist with start inclusive and end exclusive.

(define (sublist lst start end)
  (if (> start end)
    (error "invalid sublist specification")
    (let ((l (drop lst start)))
      (drop-right l (- (length lst) end)))))

;; Count amount of newlines in given string.

(define (count-newlines str)
  (count
    (lambda (x) (eqv? x #\newline))
    (string->list str)))