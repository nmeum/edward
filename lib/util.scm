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

;; Return sublist with start inclusive and end exclusive.

(define (sublist lst start end)
  (if (> start end)
    (error "invalid sublist specification")
    (let ((l (drop lst start)))
      (drop-right l (- (length lst) end)))))

;; Like init from Haskell, returns everything except the last list element.

(define (init lst)
  (define (%init lst)
    (if (eqv? 1 (length lst))
      '()
      (cons (car lst) (%init (cdr lst)))))

  (if (null? lst)
    (error "empty list")
    (%init lst)))
