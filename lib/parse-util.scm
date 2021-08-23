(define (parse-fail msg)
  (lambda (source index sk fk)
    (fk source index msg)))

(define (parse-as-string parser)
  (parse-map
    parser
    (lambda (lst)
      (list->string lst))))

(define parse-digits
  (parse-map
    (parse-as-string
      (parse-repeat+ (parse-char char-set:digit)))
    string->number))

(define (parse-default parser def)
  (parse-map
    (parse-optional parser)
    (lambda (x)
      (if x x def))))

(define parse-blank
  (parse-char char-set:blank))
(define parse-blanks
  (parse-repeat parse-blank))
(define parse-blanks+
  (parse-repeat+ parse-blank))

(define (parse-between lhs parser rhs)
  (parse-map
    (parse-seq lhs parser rhs)
    cadr))

(define (parse-esc f)
  (parse-map
    (parse-seq
      (parse-char #\\)
      f)
    cadr))

;; Invoke given parser and strip trailing blanks (if any).

(define (parse-strip-blanks parser)
  (parse-map
    (parse-seq
      parser
      parse-blanks)
    car))

;; Like parse-seq but skip blanks **before** each parser.

(define (parse-blanks-seq . o)
  (define (%parse-blanks-seq lst)
    (parse-seq-list
      (apply append
             (zip (make-list
                        (length lst)
                        (parse-ignore parse-blanks))
                  lst))))

  (%parse-blanks-seq o))

;; Parse a single line (excluding the terminating newline).

(define parse-line
  (parse-map
    (parse-seq
      (parse-as-string (parse-repeat+ (parse-not-char #\newline)))
      (parse-char #\newline))
    car))
