(define (parse-fail msg)
  (lambda (source index sk fk)
    (fk source index msg)))

(define (parse-string parser)
  (parse-map
    parser
    (lambda (lst)
      (list->string lst))))

(define parse-digits
  (parse-map
    (parse-string
      (parse-repeat+ (parse-char char-set:digit)))
    string->number))

(define (parse-default parser def)
  (parse-map
    (parse-optional parser)
    (lambda (x)
      (if x x def))))

(define parse-blanks
  (parse-repeat (parse-char char-set:blank)))

(define (parse-between lhs parser rhs)
  (parse-map
    (parse-seq lhs parser rhs)
    cadr))

(define parse-esc
  (parse-map
    (parse-seq
      (parse-char #\\)
      parse-anything)
    cadr))

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
