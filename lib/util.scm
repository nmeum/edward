(define (parse-fail msg)
  (lambda (r s i fk)
    (fk s i msg)))

(define (parse-string parser)
  (parse-map
    parser
    (lambda (lst)
      (list->string lst))))

(define (parse-between lhs parser rhs)
  (parse-map
    (parse-seq
      (parse-char lhs)
      parser
      (parse-char rhs))
    cadr))

(define parse-esc
  (parse-map
    (parse-seq
      (parse-char #\\)
      parse-anything)
    cadr))
