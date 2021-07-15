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
