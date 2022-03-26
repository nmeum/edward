(define (parse-fail msg)
  (lambda (source index sk fk)
    (fk source index msg)))

(define (parse-bind value parser)
  (parse-map
    parser
    (lambda (x) value)))

(define (parse-as-string parser)
  (parse-map
    parser
    list->string))

(define parse-digits
  (parse-map
    (parse-token char-set:digit)
    string->number))

(define (parse-default parser def)
  (parse-map
    (parse-optional parser)
    (lambda (x)
      (if x x def))))

(define (parse-ignore-optional f)
  (parse-map
    (parse-optional f)
    (lambda (x)
      (if x x ignored-value))))

(define parse-newline
  (parse-char #\newline))

(define parse-blank
  (parse-char char-set:blank))
(define parse-blanks+
  (parse-token char-set:blank))
(define parse-blanks
  (parse-or
    parse-epsilon
    parse-blanks+))

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
  (parse-atomic
    (parse-or
      (parse-bind "" parse-newline) ;; empty line
      (parse-map
        (parse-seq
          (parse-token (lambda (x) (not (char=? x #\newline))))
          parse-newline)
        car))))

;; Feed the result of the parser ctx to a single argument procedure f.
;; The procedure must then return a new parser which is executed
;; afterwards on the same index as ctx.

(define (parse-with-context ctx f)
  (lambda (source index sk fk)
    ;; call-with-parse modifies source and needs to be called first.
    (let* ((yield (lambda (r s i fk) r))
           (value (call-with-parse ctx source index yield fk)))
      (if value
        ((f value) source index sk fk)
        (fk source index "context parser failed")))))
