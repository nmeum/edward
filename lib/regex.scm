(define regex-ctrl (char-set #\\ #\&))

(define parse-backref
  (parse-map
    (parse-seq
      (parse-char #\\)
      parse-digits)
    (lambda (lst)
      (cons 'backref (cadr lst)))))

(define parse-matched
  (parse-map
    (parse-char #\&)
    (lambda (ch)
      (cons 'matched ch))))

(define parse-restr
  (parse-map
    (parse-as-string
      (parse-repeat+
        (parse-or
          (parse-esc* regex-ctrl)
          (parse-not-char regex-ctrl))))
    (lambda (str)
      (cons 'restr str))))

(define parse-subst
  (parse-repeat
    (parse-or
      parse-backref
      parse-matched
      parse-restr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (subexpr-filter subst)
  (filter (lambda (pair)
            (eqv? (first pair) 'backref)) subst))

(define (subexpr-count subst)
  (length (subexpr-filter subst)))

(define (subexpr->string subm n str)
  ;; regex offsets are byte, not character offset.
  ;; Thus, the string is converted to a bytevector first.
  (let* ((u8 (string->utf8 str))
         (match (submatches-get subm n))
         (start (submatch-start match))
         (end (submatch-end match)))
    (utf8->string (bytevector-copy u8 start end))))

(define (regex-replace bre subst str)
  (let* ((re   (parse-fully parse-subst subst))
         (nsub (subexpr-count re))
         (subm (make-submatches nsub)))
    (if (bre-match*? bre str subm)
      (fold (lambda (x y)
              (string-append y
                (match x
                  (('restr . s) s)
                  (('matched . _) (subexpr->string subm 0 str))
                  (('backref . n) (subexpr->string subm n str)))))
            "" re)
      str)))
