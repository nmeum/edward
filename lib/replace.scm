(define replace-ctrl (char-set #\\ #\& #\newline))

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
          ;; special handling for '%' as it does not neccessarily
          ;; need to be escaped unless it's the only character.
          (parse-esc
            (parse-char (char-set-adjoin replace-ctrl #\%)))
          (parse-not-char replace-ctrl))))
    (lambda (str)
      (cons 'restr str))))

(define parse-replace
  (parse-repeat
    (parse-or
      parse-backref
      parse-matched
      parse-restr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (max-backref subst)
  (fold (lambda (x y)
          (if (eqv? 'backref (car x))
            (max (cdr x) y)
            y))
        0 subst))

;; TODO Handle this in ffi.scm
(define (bre-match-bv? bre bv subm)
  (bre-match*? bre (utf8->string bv) subm))

(define (submatch subm bv n)
  (let* ((match (submatches-get subm n))
         (start (submatch-start match))
         (end (submatch-end match)))
    (bytevector-copy bv start end)))

(define (regex-replace* bre subst bv nth)
  (define (apply-replacement subm bv replacement)
    (fold (lambda (x y)
            (bytevector-append y
              (match x
                (('restr . s) (string->utf8 s))
                (('matched . _) (submatch subm bv 0))
                (('backref . n) (submatch subm bv n)))))
          #u8() replacement))

  (define (%regex-replace* subm re start n)
    (let ((v (bytevector-copy bv start)))
      (if (bre-match-bv? bre v subm)
        (let* ((m (submatches-get subm 0))
               (s (submatch-start m))
               (e (submatch-end m))

               (i (+ start e)) ;; next index in bv
               (r (delay (bytevector-append
                           (bytevector-copy v 0 s)
                           (apply-replacement subm bv re)))))
          (if (eqv? n nth)
            (bytevector-append (force r) (bytevector-copy bv i))
            (bytevector-append
              (if (zero? nth) (force r) (bytevector-copy v 0 e))
              (%regex-replace* subm re i (inc n)))))
        v)))

  ;; TODO: Parse-fully should raise a text-editor error
  (let* ((re (parse-fully parse-replace subst))
         (subm (make-submatches (max-backref re))))
    (%regex-replace* subm re 0 1)))

;; Replace nth occurrence of bre in str with subst. If nth is zero all
;; occurrences are replaced.

(define (regex-replace bre subst str nth)
  ;; regexec(3p) offsets are byte, not character offsets.
  ;; Thus, the string needs to be converted to a bytevector.
  (utf8->string
    (regex-replace* bre subst (string->utf8 str) nth)))
