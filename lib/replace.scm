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

(define (parse-restr delim)
  (define replace-ctrl
    (char-set-adjoin (char-set #\\ #\& #\newline) delim))

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

(define (parse-replace delim)
  (parse-map
    (parse-repeat
      (parse-atomic
        (parse-or
          parse-backref
          parse-matched
          (parse-restr delim))))
    (lambda (lst)
      ;; If the replacement is empty replace matched text with an empty string.
      (if (null? lst)
        (cons '(restr . "") lst)
        lst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (submatch subm bv n)
  (if (>= n (vector-length subm))
    (string->utf8 (number->string n)) ;; XXX: Handling for invalid backreferences
    (let ((match (vector-ref subm n)))
      (if match
        (bytevector-copy bv (car match) (cdr match))
        #u8()))))

(define (regex-replace* regex subst bv nth)
  (define (apply-replacement subm bv replacement)
    (fold (lambda (x y)
            (bytevector-append y
              (match x
                (('restr . s)   (string->utf8 s))
                (('matched . _) (submatch subm bv 0))
                (('backref . n) (submatch subm bv n)))))
          #u8() replacement))

  ;; TODO: Refactor this function and make it more readable.
  ;; Also don't rely on (values …) truncation (not in R7RS).
  (define (%regex-replace* re start n)
    (let* ((v (bytevector-copy bv start))
           (subm (regex-exec regex v)))
      (if subm
        (let* ((m (vector-ref subm 0)) ;; submatch for entire regex
               (s (car m))             ;; start of submatch
               (e (cdr m))             ;; end of submatch

               (i (+ start e))         ;; next index in bv
               (r (delay (bytevector-append
                           (bytevector-copy v 0 s)
                           (apply-replacement subm v re)))))
          (values
            (if (eqv? n nth)
              (bytevector-append (force r) (bytevector-copy bv i))
              (bytevector-append
                (if (zero? nth) (force r) (bytevector-copy v 0 e))
                (%regex-replace* re i (+ n 1))))
            #t))
        (values v #f))))

  (%regex-replace* subst 0 1))

;; Replace nth occurrence of regex in str with subst. If nth is zero all
;; occurrences are replaced. Returns two results: The string after
;; performing all replacement and a boolean indicating if any
;; replacements where successfully performed.

(define (regex-replace regex subst str nth)
  ;; regexec(3p) offsets are byte, not character offsets.
  ;; Thus, the string needs to be converted to a bytevector.
  (let-values (((result modified) (regex-replace* regex subst (string->utf8 str) nth)))
    (values (utf8->string result) modified)))
