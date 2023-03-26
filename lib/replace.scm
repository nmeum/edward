;;>| Replacement Parser
;;>
;;> Edward [parser combinators][edward parse] for parsing replacement strings.
;;>
;;> [edward parse]: edward.parse.html

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

;;> Parse a replacement string within text enclosed with the delimiter
;;> `delim`. While the combinator does not parse the enclosed character
;;> it ensure that this `delim` character is escaped (using a `\`)
;;> within the replacement.
;;>
;;> Refer to the documentation of the [ed substitute][ed substitute]
;;> command for more information on special character support within
;;> the replacement. All of these special characters can also be
;;> escaped.
;;>
;;> [ed substitute]: https://pubs.opengroup.org/onlinepubs/9699919799/utilities/ed.html#tag_20_38_13_25

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

;;>| Replacement Procedures
;;>
;;> Procedures for performing replacements using a parsed replacement string.

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

;;> Replace `nth` occurrence of `regex` in `str` with `subst`. If `nth`
;;> is zero all occurrences are replaced. Returns two results: The string
;;> after performing all replacement and a boolean indicating if any
;;> replacements where successfully performed.  The `regex` must be
;;> created using [make-regex][make-regex] while the replacement string
;;> `subst` must be parsed using [parse-replace][parse-replace].
;;>
;;> [make-regex]: https://wiki.call-cc.org/eggref/5/posix-regex#make-regex
;;> [parse-replace]: #parse-replace

(define (regex-replace regex subst str nth)
  ;; regexec(3p) offsets are byte, not character offsets.
  ;; Thus, the string needs to be converted to a bytevector.
  (let-values (((result modified) (regex-replace* regex subst (string->utf8 str) nth)))
    (values (utf8->string result) modified)))
