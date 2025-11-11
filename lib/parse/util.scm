;;>| Utility Parsing Combinators
;;>
;;> Additional high-level combinators implemented using the aforementioned
;;> [basic combinators][basic section]. The provided high-level combinators,
;;> return more useful parser error messages then the basic ones.
;;>
;;> [basic section]: #section-basic-parsing-combinators

;;> Parser which always fails with the given error message.

(define (parse-fail msg)
  (lambda (source index sk fk)
    (fk source index msg)))

;;> Bind a constant `value` to a given `parser`.
;;> That is, always return this value if the parser succeeds.

(define (parse-bind value parser)
  (parse-map
    parser
    (lambda (x) value)))

;;> Run a parser `f`, which *must* return a list, and convert
;;> its return value to a string using the `list->string` procedure.

(define (parse-as-string parser)
  (parse-map
    parser
    list->string))

;;> Parse one or more digits (i.e. `0-9`), interpret them as a
;;> decimal number, and return this number.

(define parse-digits
  (parse-with-failure-reason
    (parse-map
      (parse-token char-set:digit)
      string->number)
    "expected digits"))

;;> Parse an ASCII lowercase character (i.e. `a-z`).

(define parse-lowercase
  (parse-with-failure-reason
    (parse-char char-set:lower-case)
    "expected lowercase character"))

;;> Attempt parsing using the given parser `f`, if it fails return a default value `def`.

(define (parse-default f def)
  (parse-map
    (parse-optional f)
    (lambda (x)
      (or x def))))

;;> Parse a newline character.

(define parse-newline
  (parse-with-failure-reason
    (parse-char #\newline)
    "expected newline"))

;;> Parse a single blank character (i.e. horizontal whitespace).

(define parse-blank
  (parse-with-failure-reason
    (parse-char char-set:blank)
    "expected whitespace"))

;;> Parse one or more blank characters.

(define parse-blanks+
  (parse-with-failure-reason
    (parse-token char-set:blank)
    "expected whitespaces"))

;;> Parse zero or more blank characters.

(define parse-blanks
  (parse-optional parse-blanks+))

;;> Invokes parser `f` between the parsers `lhs` and `rhs`.

(define (parse-between lhs f rhs)
  (parse-map
    (parse-seq lhs f rhs)
    cadr))

;;> Returns the result of parser `f` but allows preceding its input
;;> with a backslash character to escape it in the parsed input format.

(define (parse-esc f)
  (parse-map
    (parse-seq
      (parse-char #\\)
      f)
    cadr))

;;> Parse an `alist` mapping chars to values which should be returned for each char.

(define (parse-alist alist)
  (apply
    parse-or
    (map
      (lambda (x)
        (parse-map
          (parse-char (car x))
          (lambda (_)
            (cdr x))))
      alist)))

;; Utility procedure for parsing ed(1) BRE addresses.

(define (%parse-regex-lit ch end)
  (parse-with-failure-reason
    (parse-atomic
      (parse-as-string
        (parse-between
          (parse-char ch)
          (parse-repeat (parse-or
                          (parse-esc (parse-char ch))
                          (parse-char (char-set-complement (char-set ch #\newline)))))
          end)))
    "expected regex"))

;;> Parse a regex literal which is enclosed by the character `ch`.

(define (parse-regex-lit ch)
  (%parse-regex-lit
    ch
    (parse-char ch)))

;;> Parse a regex literal which starts with character `ch` and is
;;> terminated by the same character or the end of line.

(define (parse-regex-lit* ch)
  (%parse-regex-lit
    ch
    (parse-or
      (parse-char ch)
      parse-end-of-line)))

;;> Invoke given parser and strip trailing blanks (if any).

(define (parse-strip-blanks parser)
  (parse-map
    (parse-seq
      parser
      parse-blanks)
    car))

;;> Like [parse-seq](#parse-seq) but skip blanks *before* each parser.

(define (parse-blanks-seq . o)
  (define (%parse-blanks-seq lst)
    (parse-seq-list
      (apply append
             (zip (make-list
                        (length lst)
                        (parse-ignore parse-blanks))
                  lst))))

  (%parse-blanks-seq o))

;;> Parse a single line (excluding the terminating newline character).

(define parse-line
  (parse-atomic
    (parse-or
      (parse-bind "" parse-newline) ;; empty line
      (parse-map
        (parse-seq
          (parse-token (lambda (x) (not (char=? x #\newline))))
          ;; XXX: parse-end-of-line does _not_ consume the end-of-file.
          ;; This is crucial for parse-input-mode to work correctly.
          (parse-or parse-newline parse-end-of-line))
        car))))

;;> Feed the result of the parser `ctx` to a single argument procedure `f`.
;;> The procedure must then return a new parser which is executed
;;> afterwards on the same index as `ctx`.

(define (parse-with-context ctx f)
  (lambda (source index sk fk)
    (let* ((yield (lambda (r s i fk) r))
           (value (call-with-parse ctx source index yield fk)))
      ((f value) source index sk fk))))
