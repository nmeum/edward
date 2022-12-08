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

;; Parse an alist mapping chars to values which should be returned for each char.

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

;; Utility procedures for parsing BRE addresses.

(define (%parse-regex-lit ch end)
  (parse-atomic
    (parse-as-string
      (parse-between
        (parse-char ch)
        (parse-repeat (parse-or
                        (parse-esc (parse-char ch))
                        (parse-char (char-set-complement (char-set ch #\newline)))))
        end))))

(define (parse-regex-lit ch)
  (%parse-regex-lit
    ch
    (parse-or
      (parse-char ch)
      parse-end-of-line)))

(define (parse-regex-lit* ch)
  (%parse-regex-lit
    ch
    (parse-char ch)))

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
          ;; XXX: parse-end-of-line does _not_ consume the end-of-file.
          ;; This is crucial for parse-input-mode to work correctly.
          (parse-or parse-newline parse-end-of-line))
        car))))

;; Feed the result of the parser ctx to a single argument procedure f.
;; The procedure must then return a new parser which is executed
;; afterwards on the same index as ctx.

(define (parse-with-context ctx f)
  (lambda (source index sk fk)
    (let* ((yield (lambda (r s i fk) r))
           (value (call-with-parse ctx source index yield)))
      (if value ;; default fk returns #f on error
        ((f value) source index sk fk)
        (fk source index "context parser failed")))))
