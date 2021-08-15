;; Addresses in ed
;;
;; An address is a list consisting of an address specification and a
;; (possibly empty) list of offsets which should be applied to this
;; address specification. An address specification is a pair consisting
;; of a symbol and an optional argument.
;;
;; The text in this section is aligned with the `Addresses in ed`
;; section in the POSIX-1.2008 specification of `ed(1)`.

(define make-addr
  (case-lambda
    ((addr) (list addr '()))
    ((addr off) (list addr off))))

;; The <period> character ('.') shall address the current line.

(define parse-current
  (parse-map
    (parse-char #\.)
    (lambda (ch)
      (cons 'current-line '()))))

;; The <dollar-sign> character ('$') shall address the last line of
;; the edit buffer.

(define parse-last
  (parse-map
    (parse-char #\$)
    (lambda (ch)
      (cons 'last-line '()))))

;; The positive decimal number n shall address the nth line of the edit
;; buffer.

(define parse-nth
  (parse-map
    parse-digits
    (lambda (num)
      (cons 'nth-line num))))

;; The <apostrophe>-x character pair ("'x") shall address the line
;; marked with the mark name character x, which shall be a lowercase
;; letter from the portable character set. It shall be an error if the
;; character has not been set to mark a line or if the line that was
;; marked is not currently present in the edit buffer.

(define parse-mark
  (parse-map
    (parse-seq
      (parse-char #\')
      (parse-char char-set:lower-case))
    (lambda (lst)
      (cons 'marked-line (cadr lst)))))

;; Utility procedure for parsing BRE addresses.

(define (parse-regex-lit ch)
  (parse-as-string
    (parse-between
      (parse-char ch)
      (parse-repeat (parse-or
                      (parse-esc* (parse-char ch))
                      (parse-not-char ch)))
      (parse-or
        (parse-char ch)
        parse-end))))

;; A BRE enclosed by <slash> characters ('/') shall address the first
;; line found by searching forwards from the line following the
;; current line toward the end of the edit buffer. The second <slash>
;; can be omitted at the end of a command line. Within the BRE, a
;; <backslash>-<slash> pair ("\/") shall represent a literal <slash>
;; instead of the BRE delimiter.

(define parse-forward-bre
  (parse-map
    (parse-regex-lit #\/)
    (lambda (str)
      (cons 'regex-forward str))))

;; A BRE enclosed by <question-mark> characters ('?') shall address
;; the first line found by searching backwards from the line preceding
;; the current line toward the beginning of the edit buffer. The second
;; <question-mark> can be omitted at the end of a command line. Within
;; the BRE, a <backslash>-<question-mark> pair ("\?") shall represent
;; a literal <question-mark> instead of the BRE delimiter.

(define parse-backward-bre
  (parse-map
    (parse-regex-lit #\?)
    (lambda (str)
      (cons 'regex-backward str))))

;; A <plus-sign> ('+') or <hyphen-minus> character ('-') followed by a
;; decimal number shall address the current line plus or minus the
;; number. A <plus-sign> or <hyphen-minus> character not followed by a
;; decimal number shall address the current line plus or minus 1.

(define parse-offset
  (parse-map
    (parse-seq
      (parse-or
        (parse-char #\+)
        (parse-char #\-))
      (parse-optional parse-digits))
    (lambda (lst)
      (let* ((fst (car lst))
             (snd (cadr lst))
             (num (if snd snd 1)))
        (if (eqv? fst #\-)
          (- 0 num) num)))))

(define parse-relative
  (parse-map
    parse-offset
    (lambda (num)
      (cons 'relative num))))

;; Utility procedure for parsing addresses without offset.

(define %parse-addr
  (parse-or
    parse-current
    parse-last
    parse-nth
    parse-mark
    parse-forward-bre
    parse-backward-bre
    parse-relative
    (parse-fail "unknown address format")))

;; Addresses can be followed by zero or more address offsets,
;; optionally <blank>-separated. Address offsets are constructed
;; as follows:
;;
;;  * A <plus-sign> or <hyphen-minus> character followed by a decimal
;;    number shall add or subtract, respectively, the indicated number of
;;    lines to or from the address. A <plus-sign> or <hyphen-minus
;;    character not followed by a decimal number shall add or subtract 1
;;    to or from the address.
;;
;;  * A decimal number shall add the indicated number of lines to the address.

(define parse-addr-offsets
  (parse-repeat
    (parse-map
      (parse-seq
        (parse-ignore parse-blanks)
        (parse-or
          parse-offset
          parse-digits))
      car)))

(define parse-addr-with-off
  (parse-seq
    %parse-addr
    parse-addr-offsets))

;; Addresses shall be separated from each other by a <comma> (',') or
;; <semicolon> character (';'). In the case of a <semicolon> separator,
;; the current line ('.') shall be set to the first address, and only then
;; will the second address be calculated.
;;
;; Addresses can be omitted on either side of the <comma> or <semicolon>
;; separator, in which case the resulting address pairs shall be as
;; follows:
;;
;;  (1) ,      → 1 , $
;;  (2) , addr → 1 , addr
;;  (3) addr , → addr , addr
;;  (4) ;      → . ; $
;;  (5) ; addr → . ; addr
;;  (6) addr ; → addr ; addr
;;
;; Any <blank> characters included between addresses, address separators,
;; or address offsets shall be ignored.

(define %parse-addr-range
  (parse-map
    (parse-seq
      (parse-optional parse-addr-with-off)
      (parse-ignore parse-blanks)
      (parse-or
        (parse-char #\,)
        (parse-char #\;))
      (parse-ignore parse-blanks)
      (parse-optional parse-addr-with-off))

    (match-lambda
      ((#f #\, #f)
       (list (make-addr '(nth-line . 1))
             #\,
             (make-addr '(last-line))))
      ((#f #\, addr)
       (list (make-addr '(nth-line . 1)) #\, addr))
      ((addr #\, #f)
       (list addr #\, addr))
      ((#f #\; #f)
       (list (make-addr '(current-line)) #\; (make-addr '(last-line))))
      ((#f #\; addr)
       (list (make-addr '(current-line)) #\; addr))
      ((addr #\; #f)
       (list addr #\; addr))
      ((addr1 sep addr2)
       (list addr1 sep addr2)))))

;; From the OpenBSD ed(1) man page:
;;
;;   If only one address is given in a range, then the second address is
;;   set to the given address. If only one address is expected, then the
;;   last address is used.

(define parse-addr-range
  (parse-or
    %parse-addr-range
    (parse-map
      parse-addr-with-off
      (lambda (addr)
        (list addr #\, addr)))))

(define parse-addr
  (parse-map
    parse-addr-range
    last))
