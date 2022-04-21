;; The edward address parsing code distinguishes single addresses
;; and ranges. The latter consisting of a start and end address as well
;; as a address separator (as defined in POSIX). The parse-addr
;; procedure returns a list of address ranges. The editor implementation
;; is capable of converting this list to a pair of line numbers using
;; the addrlst->lpair procedure. Command implementations expecting a
;; range address receive this pair, commands which only expect a single
;; address only receive the first element of the pair as an argument.

;; Create a single address with an optional offset.

(define make-addr
  (case-lambda
    ((addr) (list addr '()))
    ((addr off) (list addr off))))

;; Create an address range consisting of two addresses.

(define make-range
  (case-lambda
    (() (make-range (make-addr '(current-line))))
    ((addr) (list addr #\, addr))
    ((start end) (list start #\, end))))

;; Returns true if the parsed address is a range.

(define (range? obj)
  (and (list? obj)
       (eqv? (length obj) 3)))

;; Convert the given address to a range.

(define (addr->range addr)
  (if (range? addr)
    addr
    (make-range addr)))

;; Convert the given range to an address.

(define (range->addr addr)
  (if (range? addr)
    (last addr)
    addr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; From POSIX.1-2008:
;;
;;   The <period> character ('.') shall address the current line.

(define parse-current
  (parse-map
    (parse-char #\.)
    (lambda (ch)
      (cons 'current-line '()))))

;; From POSIX.1-2008:
;;
;;   The <dollar-sign> character ('$') shall address the last line of
;;   the edit buffer.

(define parse-last
  (parse-map
    (parse-char #\$)
    (lambda (ch)
      (cons 'last-line '()))))

;; From POSIX.1-2008:
;;
;;   The positive decimal number n shall address the nth line of the
;;   edit buffer.

(define parse-nth
  (parse-map
    parse-digits
    (lambda (num)
      (cons 'nth-line num))))

;; From POSIX.1-2008:
;;
;;   The <apostrophe>-x character pair ("'x") shall address the line
;;   marked with the mark name character x, which shall be a lowercase
;;   letter from the portable character set.

(define parse-mark
  (parse-map
    (parse-seq
      (parse-char #\')
      (parse-char char-set:lower-case))
    (lambda (lst)
      (cons 'marked-line (cadr lst)))))

;; Utility procedure for parsing BRE addresses.

(define (parse-regex-lit ch)
  (parse-atomic
    (parse-as-string
      (parse-between
        (parse-char ch)
        (parse-repeat (parse-or
                        (parse-esc (parse-char ch))
                        (parse-not-char ch)))
        (parse-or
          (parse-char ch)
          parse-end)))))

;; From POSIX.1-2008:
;;
;;   A BRE enclosed by <slash> characters ('/') shall address the first
;;   line found by searching forwards from the line following the
;;   current line toward the end of the edit buffer. The second <slash>
;;   can be omitted at the end of a command line. Within the BRE, a
;;   <backslash>-<slash> pair ("\/") shall represent a literal <slash>
;;   instead of the BRE delimiter.

(define parse-forward-bre
  (parse-map
    (parse-regex-lit #\/)
    (lambda (str)
      (cons 'regex-forward str))))

;; From POSIX-1.2008:
;;
;;   A BRE enclosed by <question-mark> characters ('?') shall address
;;   the first line found by searching backwards from the line preceding
;;   the current line toward the beginning of the edit buffer. The
;;   second <question-mark> can be omitted at the end of a command line.
;;   Within the BRE, a <backslash>-<question-mark> pair ("\?") shall
;;   represent a literal <question-mark> instead of the BRE delimiter.

(define parse-backward-bre
  (parse-map
    (parse-regex-lit #\?)
    (lambda (str)
      (cons 'regex-backward str))))

;; From POSIX.1-2008:
;;
;;   A <plus-sign> ('+') or <hyphen-minus> character ('-') followed by a
;;   decimal number shall address the current line plus or minus the
;;   number. A <plus-sign> or <hyphen-minus> character not followed by a
;;   decimal number shall address the current line plus or minus 1.

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
    parse-relative))

;; Addresses can be followed by zero or more address offsets, optionally
;; <blank>-separated. Offsets are a decimal number optionally prefixed
;; by <plus-sign> or <hyphen-minus> character. A <plus-sign> or
;; <hyphen-minus> character not followed by a decimal number shall be
;; interpreted as +1/-1.

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
  (parse-or
    (parse-atomic
      (parse-seq
        %parse-addr
        parse-addr-offsets))
    (parse-fail "unknown address format")))

;; From POSIX-1.2008:
;;
;;   Addresses shall be separated from each other by a <comma> (',') or
;;   <semicolon> character (';'). In the case of a <semicolon>
;;   separator, the current line ('.') shall be set to the first
;;   address, and only then will the second address be calculated.
;;
;; POSIX also mandates a table with rules in case addresses are omitted
;; on either side of the separation character. Consult the standard for
;; more information.

(define (address-seperator? obj)
  (or
    (eq? obj #\,)
    (eq? obj #\;)))

(define parse-range-sep
  (parse-char address-seperator?))

(define %parse-addr-ng
  (parse-or
    (parse-repeat+
      (parse-seq
        (parse-or
          parse-addr-with-off
          (parse-bind #f parse-beginning-of-line))
        parse-range-sep
        (parse-or
          parse-addr-with-off
          (parse-bind #f parse-epsilon))))
    (parse-map
      parse-addr-with-off
      (lambda (addr)
        (list (make-range addr))))))

(define transform-addr
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
     (list addr1 sep addr2))))

(define parse-addr-ng
  (parse-map
    %parse-addr-ng
    (lambda (lst)
      (map transform-addr lst))))

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
