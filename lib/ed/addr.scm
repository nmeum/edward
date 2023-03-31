;;>| Address Constructors
;;>
;;> Procedures which create single address and range values.

;;> Create a single address with an optional offset.

(define make-addr
  (case-lambda
    ((addr) (list addr '()))
    ((addr off) (list addr off))))

;;> Create an address range consisting of two addresses.

(define make-range
  (case-lambda
    (() (make-range (make-addr '(current-line))))
    ((addr) (list addr #\, addr))
    ((start end) (list start #\, end))
    ((start sep end) (list start sep end))))

;;> Predicate which returns true if the parsed address is a range.

(define (range? obj)
  (and (list? obj)
       (eqv? (length obj) 3)))

;;> Convert the given address to a range.

(define (addr->range addr)
  (if (range? addr)
    addr
    (make-range addr)))

;;> Convert the given range to an address.

(define (range->addr addr)
  (if (range? addr)
    (last addr)
    addr))

;;> Predicate which returns true if the given `obj` constitutes an address separator.

(define (address-separator? obj)
  (or
    (eq? obj #\,)
    (eq? obj #\;)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;>| Parser Combinators
;;>
;;> Edward [parser combinators][edward parse] for parsing ed addresses.
;;>
;;> [edward parse]: edward.parse.html

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
      (parse-commit
        (parse-char #\')
        "invalid mark")
      parse-lowercase)
    (lambda (lst)
      (cons 'marked-line (cadr lst)))))

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
    (parse-regex-lit* #\/)
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
    (parse-regex-lit* #\?)
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

;;> Utility procedure for parsing addresses without offset.

(define parse-addr
  (parse-or
    parse-current
    parse-last
    parse-nth
    parse-mark
    parse-forward-bre
    parse-backward-bre
    parse-relative))

(define %parse-addr-with-off
  (parse-repeat
    (parse-map
      (parse-seq
        (parse-ignore parse-blanks)
        (parse-or
          parse-offset
          parse-digits))
      car)))

;;> Addresses can be followed by zero or more address offsets, optionally
;;> separated by blanks. Offsets are a decimal number optionally prefixed by
;;> `+` or `-` character. A `+` or `-` character not followed by a
;;> decimal number shall be interpreted as `+1`/`-1`. This procedure is
;;> responsible for parsing an address with an optional offset.

(define parse-addr-with-off
  (parse-or
    (parse-atomic
      (parse-seq
        parse-addr
        %parse-addr-with-off))
    (parse-fail "unknown address format")))

;; From POSIX-1.2008:
;;
;;   Addresses shall be separated from each other by a <comma> (',') or
;;   <semicolon> character (';').
;;
;; POSIX also mandates a table with rules in case addresses are omitted
;; on either side of the separation character. Consult the standard for
;; more information.

(define parse-separator
  (parse-char address-separator?))

;;> This procedure expands a given parsed address according to the
;;> [omission rules][ed addresses] mandated by the POSIX standard. The
;;> procedure receives an address parsed by [parse-addrs][parse-addrs]
;;> as an input value and returns an [address-range][make-addr].
;;>
;;> [parse-addrs]: #parse-addrs
;;> [make-addr]: #make-addr
;;> [ed addresses]: https://pubs.opengroup.org/onlinepubs/9699919799/utilities/ed.html#tag_20_38_13_02

(define expand-addr
  (match-lambda
    ((#\,)
     (make-range (make-addr '(nth-line . 1))
                 #\,
                 (make-addr '(last-line))))
    ((#\, addr)
     (make-range (make-addr '(nth-line . 1)) #\, addr))
    ((addr #\,)
     (make-range addr #\, addr))
    ((#\;)
     (make-range (make-addr '(current-line)) #\; (make-addr '(last-line))))
    ((#\; addr)
     (make-range (make-addr '(current-line)) #\; addr))
    ((addr #\;)
     (make-range addr #\; addr))
    ((addr1 sep addr2)
     (make-range addr1 sep addr2))))

(define %parse-addrs
  (parse-or
    (parse-repeat+
      (parse-memoize "address parser"
        (parse-seq
          (parse-or
            parse-addr-with-off
            (parse-ignore parse-beginning-of-line))
          (parse-ignore parse-blanks)
          parse-separator
          (parse-ignore parse-blanks)
          (parse-or
            parse-addr-with-off
            (parse-ignore parse-epsilon)))))
    (parse-map
      parse-addr-with-off
      (lambda (addr)
        (list (make-range addr))))))

;;> Parse an address chain consisting of multiple addresses separated by `,` or `;`.

(define parse-addrs
  (parse-map
    %parse-addrs
    concatenate))
