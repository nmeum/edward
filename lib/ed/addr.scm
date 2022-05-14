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
    ((start end) (list start #\, end))
    ((start sep end) (list start sep end))))

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

(define parse-addr
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

(define %parse-addr-with-off
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

(define (address-separator? obj)
  (or
    (eq? obj #\,)
    (eq? obj #\;)))

(define parse-separator
  (parse-char address-separator?))

;; The ',' and ';' operators for addresses seem to be right-associative
;; (i.e. the operations are grouped from the right). POSIX doesn't
;; mandate this outright but the address table in the rationale sections
;; assumes right-associative behavior. The table contains the example
;; address `7,5,` which is supposed to be evaluated as `7,(5,)`
;; ultimately yielding `5,5` instead of `(7,5),` which would yield
;; `1,$`. The procedure below converts left-to-right parsed address
;; ranges to ranges which enforce right-associative behavior.

(define (lr->rl lst)
  (fold-right
    (lambda (cur prev-addrlst)
      (let ((last-addr (car prev-addrlst)))
        (if (address-separator? cur)
          (if (find address-separator? last-addr)
            (cons (list cur) prev-addrlst)
            (cons (cons cur last-addr) (cdr prev-addrlst)))
          (cons (cons cur last-addr) (cdr prev-addrlst)))))
    '(()) (concatenate lst)))

;; This procedure expands a given parsed address according to the
;; omission rules mandated by the POSIX standard. The return value
;; will also be an address range.

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

;; Expand an entire address list.
;;
;; This procedure also correctly handles the case where an address
;; is omitted between two seperators (e.g. ";;", ",,", ";," etc).
;;
;; See https://austingroupbugs.net/view.php?id=1582#c5829

(define (expand-addrlst lst)
  (car
    (fold (lambda (%addr prev)
            (let* ((lst  (car prev))
                   (sec  (cdr prev)) ;; second addr from previous seperator expansion (if any)
                   (addr (if (and sec (address-separator? (car %addr)))
                           (cons sec %addr)
                           %addr))
                   (exp  (expand-addr addr)))
              (cons
                (append lst (list exp))
                (and (address-separator? (car %addr))
                     (third exp)))))
          '(() . #f) (lr->rl lst))))

;; Parse an address chain consisting of multiple addresses separated by
;; <comma> or <semicolon> characters. Returns an address list which can
;; be converted to a line pair using the addrlst->lpair procedure.

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

(define parse-addrs
  (parse-map
    %parse-addrs
    expand-addrlst))
