;;> \section{Addresses in ed}

;; The text in this section is aligned with the `Addresses in ed`
;; section in the POSIX-1.2008 specification of `ed(1)`.

;;> The <period> character ('.') shall address the current line.

(define parse-current
  (parse-map
    (parse-char #\.)
    (lambda (ch)
      'current-line)))

;;> The <dollar-sign> character ('$') shall address the last line of
;;> the edit buffer.

(define parse-last
  (parse-map
    (parse-char #\$)
    (lambda (ch)
      'last-line)))

;;> The positive decimal number \var{n} shall address the nth line of
;;> the edit buffer.

(define parse-nth
  (parse-map
    parse-digits
    (lambda (num)
      (cons 'nth-line num))))

;;> The <apostrophe>-x character pair ("'x") shall address the line
;;> marked with the mark name character \var{x}, which shall be a lowercase
;;> letter from the portable character set. It shall be an error if the
;;> character has not been set to mark a line or if the line that was
;;> marked is not currently present in the edit buffer.

(define parse-mark
  (parse-map
    (parse-seq
      (parse-char #\')
      (parse-repeat+ (parse-char char-set:lower-case)))
    (lambda (lst)
      (cons 'marked-line
            (list->string (cadr lst))))))

;; Utility procedure for parsing BRE addresses.

(define (parse-regex-lit ch)
  (parse-string
    (parse-between
      (parse-char ch)
      (parse-repeat (parse-or parse-esc (parse-not-char ch)))
      (parse-or
        (parse-char ch)
        parse-end))))

;;> A BRE enclosed by <slash> characters ('/') shall address the first
;;> line found by searching forwards from the line following the
;;> current line toward the end of the edit buffer. The second <slash>
;;> can be omitted at the end of a command line. Within the BRE, a
;;> <backslash>-<slash> pair ("\/") shall represent a literal <slash>
;;> instead of the BRE delimiter.

(define parse-forward-bre
  (parse-map
    (parse-regex-lit #\/)
    (lambda (str)
      (cons 'regex-forward str))))

;;> A BRE enclosed by <question-mark> characters ('?') shall address
;;> the first line found by searching backwards from the line preceding
;;> the current line toward the beginning of the edit buffer. The second
;;> <question-mark> can be omitted at the end of a command line. Within
;;> the BRE, a <backslash>-<question-mark> pair ("\?") shall represent
;;> a literal <question-mark> instead of the BRE delimiter.

(define parse-backward-bre
  (parse-map
    (parse-regex-lit #\?)
    (lambda (str)
      (cons 'regex-backward str))))

(define parse-addr
  (parse-or
    parse-current
    parse-last
    parse-nth
    parse-mark
    parse-forward-bre
    parse-backward-bre
    (parse-fail "unknown address format")))
