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
  (parse-map-substring
    (parse-repeat+ (parse-char char-set:digit))
    (lambda (str)
      (cons 'nth-line (string->number str)))))

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

;;> A BRE enclosed by <slash> characters ('/') shall address the first
;;> line found by searching forwards from the line following the
;;> current line toward the end of the edit buffer and stopping at the
;;> first line for which the line excluding the terminating <newline>
;;> matches the BRE.

;; TODO: Terminating / character is optional
(define parse-bre
  (parse-map
    (parse-between
      #\/
      (parse-repeat (parse-or parse-esc (parse-not-char #\\)))
      #\/)
    (lambda (lst)
      (list->string lst))))

(define parse-unknown
  (lambda (r s i fk)
    (fk s i "unknown address format")))

(define parse-addr
  (parse-or
    parse-current
    parse-last
    parse-nth
    parse-mark
    parse-bre
    parse-unknown))
