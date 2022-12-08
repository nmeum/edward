;;
; Append Comand
;;

(define (exec-append editor line data)
  (editor-goto!
    editor
    (editor-append! editor line data)))

(define-input-cmd (append exec-append (make-addr '(current-line)))
  (parse-cmd-char #\a))

;;
; Change Command
;;

;; The change command does not support a zero address see:
;;
;;   * https://lists.gnu.org/archive/html/bug-ed/2016-04/msg00009.html
;;   * https://austingroupbugs.net/view.php?id=1130

(define (exec-change editor lines data)
  (editor-goto!
    editor
    (editor-replace! editor lines data)))

(define-input-cmd (change exec-change (make-range))
  (parse-cmd-char #\c))

;;
; Read Command
;;

(define (exec-read editor line filename)
  (let* ((f  (editor-filename editor filename))
         (in (read-from f)))
    (if (and
          (empty-string? (text-editor-filename editor))
          (not (filename-cmd? f)))
      (text-editor-filename-set! editor f))

    (if in
      (begin
        (editor-append! editor line (car in))
        (editor-goto! editor (editor-lines editor))

        ;; Print amount of bytes read (unless in silent mode).
        (editor-verbose editor (cdr in)))
      (editor-error editor "cannot open input file"))))

(define-file-cmd (read exec-read (make-addr '(last-line)))
  (parse-file-cmd #\r))

;;
; Substitute Command
;;

(define (exec-subst editor lines triplet nth)
  (let* ((lst (editor-get-lines editor lines))
         (bre (editor-make-regex editor (first triplet)))
         (rep (editor-restr editor (second triplet)))
         (print? (third triplet))

         ;; Pair (list of replaced lines, line number of last replaced line)
         (re (fold-right (lambda (line lnum y)
                           (let* ((r (regex-replace bre rep line nth))
                                  (n (string-split r "\n" #t)) ;; string → list
                                  (l (append n (car y))))
                             (if (or (equal? r line)        ;; not modified
                                     (not (zero? (cdr y)))) ;; not last
                               (cons l (cdr y))
                               (cons l (+ lnum (dec (length n)))))))
                         '((). 0) lst (editor-line-numbers lines))))
    (if (zero? (cdr re))
      ((subst-nomatch-handler) "no match")
      (begin
        (editor-replace! editor lines (car re))
        (editor-goto! editor (cdr re))))

    ;; Special case handling of omitted regex delimiter in substitute
    ;; command. For the substitute command only the delimiter of the
    ;; replacement can be omitted, not the regex delimiter itself.
    (when print?
      (exec-print editor (range->lpair editor (make-range))))))

(define-edit-cmd (substitute exec-subst (make-range))
  (parse-cmd-char #\s)

  ;; Triplet: (RE, replacement, print?)
  (parse-re-pair
    ;; POSIX only mentions escaping of the delimiter character within
    ;; the RE but not within the replacement thus this is not implemented.
    (lambda (delim)
      (parse-or
        (parse-bind
          'previous-replace
          (parse-char (lambda (c)
                        (and
                          (not (char=? c delim))
                          (char=? c #\%)))))
        (parse-replace delim))))

  (parse-default
    (parse-or
      (parse-bind 0 (parse-char #\g))
      parse-digits)
    1))

;;
; Delete Command
;;

(define (exec-delete editor lines)
  (let ((saddr (car lines)))
    (editor-remove! editor lines)
    (if (buffer-empty? (text-editor-buffer editor))
      (editor-goto! editor 0)
      (editor-goto! editor (min (editor-lines editor) saddr)))))

(define-edit-cmd (delete exec-delete (make-range))
  (parse-cmd-char #\d))

;;
; Edit Command
;;

(define (%exec-edit editor filename)
  (call-when-confirmed editor '%edit
    (lambda ()
      (exec-edit editor filename))))

(define-file-cmd (%edit %exec-edit)
  (parse-file-cmd #\e))

;;
; Edit Without Checking Command
;;

(define (exec-edit editor filename)
  (editor-reset! editor)

  (exec-read editor (addr->line editor (make-addr '(last-line)))
             (editor-filename editor filename))
  (text-editor-modified-set! editor #f)

  ;; exec-read only updates filename if none is set.
  ;; XXX: Might be beneficial to not re-use exec-read here.
  (when (not (filename-cmd? filename))
    (text-editor-filename-set!
      editor
      (editor-filename editor filename))))

(define-file-cmd (edit exec-edit)
  (parse-file-cmd #\E))

;;
; Filename Command
;;

(define (exec-filename editor filename)
  (if (filename-cmd? filename) ;; XXX: Could be handled in parser
    (editor-raise "current filename cannot be a shell command")
    (begin
      (unless (empty-string? filename)
        (text-editor-filename-set! editor filename))
      (println (editor-filename editor)))))

(define-file-cmd (filename exec-filename)
  (parse-file-cmd #\f))

;;
; Global Command
;;

(define (exec-global editor lines regex cmdstr)
  (exec-command-list editor regex-match? lines regex cmdstr))

(define-file-cmd (global exec-global
                         (make-range
                           (make-addr '(nth-line . 1))
                           (make-addr '(last-line))))
  (parse-cmd-char #\g)
  parse-re
  unwrap-command-list)

;;
; Interactive Global Command
;;

(define (exec-interactive editor lines regex)
  (exec-command-list-interactive editor regex-match? lines regex))

(define-file-cmd (interactive exec-interactive
                              (make-range
                                (make-addr '(nth-line . 1))
                                (make-addr '(last-line))))
  (parse-cmd-char #\G)
  parse-re)

;;
; Help Command
;;

(define (exec-help editor)
  (let ((msg (text-editor-error editor)))
    (when msg
      (println msg))))

(define-edit-cmd (help exec-help)
  (parse-cmd-char #\h))

;;
; Help-Mode Command
;;

(define (exec-help-mode editor)
  (let ((prev-help? (text-editor-help? editor)))
    (text-editor-help-set! editor (not prev-help?))
    (when (not prev-help?)
      (exec-help editor))))

(define-edit-cmd (help-mode exec-help-mode)
  (parse-cmd-char #\H))

;;
; Insert Command
;;

(define (exec-insert editor line data)
  (let* ((sline (max (dec line) 0)))
    (editor-goto!
      editor
      (editor-append! editor sline data))))

(define-input-cmd (insert exec-insert (make-addr '(current-line)))
  (parse-cmd-char #\i))

;;
; Join Command
;;

(define (exec-join editor lines)
  (let ((start (car lines))
        (end   (cdr lines)))
    (unless (eqv? start end)
      (editor-join! editor lines)
      (editor-goto! editor start))))

(define-edit-cmd (join exec-join (make-range
                                   (make-addr '(current-line))
                                   (make-addr '(current-line) '(1))))
  (parse-cmd-char #\j))

;;
; Mark Command
;;

(define (exec-mark editor line mark)
  (editor-mark-line editor line mark))

(define-edit-cmd (mark exec-mark (make-addr '(current-line)))
  (parse-cmd-char #\k)
  (parse-char char-set:lower-case))

;;
; List Command
;;

(define (exec-list editor lines)
  (let ((lst (editor-get-lines editor lines))
        (end (cdr lines)))
    (for-each (lambda (line)
                (display
                  (string->human-readable (string-append line "\n"))))
              lst)
    (editor-goto! editor end)))

(define-print-cmd list exec-list #\l (make-range))

;;
; Move Command
;;

(define (exec-move editor lines addr)
  (let ((dest-line (addr->line editor addr)))
    (if (editor-in-range editor lines dest-line)
      (editor-raise "invalid move destination")
      (editor-goto! editor (editor-move! editor lines dest-line)))))

(define-edit-cmd (move exec-move (make-range))
  (parse-cmd-char #\m)
  parse-addr-with-off)

;;
; Copy Command
;;

(define (exec-copy editor lines addr)
  (let ((dest-line (addr->line editor addr)))
    (if (editor-in-range editor lines dest-line)
      (editor-raise "invalid copy destination")
      (let ((data (editor-get-lines editor lines))
            (target (addr->line editor addr)))
        (editor-goto!
          editor
          (editor-append! editor target data))))))

(define-edit-cmd (copy exec-copy (make-range))
  (parse-cmd-char #\t)
  parse-addr-with-off)

;;
; Undo Command
;;

(define (exec-undo editor)
  (editor-undo! editor))

(define-file-cmd (undo exec-undo)
  (parse-cmd-char #\u))

;;
; Global Non-Matched Command
;;

(define (exec-global-unmatched editor lines regex cmdstr)
  (exec-command-list editor (lambda (bre line)
                              (not (regex-match? bre line)))
                     lines regex cmdstr))

(define-file-cmd (global-unmatched exec-global-unmatched
                                   (make-range
                                     (make-addr '(nth-line . 1))
                                     (make-addr '(last-line))))
  (parse-cmd-char #\v)
  parse-re
  unwrap-command-list)

;;
; Interactive Global Not-Matched Command
;;

(define (exec-interactive-unmatched editor lines regex)
  (exec-command-list-interactive editor (lambda (bre line)
                                          (not (regex-match? bre line)))
                                 lines regex))

(define-file-cmd (interactive-unmatched exec-interactive-unmatched
                                        (make-range
                                          (make-addr '(nth-line . 1))
                                          (make-addr '(last-line))))
  (parse-cmd-char #\V)
  parse-re)

;;
; Write Command
;;

(define (exec-write editor lines filename)
  (let ((fn (editor-filename editor filename))
        (data (lines->string (editor-get-lines editor lines))))
    (unless (write-to fn data)
      (editor-raise "cannot open output file"))
    ;; Assuming write-to *always* writes all bytes.
    (editor-verbose editor (count-bytes data))

    (unless (filename-cmd? filename)
      (if (empty-string? (text-editor-filename editor))
        (text-editor-filename-set! editor fn))
      (text-editor-modified-set! editor #f))))

(define-file-cmd (write exec-write
                        (make-range
                          (make-addr '(nth-line . 1))
                          (make-addr '(last-line))))
  (parse-file-cmd #\w))

;;
; Line Number Command
;;

(define (exec-line-number editor addr)
  (println (text-editor-line editor)))

(define-edit-cmd (line-number exec-line-number (make-addr '(last-line)))
  (parse-cmd-char #\=))

;;
; Number Command
;;

(define (exec-number editor lines)
  (let ((lst (editor-get-lines editor lines))
        (eline (cdr lines)))
    (for-each
      (lambda (line number)
        (println number "\t" line))
      lst (editor-line-numbers lines))
    (editor-goto! editor eline)))

(define-print-cmd number exec-number #\n (make-range))

;;
; Print Command
;;

(define (exec-print editor lines)
  (let ((lst (editor-get-lines editor lines))
        (end (cdr lines)))
    (for-each println lst)
    (editor-goto! editor end)))

(define-print-cmd print exec-print #\p (make-range))

;;
; Prompt Command
;;

(define (exec-prompt editor)
  (let* ((repl (text-editor-repl editor))
         (prompt? (repl-prompt? repl)))
    (repl-set-prompt! repl (not prompt?))))

(define-edit-cmd (prompt exec-prompt)
  (parse-cmd-char #\P))

;;
; Quit Command
;;

(define (%exec-quit editor)
  (call-when-confirmed editor '%quit
    (lambda ()
      (exec-quit editor))))

(define-file-cmd (%quit %exec-quit)
  (parse-cmd-char #\q))

;; Special case: quit command via EOF.

;; Manually use register-command here to allow interpreting
;; EOF as a command without a terminating newline character.
(register-command '%eof
  (parse-map
    (parse-or
      parse-end
      (parse-blanks-seq
        (parse-cmd-char #\q)
        (parse-ignore parse-newline)))
    (lambda (args)
      ;; XXX: register-command uses '%eof as a command name
      ;; but for the command itself we use '%quit as well.
      ;; This allows confirming quit commands with EOF and
      ;; vice versa. Furthermore we can filter out the EOF
      ;; handling individually this way (e.g. for g cmd).
      (make-cmd '%quit '() %exec-quit '()))))

;;
; Quit Without Checking Command
;;

(define (exec-quit editor)
  (exit))

(define-file-cmd (quit exec-quit)
  (parse-cmd-char #\Q))

;;
; Shell Escape Command
;;

(define (exec-command editor cmd)
  (let ((cmdstr (fold-right (lambda (x ys)
                              (string-append
                                (match x
                                       ('current-file (editor-filename editor))
                                       ('previous-command (editor-shell-cmd editor))
                                       (_ x))
                                ys)) "" cmd)))
  (unless (and (list? cmd) (every string? cmd)) ;; replacement performed
    (println cmdstr))
  (system cmdstr)
  (editor-verbose editor "!")
  (text-editor-last-cmd-set! editor cmdstr)))

(define-file-cmd (shell-escape exec-command)
  (parse-cmd-char #\!)
  (parse-map
    (parse-seq
      (parse-ignore-optional
        (parse-bind '(previous-command) (parse-char #\!)))
      (parse-repeat
        (parse-commit
          (parse-or
            (parse-bind 'current-file (parse-char #\%))
            (parse-as-string
              (parse-repeat+
                (parse-or
                  (parse-esc (parse-char #\%))
                  (parse-not-char (char-set #\% #\newline)))))))))
    concatenate))

;;
; Null Command
;;

(define (exec-null editor line)
  (if (zero? line)
    (editor-raise "invalid address")
    (begin
      (println (list-ref (buffer->list (text-editor-buffer editor)) (dec line)))
      (editor-goto! editor line))))

(define-file-cmd (null exec-null (make-addr '(current-line) '(+1)))
  (parse-ignore parse-epsilon))