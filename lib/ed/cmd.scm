(define command-parsers '())

(define (register-command name proc)
  (set! command-parsers
    (alist-cons name proc command-parsers)))

(define (get-command-parsers exclude)
  (fold (lambda (x y)
          (if (member (car x) exclude)
            y
            (cons (cdr x) y)))
        '() command-parsers))

;; According to POSIX.1-2008 it is invalid for more than one command to
;; appear on a line. However, commands other than e, E, f, q, Q, r, w, and !
;; can be suffixed by the commands l, n, or p. In this case the suffixed
;; command is executed and then the new current line is written as
;; defined by the l, n, or p command.

(define parse-print-cmd
  (parse-strip-blanks
    (parse-map
      (parse-char (char-set #\l #\n #\p))
      (lambda (x)
        ;; Current line shall be written described below under the l, n, and p commands.
        ;; XXX: Can't rely on command-parsers here as it hasn't been filled yet.
        (let ((name 'print-suffix))
          (match x
            (#\l (make-cmd name (make-range) exec-list '()))
            (#\n (make-cmd name (make-range) exec-number '()))
            (#\p (make-cmd name (make-range) exec-print '()))))))))

;; Edward distinguishes four command types: (1) print commands, i.e. the
;; l, n, and p commands, which can be used as a suffix to (2) editor
;; commands and (3) file-cmd (the e, E, f, q, Q, r, w, and !commands
;; which cannot be suffixed with a print command). Lastly, an input-cmd
;; command type is available which is a variant of the edit-cmd type and
;; allows defining commands which read additional inputs using the ed(1)
;; input mode.
;;
;; Command parsers are defined using the macro below, as part of this
;; definition each command is assigned to one of the aforedmentioned
;; types Additionally, a handler is specified for each command, the
;; parser created by the macro returns the handler and the arguments
;; passed to the command on a succesfull parse.

(define (cmd-with-print symbol def-addr handler cmd-args print-cmd)
  (make-cmd
    symbol
    def-addr
    (lambda (editor . args)
      (if (null? def-addr) ;; If command expects address
        (editor-exec editor #f (make-cmd symbol def-addr handler args))
        (editor-xexec editor (car args) (make-cmd symbol def-addr handler (cdr args))))
      (when print-cmd
        (editor-exec editor #f print-cmd)))
    cmd-args))

(define-syntax define-file-cmd
  (syntax-rules ()
    ((define-file-cmd (NAME HANDLER ADDR) BODY ...)
     (register-command (quote NAME)
       (parse-map
         (parse-blanks-seq
           BODY ...
           (parse-ignore parse-newline))
         (lambda (args)
           (make-cmd (quote NAME) ADDR HANDLER args)))))
    ((define-file-cmd (NAME HANDLER) BODY ...)
     (define-file-cmd (NAME HANDLER '()) BODY ...))))

(define-syntax define-print-cmd
  (syntax-rules ()
    ((define-print-cmd (NAME HANDLER ADDR) BODY ...)
     (register-command (quote NAME)
       (parse-map
         (parse-seq
           (parse-blanks-seq BODY ...)
           (parse-ignore (parse-optional parse-print-cmd))
           (parse-ignore parse-blanks)
           (parse-ignore parse-newline))
         (lambda (args)
           (make-cmd (quote NAME) ADDR HANDLER (car args))))))))

(define-syntax define-input-cmd
  (syntax-rules ()
    ((define-input-cmd (NAME HANDLER ADDR) BODY ...)
     (register-command (quote NAME)
       (parse-map
         (parse-seq
           (parse-blanks-seq BODY ...)
           (parse-optional parse-print-cmd)
           (parse-ignore parse-blanks)
           (parse-ignore parse-newline)

           parse-input-mode
           (parse-ignore
             (parse-or
               parse-end
               (parse-seq
                 (parse-string ".")
                 (parse-seq parse-blanks parse-newline)))))
         (lambda (args)
           (cmd-with-print
             (quote NAME)
             ADDR
             HANDLER
             (append (first args) (list (third args)))
             (second args))))))))

(define-syntax define-edit-cmd
  (syntax-rules ()
    ((define-edit-cmd (NAME HANDLER ADDR) BODY ...)
     (register-command (quote NAME)
       (parse-map
         (parse-seq
           (parse-blanks-seq BODY ...)
           (parse-optional parse-print-cmd)
           (parse-ignore parse-blanks)
           (parse-ignore parse-newline))
         (lambda (args)
           (cmd-with-print (quote NAME) ADDR HANDLER
                           (first args) (second args))))))
    ((define-edit-cmd (NAME HANDLER) BODY ...)
     (define-edit-cmd (NAME HANDLER '()) BODY ...))))

;; If changes have been made to the current buffer since the last write
;; of the buffer to a file, then ed should warn the user before the
;; buffer is destroyed. Warnings must be confirmed by repeating the
;; command which destroys the buffer.
;;
;; This procedure expects an editor record, the symbol of the command
;; to be repeated and a thunk executed if the command was confirmed or
;; no confirmation is necessary (i.e. buffer was not modified).

(define (call-when-confirmed editor cmd-sym thunk)
  (if (or
        (eqv? (text-editor-prevcmd editor) cmd-sym)
        (not (text-editor-modified? editor)))
    (thunk)
    ;; Can't use editor-raise here as the prevcmd in the
    ;; editor record is not updated then (see editor-start).
    (editor-error editor "Warning: buffer modified")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Parse a command character within a parse-blanks-seq / parse-seq. This
;; character is ignored in the parse-blanks-seq and as such not
;; returned.

(define (parse-cmd-char ch)
  ;; TODO: Prefix failure reason with command char that failed to parse.
  (parse-ignore (parse-commit (parse-char ch))))

;; Read input data in the input mode format. Returns a list of parsed
;; lines as strings which do not include the terminating newlines.

(define parse-input-mode
  (parse-repeat
    (parse-assert
      parse-line
      (lambda (line)
        (not (equal? line "."))))))

;; Parse a delimiter for a regular expression. As per POSIX, any
;; character other then <space> and <newline> can be a delimiter.

(define parse-delim-char
  (parse-char (char-set-complement (char-set #\space #\newline))))

;; Parse RE pair for the substitute command (e.g. `/RE/replacement/`).
;; The given procedure is responsible for parsing the replacement, it is
;; passed the utilized delimiter as a single character function
;; argument.
;;
;; Returns triplet (RE, replacement, print?).

(define (parse-re-pair delim-proc)
  (parse-with-context
    parse-delim-char
    (lambda (delim)
      (parse-seq
        (parse-regex-lit* delim)
        (delim-proc delim)
        (parse-or
          (parse-bind #t parse-end-of-line)
          (parse-bind #f (parse-char delim)))))))

(define parse-re
  (parse-with-context
    parse-delim-char
    (lambda (delim)
      (parse-regex-lit delim))))

;; Parameterizable handler for substution cases where no addressed
;; line matched the desired substitution. Can be overwritten using
;; parameterize.

(define subst-nomatch-handler
  (make-parameter
    (lambda (msg)
      (editor-raise msg))))

;; Read lines of a command list and perform unescaping of newlines.
;; Returns a string which can then be further processed using
;; parse-command-list. Basically, this is a two stage parsing process.

(define parse-line-continuation
  (parse-map
    (parse-seq
      (parse-token (lambda (x)
                     (and
                       (not (char=? x #\\))
                       (not (char=? x #\newline)))))
      (parse-esc (parse-char #\newline)))
    (lambda (lst)
      (string-append (car lst) "\n"))))

(define parse-last-line
  (parse-map
    (parse-token (lambda (x) (not (char=? x #\newline))))
    (lambda (str)
      (string-append str "\n"))))

(define unwrap-command-list+
  (parse-map
    (parse-seq
      (parse-repeat parse-line-continuation)
      parse-last-line)
    (lambda (lst)
      (string-append
        (apply string-append (first lst))
        (second lst)))))

(define unwrap-command-list
  (parse-or
    ;; empty command list is equivalent to the p command
    (parse-bind "p\n" parse-end-of-line)
    unwrap-command-list+))

;; Returns list of editor command from a command list string as created
;; by the unwrap-command-list procedure. The list can afterwards be
;; passed to the editor-exec-cmdlist procedure.

(define (parse-command-list cmdstr)
  (call-with-parse (parse-repeat+ parse-global-cmd)
                   (string->parse-stream cmdstr)
                   0
                   (lambda (r s i fk)
                     (if (parse-stream-end? s i)
                       r
                       (fk s i "incomplete command list parse")))
                   (lambda (s i reason) (editor-raise reason))))

;; Execute line-proc for each matched line for a global command.

(define (each-matched-line editor lines regex match-proc line-proc)
  (let ((bre (editor-make-regex editor regex)))
    (for-each (lambda (line)
                (when (match-proc bre line)
                  ;; The executed command may perform modifications
                  ;; which affect line numbers. As such, we find the
                  ;; current number for the given line using pointer
                  ;; comparision on the text editor buffer.
                  (let ((lnum (editor-get-lnum editor line)))
                    (when lnum ;; line has not been deleted by a preceeding command
                      (parameterize ((subst-nomatch-handler id))
                        (line-proc lnum line))))))
              (editor-get-lines editor lines))))

;; Execute a command list, parsed using unwrap-command-list, for the g and v command.

(define (exec-command-list editor match-proc lines regex cmdstr)
  (let ((cmds (parse-command-list cmdstr)))
    (each-matched-line editor lines regex match-proc
                       (lambda (lnum line)
                         (editor-goto! editor lnum)
                         (editor-exec-cmdlist editor cmds)))))

(define (exec-command-list-interactive editor match-proc lines regex)
  (define previous-command '())
  (define (get-interactive editor)
    (let* ((cmd (editor-interactive editor parse-interactive-cmd))
           (ret (match cmd
                  ('null-command #f)
                  ('repeat-previous
                   (if (null? previous-command)
                     (editor-raise "no previous command")
                     previous-command))
                  (_ cmd))))
      (when ret
        (set! previous-command ret))
      ret))

  (each-matched-line editor lines regex match-proc
                     (lambda (lnum line)
                       (println line)
                       (let ((cmd-pair (get-interactive editor)))
                         (when cmd-pair ;; not null command
                           (editor-goto! editor lnum)
                           (editor-exec editor (car cmd-pair) (cdr cmd-pair)))))))

;; Parses a filename which is then read/written by ed.

(define parse-filename
  (parse-atomic
    (parse-or
      (parse-map
        (parse-seq
          (parse-string "!")
          (parse-token (lambda (x) (not (char=? x #\newline)))))
        (lambda (lst) (apply string-append lst)))
      (parse-token char-set:graphic))))

;; Parses a command character followed by an optional file parameter.
;; The compontests **must** be separated by one or more <blank>
;; characters.

(define (parse-file-cmd ch)
  (parse-map
    (parse-seq
      (parse-cmd-char ch)
      (parse-default
        (parse-map (parse-seq parse-blanks+ parse-filename) cadr)
        ""))
    car))

(define (filename-cmd? fn)
  (and
    (not (empty-string? fn))
    (eqv? (string-ref fn 0) #\!)))

(define (filename-unwrap fn)
  (let ((fn-cmd? (filename-cmd? fn)))
    (if fn-cmd?
      (values #t (string-copy fn 1))
      (values #f fn))))

(define (with-io-error-handler fn thunk)
  (call-with-current-continuation
    (lambda (k)
      (with-exception-handler
        (lambda (eobj)
          (fprintln (current-error-port) fn ": "
                    (error-object-message eobj))
          (k #f))
        thunk))))

;; Write given data to given filename. If filename starts with `!` (i.e.
;; is a command according to filename-cmd?), write data to standard
;; input of given command string.

(define (write-to filename data)
  (let-values (((fn-cmd? fn) (filename-unwrap filename)))
    (with-io-error-handler fn
      (lambda ()
        (let ((proc (lambda (port) (write-string data port))))
          (if fn-cmd?
            (call-with-output-pipe fn proc)
            (call-with-output-file fn proc)))))))

;; Read data from given filename as a list of lines. If filename start
;; with `!` (i.e. is a command), read data from the standard output of
;; the given command.
;;
;; If an error occurs returns false and prints an error message to the
;; current-error-port. Otherwise, returns a pair of retrieved lines and
;; amount of total bytes received.

(define (read-from filename)
  (let-values (((fn-cmd? fn) (filename-unwrap filename)))
    (with-io-error-handler fn
      (lambda ()
        (if fn-cmd?
          (call-with-input-pipe fn port->lines)
          (call-with-input-file fn port->lines))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define-print-cmd (list exec-list (make-range))
  (parse-cmd-char #\l))

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

(define-print-cmd (number exec-number (make-range))
  (parse-cmd-char #\n))

;;
; Print Command
;;

(define (exec-print editor lines)
  (let ((lst (editor-get-lines editor lines))
        (end (cdr lines)))
    (for-each println lst)
    (editor-goto! editor end)))

(define-print-cmd (print exec-print (make-range))
  (parse-cmd-char #\p))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Parse any of the commands listed above and strip any trailing blanks
;; as the command letter can be preceded by zero or more <blank>
;; characters.
;;
;; Returns a list where car is the handler for the parsed commands and
;; cdr are the arguments which are supposed to be passed to this
;; handler.

(define (%parse-cmd parsers)
  (parse-map
    (parse-seq
      (parse-optional parse-addrs)
      (apply
        parse-or
        (append parsers (list (parse-fail "unknown command")))))
    (lambda (x)
      (let ((cmd (last x))
            (addr (first x)))
        (cons addr cmd)))))

(define (parse-cmd)
  (%parse-cmd (get-command-parsers '())))

(define parse-global-cmd
  (%parse-cmd
    ;; Filter out cmds producing undefined behaviour in global command.
    (get-command-parsers '(%eof global interactive global-unmatched
                           interactive-unmatched shell-escape))))

(define parse-interactive-cmd
  (parse-or
    (parse-bind 'null-command parse-newline)
    (parse-bind 'repeat-previous (parse-string "&\n"))
    (%parse-cmd
      ;; Filter out cmds not supported in interactive mode (as per POSIX).
      (get-command-parsers '(%eof append change insert global interactive
                             global-unmatched interactive-unmatched)))))
