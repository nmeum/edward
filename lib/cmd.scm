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
        (let ((name 'print-suffix)
              (args (list (make-range)))) ;; current line
          (match x
            (#\l (make-cmd name exec-list args))
            (#\n (make-cmd name exec-number args))
            (#\p (make-cmd name exec-print args))))))))

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

(define (cmd-with-print symbol handler cmd-args print-cmd)
  (make-cmd
    symbol
    (lambda (editor . args)
      (editor-exec editor (make-cmd symbol handler args))
      (when print-cmd
        (editor-exec editor print-cmd)))
    cmd-args))

(define-syntax define-file-cmd
  (syntax-rules ()
    ((define-file-cmd (NAME HANDLER) BODY ...)
     (register-command (quote NAME)
       (parse-map
         (parse-blanks-seq
           BODY ...
           (parse-ignore parse-newline))
         (lambda (args)
           (make-cmd (quote NAME) HANDLER args)))))))

(define-syntax define-print-cmd
  (syntax-rules ()
    ((define-print-cmd (NAME HANDLER) BODY ...)
     (register-command (quote NAME)
       (parse-map
         (parse-seq
           (parse-blanks-seq BODY ...)
           (parse-ignore (parse-optional parse-print-cmd))
           (parse-ignore parse-blanks)
           (parse-ignore parse-newline))
         (lambda (args)
           (make-cmd (quote NAME) HANDLER (car args))))))))

(define-syntax define-input-cmd
  (syntax-rules ()
    ((define-input-cmd (NAME HANDLER) BODY ...)
     (register-command (quote NAME)
       (parse-map
         (parse-seq
           (parse-blanks-seq BODY ...)
           (parse-optional parse-print-cmd)
           (parse-ignore parse-blanks)
           parse-input-mode

           ;; Allow input-cmds to be terminated with eof character as well.
           ;; Required for global commands since terminating '.' can be omitted.
           (parse-ignore
             (parse-or
               parse-end
               (parse-seq parse-blanks parse-newline))))
         (lambda (args)
           (cmd-with-print
             (quote NAME)
             HANDLER
             (append (first args) (list (third args)))
             (second args))))))))

(define-syntax define-edit-cmd
  (syntax-rules ()
    ((define-edit-cmd (NAME HANDLER) BODY ...)
     (register-command (quote NAME)
       (parse-map
         (parse-seq
           (parse-blanks-seq BODY ...)
           (parse-optional parse-print-cmd)
           (parse-ignore parse-blanks)
           (parse-ignore parse-newline))
         (lambda (args)
           (cmd-with-print (quote NAME) HANDLER
                           (first args) (second args))))))))

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
  (parse-ignore (parse-char ch)))

;; Read input data in the input mode format. Returns a list of parsed
;; lines as strings which do not include the terminating newlines.

(define %parse-input-mode
  (parse-map
    (parse-seq
      (parse-repeat
        (parse-assert
          parse-line
          (lambda (line)
            (not (equal? line ".")))))
      (parse-or
        parse-end ;; required for global command
        (parse-string ".")))
    car))

(define parse-input-mode
  (parse-map
    (parse-seq
      parse-newline
      %parse-input-mode)
    second))

;; Parse RE pair for the substitute command (e.g. `/RE/replacement/`).
;; The given procedure is responsible for parsing the replacement, it is
;; passed the utilized delimiter as a single character function
;; argument.
;;
;; Returns pair (RE, replacement).

(define (parse-re-pair delim-proc)
  (parse-with-context
    ;; Any character other then <space> and <newline> can be a delimiter.
    (parse-char (char-set-complement (char-set #\space #\newline)))

    (lambda (delim)
      (parse-map
        (parse-seq
          (parse-regex-lit delim)
          (delim-proc delim)
          (parse-ignore (parse-char delim)))
        (lambda (lst) (cons (first lst) (second lst)))))))

;; TODO: Reduce code duplication with parse-re-pair
(define parse-re
  (parse-with-context
    ;; Any character other then <space> and <newline> can be a delimiter.
    (parse-char (char-set-complement (char-set #\space #\newline)))

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

(define unwrap-command-list+
  (parse-map
    (parse-seq
      (parse-repeat
        (parse-map
          (parse-seq
            (parse-repeat+ (parse-not-char (char-set #\\ #\newline)))
            (parse-esc (parse-char #\newline)))
          (match-lambda
            ((lst chr) (append lst (list chr))))))
      (parse-repeat+ (parse-not-char #\newline))) ;; last line
    (match-lambda
      ((lines last-line)
        (string-append
          (apply string-append (map list->string lines))
          (list->string last-line)
          "\n"))))) ;; terminate last command with newline

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

(define (each-matched-line editor range regex match-proc line-proc)
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
              (editor-get-range editor range))))

;; Execute a command list, parsed using unwrap-command-list, for the g and v command.

(define (exec-command-list editor match-proc range regex cmdstr)
  (let ((cmds (parse-command-list cmdstr)))
    (each-matched-line editor range regex match-proc
                       (lambda (lnum line)
                         (editor-goto! editor lnum)
                         (editor-exec-cmdlist editor cmds)))))

(define (exec-command-list-interactive editor match-proc range regex)
  (define previous-command '())
  (define (get-interactive editor)
    (let* ((cmd (editor-interactive editor))
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

  (each-matched-line editor range regex match-proc
                     (lambda (lnum line)
                       (println line)
                       (let ((cmd (get-interactive editor)))
                         (when cmd ;; not null command
                           (editor-goto! editor lnum)
                           (editor-exec editor cmd))))))

;; Parses a filename which is then read/written by ed.

(define parse-filename
  (parse-or
    (parse-map
      (parse-seq
        (parse-string "!")
        (parse-as-string
          (parse-repeat+ (parse-not-char #\newline))))
      (lambda (lst) (apply string-append lst)))
    (parse-as-string (parse-repeat (parse-char char-set:graphic)))))

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
  (if (and
        (not (empty-string? fn))
        (eqv? (string-ref fn 0) #\!))
    (values #t (string-copy fn 1))
    (values #f fn)))

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
  (let-values (((fn-cmd? fn) (filename-cmd? filename)))
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
  (let-values (((fn-cmd? fn) (filename-cmd? filename)))
    (with-io-error-handler fn
      (lambda ()
        (if fn-cmd?
          (call-with-input-pipe fn port->lines)
          (call-with-input-file fn port->lines))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
; Append Comand
;;

(define (exec-append editor addr data)
  (editor-goto!
    editor
    (editor-append! editor addr data)))

(define-input-cmd (append exec-append)
  (parse-default parse-addr (make-addr '(current-line)))
  (parse-cmd-char #\a))

;;
; Change Command
;;

;; XXX handling of address 0 is actually somewhat disputed:
;;
;;   * https://lists.gnu.org/archive/html/bug-ed/2016-04/msg00009.html
;;   * https://austingroupbugs.net/view.php?id=1130

(define (exec-change editor range data)
  (editor-goto!
    editor
    (editor-replace! editor range data)))

(define-input-cmd (change exec-change)
  (parse-default parse-addr-range (make-range))
  (parse-cmd-char #\c))

;;
; Read Command
;;

(define (exec-read editor addr filename)
  (let* ((f  (editor-filename editor filename))
         (in (read-from f)))
    (if (and
          (empty-string? (text-editor-filename editor))
          (not (filename-cmd? f)))
      (text-editor-filename-set! editor f))

    (if in
      (begin
        (editor-append! editor addr (car in))
        (editor-goto! editor (editor-lines editor))

        ;; Print amount of bytes read (unless in silent mode).
        (editor-verbose editor (cdr in)))
      (editor-error editor "cannot open input file"))))

(define-file-cmd (read exec-read)
  (parse-default parse-addr (make-addr '(last-line)))
  (parse-file-cmd #\r))

;;
; Substitute Command
;;

(define (exec-subst editor range subst nth)
  (let* ((lst (editor-get-range editor range))
         (bre (editor-make-regex editor (car subst)))
         (rep (editor-restr editor (cdr subst)))

         ;; Pair (list of replaced lines, line number of last replaced line)
         (re (fold-right (lambda (line lnum y)
                           (let* ((r (regex-replace bre rep line nth))
                                  (n (string-split r #\newline)) ;; string → list
                                  (l (append n (car y))))
                             (if (or (equal? r line)        ;; not modified
                                     (not (zero? (cdr y)))) ;; not last
                               (cons l (cdr y))
                               (cons l (+ lnum (dec (length n)))))))
                         '((). 0) lst (range->lines editor range))))
    (if (zero? (cdr re))
      ((subst-nomatch-handler) "no match")
      (begin
        (editor-replace! editor range (car re))
        (editor-goto! editor (cdr re))))))

(define-edit-cmd (substitute exec-subst)
  (parse-default parse-addr-range (make-range))
  (parse-cmd-char #\s)

  ;; Pair: (RE, replacement)
  (parse-re-pair
    ;; POSIX only mentions escaping of the delimiter character within
    ;; the RE but not within the replacement thus this is not implemented.
    (lambda (delim)
      (parse-or
        (parse-bind
          'previous-replace
          (parse-assert
            (parse-repeat+ (parse-not-char delim))
            (lambda (lst)
              (equal? lst '(#\%)))))
        parse-replace)))

  (parse-default
    (parse-or
      (parse-bind 0 (parse-char #\g))
      parse-digits)
    1))

;;
; Delete Command
;;

(define (exec-delete editor range)
  (let-values (((saddr _) (editor-range editor range)))
    (editor-remove! editor range)
    (if (buffer-empty? (text-editor-buffer editor))
      (editor-goto! editor 0)
      (editor-goto! editor (min (editor-lines editor) saddr)))))

(define-edit-cmd (delete exec-delete)
  (parse-default parse-addr-range (make-range))
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

  (exec-read editor (make-addr '(last-line))
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

(define (exec-global editor range regex cmdstr)
  (exec-command-list editor regex-match? range regex cmdstr))

(define-file-cmd (global exec-global)
  (parse-default parse-addr-range
                 (make-range
                   (make-addr '(nth-line . 1))
                   (make-addr '(last-line))))
  (parse-cmd-char #\g)
  parse-re
  unwrap-command-list)

;;
; Interactive Global Command
;;

(define (exec-interactive editor range regex)
  (exec-command-list-interactive editor regex-match? range regex))

(define-file-cmd (interactive exec-interactive)
  (parse-default parse-addr-range
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

(define (exec-insert editor addr data)
  (let* ((line (addr->line editor addr))
         (sline (max (dec line) 0))
         (saddr (make-addr (cons 'nth-line sline))))
    (editor-goto!
      editor
      (editor-append! editor saddr data))))

(define-input-cmd (insert exec-insert)
  (parse-default parse-addr (make-addr '(current-line)))
  (parse-cmd-char #\i))

;;
; Join Command
;;

(define (exec-join editor range)
  (let-values (((start end) (editor-range editor range)))
    (unless (eqv? start end)
      (editor-join! editor range)
      (editor-goto! editor start))))

(define-edit-cmd (join exec-join)
  (parse-default parse-addr-range
                 (make-range
                   (make-addr '(current-line))
                   (make-addr '(current-line) '(1))))
  (parse-cmd-char #\j))

;;
; Mark Command
;;

(define (exec-mark editor addr mark)
  (editor-mark-line
    editor (addr->line editor addr) mark))

(define-edit-cmd (mark exec-mark)
  (parse-default parse-addr (make-addr '(current-line)))
  (parse-cmd-char #\k)
  (parse-char char-set:lower-case))

;;
; List Command
;;

(define (exec-list editor range)
  (let-values (((lst) (editor-get-range editor range))
               ((_ end) (editor-range editor range)))
    (for-each (lambda (line)
                (display
                  (string->human-readable (string-append line "\n"))))
              lst)
    (editor-goto! editor end)))

(define-print-cmd (list exec-list)
  (parse-default parse-addr-range (make-range))
  (parse-cmd-char #\l))

;;
; Move Command
;;

(define (exec-move editor range addr)
  (if (editor-in-range editor range addr)
    (editor-raise "invalid move destination")
    (editor-goto! editor (editor-move! editor range addr))))

(define-edit-cmd (move exec-move)
  (parse-default parse-addr-range (make-range))
  (parse-cmd-char #\m)
  parse-addr)

;;
; Copy Command
;;

(define (exec-copy editor range addr)
  (if (editor-in-range editor range addr)
    (editor-raise "invalid copy destination")
    (let ((data (editor-get-range editor range))
          (target (addr->line editor addr)))
      (editor-goto!
        editor
        (editor-append! editor addr data)))))

(define-edit-cmd (copy exec-copy)
  (parse-default parse-addr-range (make-range))
  (parse-cmd-char #\t)
  parse-addr)

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

(define (exec-global-unmatched editor range regex cmdstr)
  (exec-command-list editor (lambda (bre line)
                              (not (regex-match? bre line)))
                     range regex cmdstr))

(define-file-cmd (global-unmatched exec-global-unmatched)
  (parse-default parse-addr-range
                 (make-range
                   (make-addr '(nth-line . 1))
                   (make-addr '(last-line))))
  (parse-cmd-char #\v)
  parse-re
  unwrap-command-list)

;;
; Interactive Global Not-Matched Command
;;

(define (exec-interactive-unmatched editor range regex)
  (exec-command-list-interactive editor (lambda (bre line)
                                          (not (regex-match? bre line)))
                                 range regex))

(define-file-cmd (interactive-unmatched exec-interactive-unmatched)
  (parse-default parse-addr-range
                 (make-range
                   (make-addr '(nth-line . 1))
                   (make-addr '(last-line))))
  (parse-cmd-char #\V)
  parse-re)

;;
; Write Command
;;

(define (exec-write editor range filename)
  (let ((fn (editor-filename editor filename))
        (data (lines->string (editor-get-range editor range))))
    (unless (write-to fn data)
      (editor-raise "cannot open output file"))
    ;; Assuming write-to *always* writes all bytes.
    (editor-verbose editor (count-bytes data))

    (unless (filename-cmd? filename)
      (if (empty-string? (text-editor-filename editor))
        (text-editor-filename-set! editor fn))
      (text-editor-modified-set! editor #f))))

(define-file-cmd (write exec-write)
  (parse-default parse-addr-range
                 (list
                   (make-addr '(nth-line . 1))
                   #\,
                   (make-addr '(last-line))))
  (parse-file-cmd #\w))

;;
; Line Number Command
;;

(define (exec-line-number editor addr)
  (println (text-editor-line editor)))

(define-edit-cmd (line-number exec-line-number)
  (parse-default parse-addr (make-addr '(last-line)))
  (parse-cmd-char #\=))

;;
; Number Command
;;

(define (exec-number editor range)
  (let-values (((lst) (editor-get-range editor range))
               ((_ eline) (editor-range editor range)))
    (for-each
      (lambda (line number)
        (println number "\t" line))
      lst (range->lines editor range))
    (editor-goto! editor eline)))

(define-print-cmd (number exec-number)
  (parse-default parse-addr-range (make-range))
  (parse-cmd-char #\n))

;;
; Print Command
;;

(define (exec-print editor range)
  (let-values (((lst) (editor-get-range editor range))
               ((_ end) (editor-range editor range)))
    (for-each println lst)
    (editor-goto! editor end)))

(define-print-cmd (print exec-print)
  (parse-default parse-addr-range (make-range))
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
        (parse-or
          (parse-bind 'current-file (parse-char #\%))
          (parse-as-string
            (parse-repeat+
              (parse-or
                (parse-esc (parse-char #\%))
                (parse-not-char (char-set #\% #\newline))))))))
    concatenate))

;;
; Null Command
;;

(define (exec-null editor addr)
  (let ((line (addr->line editor addr)))
    (if (zero? line)
      (editor-raise "invalid address")
      (begin
        (println (list-ref (buffer->list (text-editor-buffer editor)) (dec line)))
        (editor-goto! editor line)))))

(define-file-cmd (null exec-null)
  (parse-default parse-addr (make-addr '(current-line) '(+1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Parse any of the commands listed above and strip any trailing blanks
;; as the command letter can be preceded by zero or more <blank>
;; characters.
;;
;; Returns a list where car is the handler for the parsed commands and
;; cdr are the arguments which are supposed to be passed to this
;; handler.

;; TODO: Commit to individual command parsers and don't backtrack.
;; Implementing this would require separating address parsing from
;; command parsing since we can only commit to a command after we
;; have parsed the address successfully (the same address may be
;; applicable to different commands).
(define (%parse-cmd parsers)
  (apply
    parse-or
    (append parsers (list (parse-fail "unknown command")))))

(define parse-cmd
  (%parse-cmd (get-command-parsers '())))

(define parse-global-cmd
  (%parse-cmd
    ;; Filter out cmds producing undefined behaviour in global command.
    (get-command-parsers '(global interactive global-unmatched
                           interactive-unmatched shell-escape))))

(define parse-interactive-cmd
  (parse-or
    (parse-bind 'null-command parse-newline)
    (parse-bind 'repeat-previous (parse-string "&\n"))
    (%parse-cmd
      ;; Filter out cmds not supported in interactive mode (as per POSIX).
      (get-command-parsers '(append change insert global interactive
                             global-unmatched interactive-unmatched)))))
