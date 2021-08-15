;; Commands in ed
;;
;; Commands to ed have a simple and regular structure: zero, one, or two
;; addresses followed by a single-character command, possibly followed
;; by parameters to that command. These addresses specify one or more
;; lines in the buffer. Every command that requires addresses has
;; default addresses (shown in parentheses), so that the addresses very
;; often can be omitted. Implemented ed commands are listed below.
;;
;; The text in this section is aligned with the `Commands in ed`
;; section in the POSIX-1.2008 specification of `ed(1)`.

(define command-parsers (list (parse-fail "unknown command")))
(define (register-command proc)
  (set! command-parsers (cons proc command-parsers)))

;; It is generally invalid for more than one command to appear on a
;; line. However, any command (except e, E, f, q, Q, r, w, and !) can be
;; suffixed by the letter l, n, or p; in which case, except for the l,
;; n, and p commands, the command shall be executed and then the new
;; current line shall be written as described below under the l, n, and
;; p commands.

(define parse-print-cmd
  (parse-strip-blanks
    (parse-map
      (parse-char (char-set #\l #\n #\p))
      (lambda (x)
        ;; Current line shall be written described below under the l, n, and p commands.
        (let ((cur (list (make-addr '(current-line))
                         #\,
                         (make-addr '(current-line)))))
          (match x
            (#\l (list exec-list cur))
            (#\n (list exec-number cur))
            (#\p (list exec-print cur))))))))

;; In order to allow adding a l, n, or p suffix to certain commands,
;; edward dinstinguishes three kinds of commands: (1) print commands,
;; i.e. the l, n, and p commands, which can be used as a suffix to (2)
;; editor commands and (3) file-cmd (the e, E, f, q, Q, r, w, and !
;; commands which cannot be suffixed with a print command). Command
;; parsers are defined using the macro below, as part of this definition
;; each command is assigned to one of the aforedmentioned groups.
;; Additionally, a handler is specified for each command, the parser
;; created by the macro returns the handler and the arguments passed to
;; the command on a succesfull parse.

(define-syntax define-command
  (syntax-rules (edit-cmd print-cmd file-cmd)
    ((define-command (file-cmd HANDLER) BODY ...)
     (register-command
       (parse-map
         (parse-blanks-seq
           BODY ...)
         (lambda (args)
           (cons
             (with-ret HANDLER (quote HANDLER))
             args)))))
    ((define-command (print-cmd HANDLER) BODY ...)
     (register-command
       (parse-map
         (parse-seq
           (parse-blanks-seq BODY ...)
           (parse-ignore (parse-optional parse-print-cmd)))
         (lambda (args)
           (cons
             (with-ret HANDLER (quote HANDLER))
             (car args))))))
    ((define-command (edit-cmd HANDLER) BODY ...)
     (register-command
       (parse-map
         (parse-seq
           (parse-blanks-seq BODY ...)
           (parse-optional parse-print-cmd))
         (lambda (orig-args)
           (cons
             (lambda (editor . args)
               (apply HANDLER editor args)
               (let ((pcmd (last orig-args)))
                 (when pcmd
                   (apply (car pcmd) editor (cdr pcmd))))
               (quote HANDLER))
             (car orig-args))))))))

;; If changes have been made in the buffer since the last w command that
;; wrote the entire buffer, ed shall warn the user if an attempt is made
;; to destroy the editor buffer via the e or q commands. The ed utility
;; shall write the string: "?\n" (followed by an explanatory message if
;; help mode has been enabled via the H command) to standard output and
;; shall continue in command mode with the current line number
;; unchanged. If the e or q command is repeated with no intervening
;; command, it shall take effect.

(define-syntax define-confirm
  (syntax-rules ()
    ((define-confirm (NAME PROC))
     (define (NAME editor . args)
       (if (or
             (eqv? (text-editor-prevcmd editor) (quote NAME))
             (not (text-editor-modified? editor)))
         (apply PROC editor args)
         ;; XXX: Can't use error here as the return value is not propagated then.
         (editor-error editor "Warning: buffer modified"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Parse a command character within a parse-blanks-seq / parse-seq. This
;; character is ignored in the parse-blanks-seq and as such not
;; returned.

(define (parse-cmd ch)
  (parse-ignore (parse-char ch)))

;; Parses a filename which is then read/written by ed.
;;
;; TODO: If file is replaced by '!', the rest of the line shall be taken
;; to be a shell command line whose output is to be read. Such a shell
;; command line shall not be remembered as the current file.

(define parse-filename
  (parse-or
    (parse-map
      (parse-seq
        (parse-string "!")
        (parse-as-string
          (parse-repeat+ (parse-char (lambda (x) #t)))))
      (lambda (lst) (apply string-append lst)))
    (parse-as-string (parse-repeat (parse-not-char char-set:blank)))))

;; Parses a command character followed by an optional file parameter.
;; The compontests **must** be separated by one or more <blank>
;; characters.

(define (parse-file-cmd ch)
  (parse-map
    (parse-seq
      (parse-cmd ch)
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

;; Write given data to given filename. If filename starts with `!` (i.e.
;; is a command according to filename-cmd?), write data to standard
;; input of given command string.

(define (write-to filename data)
  (let-values (((fn-cmd? fn) (filename-cmd? filename)))
    (if fn-cmd?
      (pipe-to fn data)
      (call-with-output-file fn
        (lambda (port)
          (write-string data port))))))

;; Read data from given filename as a list of lines. If filename start
;; with `!` (i.e. is a command), read data from the standard output of
;; the given command.

(define (read-from filename)
  (let-values (((fn-cmd? fn) (filename-cmd? filename)))
    (if fn-cmd?
      (pipe-from fn)
      (file->buffer fn))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Append Comand
;;
;;   (.)a
;;   <text>
;;   .
;;
;; The a command shall read the given text and append it after the
;; addressed line; the current line number shall become the address of
;; the last inserted line or, if there were none, the addressed line.
;; Address 0 shall be valid for this command; it shall cause the
;; appended text to be placed at the beginning of the buffer.

(define (exec-append editor addr)
  (editor-goto! editor addr)
  (let* ((data (editor-read-input editor))
         (last-inserted (editor-append! editor data)))
    (editor-goto! editor last-inserted)))

(define-command (edit-cmd exec-append)
  (parse-default parse-addr (make-addr '(current-line)))
  (parse-cmd #\a))

;; Change Command
;;
;;   (.,.)c
;;   <text>
;;   .
;;
;; The c command shall delete the addressed lines, then accept input
;; text that replaces these lines; the current line shall be set to the
;; address of the last line input; or, if there were none, at the line
;; after the last line deleted; if the lines deleted were originally at
;; the end of the buffer, the current line number shall be set to the
;; address of the new last line; if no lines remain in the buffer, the
;; current line number shall be set to zero. Address 0 shall be valid
;; for this command; it shall be interpreted as if address 1 were
;; specified.

;; XXX handling of address 0 is actually somewhat disputed:
;;
;;   * https://lists.gnu.org/archive/html/bug-ed/2016-04/msg00009.html
;;   * https://austingroupbugs.net/view.php?id=1130

(define (exec-change editor range)
  (let ((in (editor-read-input editor)))
    (editor-goto! editor (editor-replace! editor range in))))

(define-command (edit-cmd exec-change)
  (parse-default parse-addr-range
                 (list
                   (make-addr '(current-line))
                   #\,
                   (make-addr '(current-line))))
  (parse-cmd #\c))

;; Read Command
;;
;;   ($)r [file]
;;
;; The r command shall read in the file named by the pathname file and
;; append it after the addressed line. If no file argument is given, the
;; currently remembered pathname, if any, shall be used (see the e and f
;; commands). The currently remembered pathname shall not be changed
;; unless there is no remembered pathname. Address 0 shall be valid for
;; r and shall cause the file to be read at the beginning of the buffer.
;;
;; The current line number shall be set to the address of the last line
;; read in.

(define (exec-read editor addr filename)
  (editor-goto! editor addr)
  (let* ((f (editor-filename editor filename))
         (r (read-from f)))
    (if (and
          (empty-string? (text-editor-filename editor))
          (not (filename-cmd? f)))
      (text-editor-filename-set! editor f))

    (editor-append! editor (car r))
    (editor-goto! editor (length (text-editor-buffer editor)))

    (editor-verbose editor (cdr r))))

(define-command (file-cmd exec-read)
  (parse-default parse-addr (make-addr '(last-line)))
  (parse-file-cmd #\r))

;; Substitute Command

(define (exec-subst editor range regex replace)
  ;; TODO: Support for special '%' character
  ;; TODO: Like in sed any character can be used as a delimiter.
  ;; TODO: Support flags
  (let ((lst (editor-get-range editor range))
        (bre (make-bre regex)))
    (editor-replace!
      editor
      range
      (map (lambda (x)
             (regex-replace bre replace x))
           lst))))

(define-command (edit-cmd exec-subst)
  (parse-default parse-addr-range
                 (list
                   (make-addr '(current-line))
                   #\,
                   (make-addr '(current-line))))
  (parse-cmd #\s)
  (parse-regex-lit #\/)
  (parse-as-string
    (parse-repeat+ (parse-not-char #\/)))
  (parse-ignore (parse-char #\/)))

;; Delete Command
;;
;;   (.,.)d
;;
;; The d command shall delete the addressed lines from the buffer. The
;; address of the line after the last line deleted shall become the
;; current line number; if the lines deleted were originally at the end
;; of the buffer, the current line number shall be set to the address of
;; the new last line; if no lines remain in the buffer, the current line
;; number shall be set to zero.

(define (exec-delete editor range)
  (let ((saddr (addr->line editor (first range))))
    (editor-remove! editor range)
    (if (null? (text-editor-buffer editor))
      (editor-goto! editor 0)
      (editor-goto! editor (min (length (text-editor-buffer editor)) saddr)))))

(define-command (edit-cmd exec-delete)
  (parse-default parse-addr-range
                 (list
                   (make-addr '(current-line))
                   #\,
                   (make-addr '(current-line))))
  (parse-cmd #\d))

;; Edit Command
;;
;;  e [file]
;;
;; The e command shall delete the entire contents of the buffer and then
;; read in the file named by the pathname file. The current line number
;; If no pathname is given, the currently remembered pathname, if any,
;; shall be used (see the f command). All marks shall be discarded upon
;; the completion of a successful e command. If the buffer has changed
;; since the last time the entire buffer was written, the user shall be
;; warned, as described previously.

(define-confirm (%exec-edit exec-edit))
(define-command (file-cmd %exec-edit)
  (parse-file-cmd #\e))

;; Edit Without Checking Command
;;
;;   E [file]
;;
;; The E command shall possess all properties and restrictions of the e
;; command except that the editor shall not check to see whether any
;; changes have been made to the buffer since the last w command.

(define (exec-edit editor filename)
  (text-editor-buffer-set! editor '())
  (text-editor-marks-set! editor '())
  (exec-read editor (make-addr '(last-line))
             (editor-filename editor filename)))

(define-command (file-cmd exec-edit)
  (parse-file-cmd #\E))

;; Filename Command
;;
;;   f [file]
;;
;; If file is given, the f command shall change the currently remembered
;; pathname to file; whether the name is changed or not, it shall then
;; write the (possibly new) currently remembered pathname to the
;; standard output.

(define (exec-filename editor filename)
  (if (filename-cmd? filename) ;; XXX: Could be handled in parser
    (error "current filename cannot be a shell command")
    (begin
      (unless (empty-string? filename)
        (text-editor-filename-set! editor filename))
      (editor-verbose editor (editor-filename editor)))))

(define-command (file-cmd exec-filename)
  (parse-file-cmd #\f))

;; Help Command
;;
;;   h
;;
;; The h command shall write a short message to standard output that
;; explains the reason for the most recent '?' notification. The current
;; line number shall be unchanged.

(define (exec-help editor)
  (let ((msg (text-editor-error editor)))
    (when msg
      (println msg))))

(define-command (edit-cmd exec-help)
  (parse-cmd #\h))

;; Help-Mode Command
;;
;;   H
;;
;; The H command shall cause ed to enter a mode in which help messages
;; (see the h command) shall be written to standard output for all
;; subsequent '?' notifications. The H command alternately shall turn
;; this mode on and off; it is initially off. If the help-mode is being
;; turned on, the H command also explains the previous '?' notification,
;; if there was one. The current line number shall be unchanged.

(define (exec-help-mode editor)
  (let ((prev-help? (text-editor-help? editor)))
    (text-editor-help-set! editor (not prev-help?))
    (when (not prev-help?)
      (exec-help editor))))

(define-command (edit-cmd exec-help-mode)
  (parse-cmd #\H))

;; Insert Command
;;
;;   (.)i
;;   <text>
;;   .
;;
;; The i command shall insert the given text before the addressed line;
;; the current line is set to the last inserted line or, if there was
;; none, to the addressed line. This command differs from the a command
;; only in the placement of the input text. Address 0 shall be valid for
;; this command; it shall be interpreted as if address 1 were specified.

(define (exec-insert editor addr)
  (let ((line (addr->line editor addr)))
    (editor-goto! editor (max (dec line) 0))
    (let* ((data (editor-read-input editor))
           (last-inserted (editor-append! editor data)))
      (editor-goto! editor last-inserted))))

(define-command (edit-cmd exec-insert)
  (parse-default parse-addr (make-addr '(current-line)))
  (parse-cmd #\i))

;; Join Command
;;
;;   (.,.+1)j
;;
;; The j command shall join contiguous lines by removing the appropriate
;; <newline> characters. If exactly one address is given, this command
;; shall do nothing. If lines are joined, the current line number shall
;; be set to the address of the joined line; otherwise, the current line
;; number shall be unchanged.

(define (exec-join editor range)
  (let ((start (addr->line editor (first range)))
        (end (addr->line editor (last range))))
    (unless (eqv? start end)
      (editor-join! editor range)
      (editor-goto! editor start))))

(define-command (edit-cmd exec-join)
  (parse-default parse-addr-range
                 (list
                   (make-addr '(current-line))
                   #\,
                   (make-addr '(current-line) '(1))))
  (parse-cmd #\j))

;; Mark Command
;;
;;  (.)kx
;;
;; The k command shall mark the addressed line with name x, which the
;; application shall ensure is a lowercase letter from the portable
;; character set. The address "'x" shall then refer to this line; the
;; current line number shall be unchanged.

(define (exec-mark editor addr mark)
  (editor-mark-line
    editor (addr->line editor addr) mark))

(define-command (edit-cmd exec-mark)
  (parse-default parse-addr (make-addr '(current-line)))
  (parse-cmd #\k)
  (parse-char char-set:lower-case))

;; List Command

(define (exec-list editor range)
  (let ((lst (editor-get-range editor range))
        (end (addr->line editor (last range))))
  (for-each (lambda (line)
              (display
                (string->human-readable (string-append line "\n"))))
            lst)
  (editor-goto! editor end)))

(define-command (print-cmd exec-list)
  (parse-default parse-addr-range
                 (list
                   (make-addr '(current-line))
                   #\,
                   (make-addr '(current-line))))
  (parse-cmd #\l))

;; Move Command
;;
;;   (,.,)maddress
;;
;; The m command shall reposition the addressed lines after the line
;; addressed by address.  Address 0 shall be valid for address and cause
;; the addressed lines to be moved to the beginning of the buffer. It
;; shall be an error if address address falls within the range of moved
;; lines. The current line number shall be set to the address of the
;; last line moved.

(define (exec-move editor range addr)
  (if (editor-in-range editor range addr)
    (error "invalid move destination")
    (let ((data (editor-get-range editor range))
          (target (addr->line editor addr)))
      (exec-delete editor range)
      (editor-goto! editor (min target
                                (length (text-editor-buffer editor))))

      (let ((last-inserted (editor-append! editor data)))
        (editor-goto! editor last-inserted)))))

(define-command (edit-cmd exec-move)
  (parse-default parse-addr-range
                 (list
                   (make-addr '(current-line))
                   #\,
                   (make-addr '(current-line))))
  (parse-cmd #\m)
  parse-addr)

;; Copy Command
;;
;;  (.,.)taddress
;;
;; The t command shall be equivalent to the m command, except that a
;; copy of the addressed lines shall be placed after address address
;; (which can be 0); the current line number shall be set to the address
;; of the last line added.

(define (exec-copy editor range addr)
  (if (editor-in-range editor range addr)
    (error "invalid copy destination")
    (let ((data (editor-get-range editor range))
          (target (addr->line editor addr)))
      (editor-goto! editor addr)
      (let ((last-inserted (editor-append! editor data)))
        (editor-goto! editor last-inserted)))))

(define-command (edit-cmd exec-copy)
  (parse-default parse-addr-range
                 (list
                   (make-addr '(current-line))
                   #\,
                   (make-addr '(current-line))))
  (parse-cmd #\t)
  parse-addr)

;; Write Command
;;
;;   (1,$)w [file]
;;
;; The w command shall write the addressed lines into the file named by
;; the pathname file.  The command shall create the file, if it does not
;; exist, or shall replace the contents of the existing file. The
;; currently remembered pathname shall not be changed unless there is no
;; remembered pathname.  If no pathname is given, the currently
;; remembered pathname, if any, shall be used (see the e and f
;; commands); the current line number shall be unchanged. If the command
;; is successful, the number of bytes written shall be written to
;; standard output,

(define (exec-write editor range filename)
  (let ((fn (editor-filename editor filename))
        (data (buffer->string (editor-get-range editor range))))
    ;; Assuming write-to *always* writes all bytes.
    (write-to fn data)
    (editor-verbose editor (string-length data))

    (unless (filename-cmd? filename)
      (if (empty-string? (text-editor-filename editor))
        (text-editor-filename-set! editor fn))
      (text-editor-modified-set! editor #f))))

(define-command (file-cmd exec-write)
  (parse-default parse-addr-range
                 (list
                   (make-addr '(nth-line . 1))
                   #\,
                   (make-addr '(last-line))))
  (parse-file-cmd #\w))

;; Line Number Command
;;
;;  ($)=
;;
;; The line number of the addressed line shall be written to standard
;; output in the following format:
;;
;;   "%d\n", <line number>
;;
;; The current line number shall be unchanged by this command.

(define (exec-line-number editor addr)
  (println (text-editor-line editor)))

(define-command (edit-cmd exec-line-number)
  (parse-default parse-addr (make-addr '(last-line)))
  (parse-cmd #\=))

;; Number Command
;;
;;   (.,.)n
;;
;; The n command shall write to standard output the addressed lines,
;; preceding each line by its line number and a <tab>; the current line
;; number shall be set to the address of the last line written. The n
;; command can be appended to any command other than e, E, f, q, Q, r,
;; w, or !.

(define (exec-number editor range)
  (let ((lst (editor-get-range editor range))
        (sline (addr->line editor (first range)))
        (eline (addr->line editor (last range))))
    (for-each
      (lambda (pair)
        (println (car pair) "\t" (cadr pair)))
      (zip (iota (inc (- eline sline)) sline) lst))
    (editor-goto! editor eline)))

(define-command (print-cmd exec-number)
  (parse-default parse-addr-range
                 (list
                   (make-addr '(current-line))
                   #\,
                   (make-addr '(current-line))))
  (parse-cmd #\n))

;; Print Command
;;
;;  (.,.)p
;;
;; The p command shall write to standard output the addressed lines; the
;; current line number shall be set to the address of the last line
;; written. The p command can be appended to any command other than e,
;; E, f, q, Q, r, w, or !.

(define (exec-print editor range)
  (let ((lst (editor-get-range editor range))
        (end (addr->line editor (last range))))
  (for-each println lst)
  (editor-goto! editor end)))

(define-command (print-cmd exec-print)
  (parse-default parse-addr-range
                 (list
                   (make-addr '(current-line))
                   #\,
                   (make-addr '(current-line))))
  (parse-cmd #\p))

;; Prompt Command
;;
;;   P
;;
;; The P command shall cause ed to prompt with an <asterisk> ('*') (or
;; string, if -p is specified) for all subsequent commands. The P
;; command alternatively shall turn this mode on and off; it shall be
;; initially on if the -p option is specified; otherwise, off. The
;; current line number shall be unchanged.

(define (exec-prompt editor)
  (let* ((input-handler (text-editor-input-handler editor))
         (prompt? (input-handler-prompt? input-handler)))
    (input-handler-set-prompt!
      input-handler
      (not prompt?))))

(define-command (edit-cmd exec-prompt)
  (parse-cmd #\P))

;; Quit Command
;;
;;   q
;;
;; The q command shall cause ed to exit. If the buffer has changed since
;; the last time the entire buffer was written, the user shall be warned,
;; as described previously.

(define-confirm (%exec-quit exec-quit))
(define-command (file-cmd %exec-quit)
  (parse-cmd #\q))

;; Quit Without Checking Command
;;
;;   Q
;;
;; The Q command shall cause ed to exit without checking whether changes
;; have been made in the buffer since the last w command.

(define (exec-quit editor)
  (exit))

(define-command (file-cmd exec-quit)
  (parse-cmd #\Q))

;; Null Command
;;
;;  (.+1)
;;
;; An address alone on a line shall cause the addressed line to be
;; written. A <newline> alone shall be equivalent to "+1p".  The current
;; line number shall be set to the address of the written line.

(define (exec-null editor addr)
  (let ((line (addr->line editor addr)))
    (if (zero? line)
      (error "invalid address")
      (begin
        (println (list-ref (text-editor-buffer editor) (dec line)))
        (editor-goto! editor line)))))

(define-command (edit-cmd exec-null)
  (parse-default parse-addr (make-addr '(current-line) '(+1)))
  (parse-ignore parse-eof-object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Parse any of the commands listed above and strip any trailing blanks
;; as the command letter can be preceded by zero or more <blank>
;; characters.
;;
;; Returns a list where car is the handler for the parsed commands and
;; cdr are the arguments which are supposed to be passed to this
;; handler.

;; TODO: Commit to individual command parsers and don't backtrack.
(define parse-cmds
  (parse-strip-blanks (apply parse-or command-parsers)))
