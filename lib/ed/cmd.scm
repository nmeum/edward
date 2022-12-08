;; Command parsers are registered in the following alist through macros
;; provided below. These macros use the register-command procedure.
;; Parsers can be obtained from the alist using get-command-parsers.

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

;; Print commands (l, n, p) are additionally tracked in a seperated
;; alist. This eases implementing commands which can be prefixed
;; with a print command (see parse-print-cmd definition below).

(define print-commands '())

(define (register-print-command char proc)
  (set! print-commands
    (alist-cons char proc print-commands)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; According to POSIX.1-2008 it is invalid for more than one command to
;; appear on a line. However, commands other than e, E, f, q, Q, r, w, and !
;; can be suffixed by the commands l, n, or p. In this case the suffixed
;; command is executed and then the new current line is written as
;; defined by the l, n, or p command.

(define parse-print-cmd
  (parse-lazy ;; must be lazy, otherwise print-commands is not populated.
    (parse-strip-blanks
      (parse-map
        (parse-alist print-commands)
        (lambda (proc)
          (make-cmd 'print-suffix (make-range) proc '()))))))

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
    ((define-print-cmd NAME HANDLER CHAR ADDR)
     (begin
       (register-print-command CHAR HANDLER)
       (register-command (quote NAME)
         (parse-map
           (parse-seq
             (parse-blanks-seq (parse-cmd-char CHAR))
             (parse-ignore (parse-optional parse-print-cmd))
             (parse-ignore parse-blanks)
             (parse-ignore parse-newline))
           (lambda (args)
             (make-cmd (quote NAME) ADDR HANDLER (car args)))))))))

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
  (call-with-parse (parse-repeat+ (parse-global-cmd))
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
    (let* ((cmd (editor-interactive editor (parse-interactive-cmd)))
           (ret (match cmd
                  ('eof (editor-raise "unexpected end-of-file"))
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

(define (parse-global-cmd)
  (%parse-cmd
    ;; Filter out cmds producing undefined behaviour in global command.
    (get-command-parsers '(%eof global interactive global-unmatched
                           interactive-unmatched shell-escape))))

(define (parse-interactive-cmd)
  (parse-or
    (parse-bind 'eof parse-end)
    (parse-bind 'null-command parse-newline)
    (parse-bind 'repeat-previous (parse-string "&\n"))
    (%parse-cmd
      ;; Filter out cmds not supported in interactive mode (as per POSIX).
      (get-command-parsers '(%eof append change insert global interactive
                             global-unmatched interactive-unmatched)))))
