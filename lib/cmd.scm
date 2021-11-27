(define command-parsers (list (parse-fail "unknown command")))
(define (register-command proc)
  (set! command-parsers (cons proc command-parsers)))

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
        (let ((cur (make-range)))
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
           BODY ...
           (parse-ignore parse-newline))
         (lambda (args)
           (cons
             (with-ret HANDLER (quote HANDLER))
             args)))))
    ((define-command (print-cmd HANDLER) BODY ...)
     (register-command
       (parse-map
         (parse-seq
           (parse-blanks-seq BODY ...)
           (parse-ignore (parse-optional parse-print-cmd))
           (parse-ignore parse-blanks)
           (parse-ignore parse-newline))
         (lambda (args)
           (cons
             (with-ret HANDLER (quote HANDLER))
             (car args))))))
    ((define-command (edit-cmd HANDLER) BODY ...)
     (register-command
       (parse-map
         (parse-seq
           (parse-blanks-seq BODY ...)
           (parse-optional parse-print-cmd)
           (parse-ignore parse-blanks)
           (parse-ignore parse-newline))
         (lambda (orig-args)
           (cons
             (lambda (editor . args)
               (apply HANDLER editor args)
               (let ((pcmd (last orig-args)))
                 (when pcmd
                   (apply (car pcmd) editor (cdr pcmd))))
               (quote HANDLER))
             (car orig-args))))))))

;; If changes have been made to the current buffer since the last write
;; of the buffer to a file, then ed should warn the user before the
;; buffer is destroyed. Warnings must be confirmed by repeating the
;; command which destroys the buffer.

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

(define parse-filename
  (parse-or
    (parse-map
      (parse-seq
        (parse-string "!")
        (parse-as-string
          (parse-repeat+ (parse-not-char #\newline))))
      (lambda (lst) (apply string-append lst)))
    (parse-as-string (parse-repeat (parse-not-char char-set:whitespace)))))

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

(define (exec-append editor addr)
  (editor-goto! editor addr)
  (let* ((data (editor-read-input editor))
         (last-inserted (editor-append! editor data)))
    (editor-goto! editor last-inserted)))

(define-command (edit-cmd exec-append)
  (parse-default parse-addr (make-addr '(current-line)))
  (parse-cmd #\a))

;;
; Change Command
;;

;; XXX handling of address 0 is actually somewhat disputed:
;;
;;   * https://lists.gnu.org/archive/html/bug-ed/2016-04/msg00009.html
;;   * https://austingroupbugs.net/view.php?id=1130

(define (exec-change editor range)
  (let ((in (editor-read-input editor)))
    (editor-goto! editor (editor-replace! editor range in))))

(define-command (edit-cmd exec-change)
  (parse-default parse-addr-range (make-range))
  (parse-cmd #\c))

;;
; Read Command
;;

(define (exec-read editor addr filename)
  (editor-goto! editor addr)
  (let*-values (((f) (editor-filename editor filename))
                ((numbytes lines) (read-from f)))
    (if (and
          (empty-string? (text-editor-filename editor))
          (not (filename-cmd? f)))
      (text-editor-filename-set! editor f))

    (editor-append! editor lines)
    (editor-goto! editor (length (text-editor-buffer editor)))

    (editor-verbose editor numbytes)))

(define-command (file-cmd exec-read)
  (parse-default parse-addr (make-addr '(last-line)))
  (parse-file-cmd #\r))

;;
; Substitute Command
;;

(define (exec-subst editor range subst nth)
  (let* ((lst (editor-get-range editor range))
         (bre (make-bre (editor-regex editor (car subst))))
         (rep (editor-restr editor (cdr subst)))

         ;; Pair (list of replaced lines, line number of last replaced line)
         (re (fold-right (lambda (line lnum y)
                           (let* ((r (regex-replace bre rep line nth))
                                  (l (cons r (car y))))
                             (if (or (equal? r line)        ;; not modified
                                     (not (zero? (cdr y)))) ;; not last
                               (cons l (cdr y))
                               (cons l (+ lnum (count-newlines r))))))
                         '((). 0) lst (range->lines editor range))))
    (if (zero? (cdr re))
      (editor-raise "no match")
      (begin
        (editor-replace! editor range (car re))
        (editor-goto! editor (cdr re))))))

(define-command (edit-cmd exec-subst)
  (parse-default parse-addr-range (make-range))
  (parse-cmd #\s)

  ;; Returns pair (regex, replacement)
  (parse-with-context
    ;; Any character other then <space> and <newline> can be a delimiter.
    (parse-char (char-set-complement (char-set #\space #\newline)))

    ;; TODO: Refactor this into one parser combinator.
    (lambda (delim)
      (parse-map
        (parse-seq
          (parse-regex-lit delim)
          (parse-or
            (parse-map
              (parse-assert
                (parse-repeat+ (parse-not-char delim))
                (lambda (lst)
                  (equal? lst '(#\%))))
              (lambda (x) 'previous-replace))
            parse-replace)
          (parse-ignore (parse-char delim)))
        (lambda (lst) (cons (first lst) (second lst))))))

  (parse-default
    (parse-or
      (parse-map (parse-char #\g) (lambda (x) 0))
      parse-digits)
    1))

;;
; Delete Command
;;

(define (exec-delete editor range)
  (let ((saddr (addr->line editor (first range))))
    (editor-remove! editor range)
    (if (null? (text-editor-buffer editor))
      (editor-goto! editor 0)
      (editor-goto! editor (min (length (text-editor-buffer editor)) saddr)))))

(define-command (edit-cmd exec-delete)
  (parse-default parse-addr-range (make-range))
  (parse-cmd #\d))

;;
; Edit Command
;;

(define-confirm (%exec-edit exec-edit))
(define-command (file-cmd %exec-edit)
  (parse-file-cmd #\e))

;;
; Edit Without Checking Command
;;

(define (exec-edit editor filename)
  (text-editor-buffer-set! editor '())
  (text-editor-marks-set! editor '())
  (exec-read editor (make-addr '(last-line))
             (editor-filename editor filename)))

(define-command (file-cmd exec-edit)
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
      (editor-verbose editor (editor-filename editor)))))

(define-command (file-cmd exec-filename)
  (parse-file-cmd #\f))

;;
; Help Command
;;

(define (exec-help editor)
  (let ((msg (text-editor-error editor)))
    (when msg
      (println msg))))

(define-command (edit-cmd exec-help)
  (parse-cmd #\h))

;;
; Help-Mode Command
;;

(define (exec-help-mode editor)
  (let ((prev-help? (text-editor-help? editor)))
    (text-editor-help-set! editor (not prev-help?))
    (when (not prev-help?)
      (exec-help editor))))

(define-command (edit-cmd exec-help-mode)
  (parse-cmd #\H))

;;
; Insert Command
;;

(define (exec-insert editor addr)
  (let ((line (addr->line editor addr)))
    (editor-goto! editor (max (dec line) 0))
    (let* ((data (editor-read-input editor))
           (last-inserted (editor-append! editor data)))
      (editor-goto! editor last-inserted))))

(define-command (edit-cmd exec-insert)
  (parse-default parse-addr (make-addr '(current-line)))
  (parse-cmd #\i))

;;
; Join Command
;;

(define (exec-join editor range)
  (let ((start (addr->line editor (first range)))
        (end (addr->line editor (last range))))
    (unless (eqv? start end)
      (editor-join! editor range)
      (editor-goto! editor start))))

(define-command (edit-cmd exec-join)
  (parse-default parse-addr-range
                 (make-range
                   (make-addr '(current-line))
                   (make-addr '(current-line) '(1))))
  (parse-cmd #\j))

;;
; Mark Command
;;

(define (exec-mark editor addr mark)
  (editor-mark-line
    editor (addr->line editor addr) mark))

(define-command (edit-cmd exec-mark)
  (parse-default parse-addr (make-addr '(current-line)))
  (parse-cmd #\k)
  (parse-char char-set:lower-case))

;;
; List Command
;;

(define (exec-list editor range)
  (let ((lst (editor-get-range editor range))
        (end (addr->line editor (last range))))
  (for-each (lambda (line)
              (display
                (string->human-readable (string-append line "\n"))))
            lst)
  (editor-goto! editor end)))

(define-command (print-cmd exec-list)
  (parse-default parse-addr-range (make-range))
  (parse-cmd #\l))

;;
; Move Command
;;

(define (exec-move editor range addr)
  (if (editor-in-range editor range addr)
    (editor-raise "invalid move destination")
    (let ((data (editor-get-range editor range))
          (target (addr->line editor addr)))
      (exec-delete editor range)
      (editor-goto! editor (min target
                                (length (text-editor-buffer editor))))

      (let ((last-inserted (editor-append! editor data)))
        (editor-goto! editor last-inserted)))))

(define-command (edit-cmd exec-move)
  (parse-default parse-addr-range (make-range))
  (parse-cmd #\m)
  parse-addr)

;;
; Copy Command
;;

(define (exec-copy editor range addr)
  (if (editor-in-range editor range addr)
    (editor-raise "invalid copy destination")
    (let ((data (editor-get-range editor range))
          (target (addr->line editor addr)))
      (editor-goto! editor addr)
      (let ((last-inserted (editor-append! editor data)))
        (editor-goto! editor last-inserted)))))

(define-command (edit-cmd exec-copy)
  (parse-default parse-addr-range (make-range))
  (parse-cmd #\t)
  parse-addr)

;;
; Write Command
;;

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

;;
; Line Number Command
;;

(define (exec-line-number editor addr)
  (println (text-editor-line editor)))

(define-command (edit-cmd exec-line-number)
  (parse-default parse-addr (make-addr '(last-line)))
  (parse-cmd #\=))

;;
; Number Command
;;

(define (exec-number editor range)
  (let ((lst (editor-get-range editor range))
        (eline (addr->line editor (last range))))
    (for-each
      (lambda (line number)
        (println number "\t" line))
      lst (range->lines editor range))
    (editor-goto! editor eline)))

(define-command (print-cmd exec-number)
  (parse-default parse-addr-range (make-range))
  (parse-cmd #\n))

;;
; Print Command
;;

(define (exec-print editor range)
  (let ((lst (editor-get-range editor range))
        (end (addr->line editor (last range))))
  (for-each println lst)
  (editor-goto! editor end)))

(define-command (print-cmd exec-print)
  (parse-default parse-addr-range (make-range))
  (parse-cmd #\p))

;;
; Prompt Command
;;

(define (exec-prompt editor)
  (let* ((input-handler (text-editor-input-handler editor))
         (prompt? (input-handler-prompt? input-handler)))
    (input-handler-set-prompt!
      input-handler
      (not prompt?))))

(define-command (edit-cmd exec-prompt)
  (parse-cmd #\P))

;;
; Quit Command
;;

(define-confirm (%exec-quit exec-quit))
(define-command (file-cmd %exec-quit)
  (parse-cmd #\q))

;;
; Quit Without Checking Command
;;

(define (exec-quit editor)
  (exit))

(define-command (file-cmd exec-quit)
  (parse-cmd #\Q))

;;
; Null Command
;;

(define (exec-null editor addr)
  (let ((line (addr->line editor addr)))
    (if (zero? line)
      (editor-raise "invalid address")
      (begin
        (println (list-ref (text-editor-buffer editor) (dec line)))
        (editor-goto! editor line)))))

(define-command (file-cmd exec-null)
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
(define parse-cmds
  (apply parse-or command-parsers))
