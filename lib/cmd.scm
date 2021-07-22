(define command-parsers (list (parse-fail "Unknown command")))
(define (register-command proc)
  (set! command-parsers (cons proc command-parsers)))

(define-syntax define-command
  (syntax-rules ()
    ((define-command (DESC HANDLER) BODY ...)
      (register-command
        (parse-map
          (parse-blanks-seq
            BODY ...)
          (lambda (args) (cons HANDLER args)))))))

(define (parse-cmd ch)
  (parse-ignore (parse-char ch)))

;; TODO: Implement support for !file

(define parse-filename
  (parse-string
    (parse-repeat (parse-not-char char-set:blank))))

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
  (let ((last-inserted (editor-append! editor (input-mode-read))))
    (editor-goto! editor last-inserted)))

(define-command ("Append Command" exec-append)
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
  (let ((saddr (addr->line editor (first range))))
    (editor-remove! editor range)
    (editor-goto! editor (max 0 (dec saddr)))

    (let ((in (input-mode-read)))
      (if (null? in)
        (editor-goto! editor (min (length (text-editor-buffer editor)) saddr))
        (editor-goto! editor (editor-append! editor in))))))

(define-command ("Change Command" exec-change)
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
         (r (file->buffer f)))
    (if (empty-string? (text-editor-filename editor))
      (text-editor-filename-set! editor f))

    (editor-append! editor (car r))
    (editor-goto! editor (length (text-editor-buffer editor)))

    (editor-println editor (cdr r))))

(define-command ("Read Command" exec-read)
  (parse-default parse-addr (make-addr '(last-line)))
  (parse-cmd #\r)
  parse-filename)

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

(define-command ("Delete Command" exec-delete)
  (parse-default parse-addr-range
                 (list
                   (make-addr '(current-line))
                   #\,
                   (make-addr '(current-line))))
  (parse-cmd #\d))

;; Edit Without Checking Command
;;
;;   E [file]
;;
;; The E command shall possess all properties and restrictions of the e
;; command except that the editor shall not check to see whether any
;; changes have been made to the buffer since the last w command.

(define (exec-edit editor filename)
  (text-editor-buffer-set! editor '())
  (exec-read editor (make-addr '(last-line)) filename))

(define-command ("Edit Without Checking Command" exec-edit)
  (parse-cmd #\E)
  parse-filename)

;; Filename Command
;;
;;   f [file]
;;
;; If file is given, the f command shall change the currently remembered
;; pathname to file; whether the name is changed or not, it shall then
;; write the (possibly new) currently remembered pathname to the
;; standard output.

(define (exec-filename editor filename)
  (unless (empty-string? filename)
    (text-editor-filename-set! editor filename))
  (editor-println editor (editor-filename editor)))

(define-command ("Filename Command" exec-filename)
  (parse-cmd #\f)
  parse-filename)

;; Help Command
;;
;;   h
;;
;; The h command shall write a short message to standard output that
;; explains the reason for the most recent '?' notification. The current
;; line number shall be unchanged.

(define (exec-help editor)
  (let ((e (text-editor-error editor)))
    (when e
      (display-error e))))

(define-command ("Help Command" exec-help)
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

(define-command ("Help-Mode Command" exec-help-mode)
  (parse-cmd #\H))

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
  (let ((fn (editor-filename editor filename)))
    (call-with-output-file fn
      (lambda (port)
        (let ((s (buffer->string (editor-get-range editor range))))
          (write-string s port)
          ;; Assuming write-string *always* writes all bytes.
          (editor-println editor (string-length s)))))))

(define-command ("Write Command" exec-write)
  (parse-default parse-addr-range
                 (list
                   (make-addr '(nth-line . 1))
                   #\,
                   (make-addr '(last-line))))
  (parse-cmd #\w)
  parse-filename)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define parse-cmd
  ;; Strip trailing blanks after parsed command.
  (parse-map
    (parse-seq
      (apply parse-or command-parsers)
      parse-blanks)
    car))
