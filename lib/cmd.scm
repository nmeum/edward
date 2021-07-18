(define command-parsers '())
(define (register-command proc)
  (set! command-parsers (cons proc command-parsers)))

(define-syntax define-command
  (syntax-rules ()
    ((define-command (DESC HANDLER) BODY ...)
      (register-command
        (parse-map
          BODY ...
          (lambda (args) (cons HANDLER args)))))))

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
  (goto-addr editor addr)
  (append-text editor (input-mode-read))

  ;; Current line shall become the address of the last inserted line.
  (goto editor (length (text-editor-buffer editor))))

(define-command ("Append Command" exec-append)
  (parse-map
    (parse-blanks-seq
      (parse-default parse-addr (make-addr '(current-line)))
      (parse-ignore (parse-char #\a)))
    ;; XXX: For some reason parse-ignore doesn't work for the last
    ;; element in a parse-seq sequence, thus we remove it using init.
    ;;
    ;; See: https://github.com/ashinn/chibi-scheme/issues/757
    init))

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

(define (exec-read editor addr filename)
  (goto-addr editor addr)
  (let* ((f (if (empty-string? filename)
              (editor-filename editor)
              filename))
         (r (file->buffer f)))
    (unless (empty-string? (text-editor-filename editor))
      (text-editor-filename-set! editor f))

    (append-text editor (car r))
    (println (cdr r))))

(define-command ("Read Command" exec-read)
  (parse-blanks-seq
    (parse-default parse-addr (make-addr '(last-line)))
    (parse-ignore (parse-char #\r))
    (parse-string (parse-repeat parse-anything))))

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

;; TODO: Implement support for !file

(define (exec-write editor range filename)
  (let ((fn (if (empty-string? filename)
              (editor-filename editor) filename)))
    (call-with-output-file fn
      (lambda (port)
        (let ((s (buffer->string (get-range editor range))))
          (write-string s port)
          ;; Assuming write-string *always* writes all bytes.
          (println (string-length s)))))))

(define-command ("Write Command" exec-write)
  (parse-blanks-seq
    (parse-default parse-addr-range
                   (list
                     (make-addr '(nth-line . 1))
                     #\,
                     (make-addr '(last-line))))
    (parse-ignore (parse-char #\w))
    (parse-string (parse-repeat parse-anything))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define parse-cmd
  (apply parse-or command-parsers))
