(define-record-type Text-Editor
  (%make-text-editor filename buffer line column)
  text-editor?
  ;; Name of the file currently being edited.
  (filename text-editor-filename)
  ;; List of strings representing all lines in the file.
  (buffer text-editor-buffer text-editor-buffer-set!)
  ;; Current line in the buffer.
  (line text-editor-line text-editor-line-set!)
  ;; Current column line in the buffer.
  (column text-editor-column))

(define (make-text-editor filename)
  (call-with-input-file filename
    (lambda (port)
      (letrec ((read-all (lambda ()
                           (let ((l (read-line port)))
                             (if (eof-object? l)
                               '()
                               (cons l (read-all)))))))
        (%make-text-editor filename (read-all) 0 0)))))

(define (%goto editor off line)
  (let* ((buffer (text-editor-buffer editor))
         (total-off (apply + off))
         (nline (+ total-off line (text-editor-line editor))))
    (if (or
          (> 0 nline)
          (> nline (length buffer)))
      (error "invalid final address value")
      (text-editor-line-set! editor nline))))

(define goto
  (case-lambda
    ((editor off)
     (%goto editor off (text-editor-line editor)))
    ((editor off line)
     (%goto editor off line))))

(define goto-addr
  (match-lambda*
    ((e ((current-line) off))
     (goto e off))
    ((e ((last-line) off))
     (goto e off (length (text-editor-buffer e))))))

(define (append-text editor text)
  (let ((buf  (text-editor-buffer editor))
        (line (text-editor-line editor)))
  (text-editor-buffer-set! editor
    (list
      (take buf line)
      text
      (drop buf line)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (repl prompt proc)
  (display prompt)
  (flush-output-port)
  (let ((input (read-line)))
    (unless (eof-object? input)
      (proc input)
      (repl prompt proc))))

(define (start-editor filename)
  (let* ((editor (make-text-editor filename))
         (eval-input (lambda (input)
                       (let* ((s (string->parse-stream input))
                              (r (parse-fully parse-cmd s))
                              (handler (car r))
                              (args    (cadr r)))
                         (handler editor args)))))
    (repl ": " eval-input)))

(define (input-mode-read)
  (let ((input (read-line)))
    (if (eof-object? input)
      (error "unexpected EOF")
      (if (equal? input ".")
        '()
        (cons input (input-mode-read))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (handle-append editor addr)
  (goto-addr editor addr)
  (let ((text (input-mode-read)))
    (append-text editor text)))
    ;; TODO: Update current line
