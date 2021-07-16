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

(define (add-offset editor off)
  (let* ((total-off (apply + off))
         (buffer (text-editor-buffer editor))
         (nline (+ total-off (text-editor-line editor))))
    (if (or
          (> 0 nline)
          (> nline (length buffer)))
      (error "invalid final address value")
      (text-editor-line-set! editor nline))))

(define (goto-line editor line)
  (if (or
        (> 0 line)
        (> line (length (text-editor-buffer editor))))
    (error "invalid line value")
    (text-editor-line-set! editor line)))

(define goto-addr
  (match-lambda*
    ((e ((current-line) off))
     (add-offset e off))
    ((e ((last-line) off))
     ;; TODO: Need to reset on error
     ;;
     ;; Doing this separatly doesn't take into account that the
     ;; intermediate value maybe out of bounds but not the final one.
     (goto-line e (length (text-editor-buffer e)))
     (add-offset e off))))

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
