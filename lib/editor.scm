(define-record-type Text-Editor
  (%make-text-editor filename buffer line)
  text-editor?
  ;; Name of the file currently being edited.
  (filename text-editor-filename)
  ;; List of strings representing all lines in the file.
  (buffer text-editor-buffer text-editor-buffer-set!)
  ;; Current line in the buffer.
  (line text-editor-line text-editor-line-set!))

(define (make-text-editor filename)
  (call-with-input-file filename
    (lambda (port)
      (letrec ((read-all (lambda ()
                           (let ((l (read-line port)))
                             (if (eof-object? l)
                               '()
                               (cons l (read-all)))))))
        (%make-text-editor filename (read-all) 0)))))

(define (%addr->line editor off line)
  (let* ((buffer (text-editor-buffer editor))
         (total-off (apply + off))
         (nline (+ total-off line (text-editor-line editor))))
    (if (or
          (> 0 nline)
          (> nline (length buffer)))
      (error "invalid final address value")
      nline)))

(define addr->line
  (match-lambda*
    ((e (current-line off))
     (%addr->line e off (text-editor-line e)))
    ((e  ((last-line off)))
     (%addr->line e off (length (text-editor-buffer e))))))

(define (goto editor line)
  (text-editor-line-set! editor line))
(define (goto-addr editor addr)
  (goto editor (addr->line editor addr)))

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
                              (r (parse-fully parse-cmd s)))
                         (apply (car r)
                                (cons editor (cdr r)))))))
    (repl ": " eval-input)))

(define (input-mode-read)
  (let ((input (read-line)))
    (if (eof-object? input)
      (error "unexpected EOF")
      (if (equal? input ".")
        '()
        (cons input (input-mode-read))))))
