(define-record-type Text-Editor
  (%make-text-editor filename buffer line column)
  text-editor?
  ;; Name of the file currently being edited.
  (filename text-editor-filename)
  ;; List of strings representing all lines in the file.
  (buffer text-editor-buffer)
  ;; Current line in the buffer.
  (line text-editor-line)
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
        (%make-text-editor filename (read-all) 1 1)))))

(define handle-append
  (case-lambda
    ((addr)
     (begin
       (display "appending text at ") (display addr) (newline)))))
