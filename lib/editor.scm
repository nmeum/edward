;; This file implements a text editor on top of a line-buffer. The
;; buffer is just a list of string where each string represents a
;; line, newlines not included.

(define (file->buffer filename)
  (define (%file->buffer port numbytes)
    (let ((l (read-line port)))
      (if (eof-object? l)
        (list numbytes)
        (cons l
              (%file->buffer port
                ;; inc for newline stripped by read-line
                ;; XXX: Buggy if last line is not not terminated with \n.
                (inc (+ numbytes (string-length l))))))))

  (call-with-input-file filename
    (lambda (port)
      (let ((r (%file->buffer port 0)))
        (if (eqv? (length r) 1)
          (cons '() (car r))
          (cons (init r) (last r)))))))

(define (buffer->string buffer)
  (fold-right (lambda (x ys)
                (string-append x "\n" ys))
              "" buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type Text-Editor
  (%make-text-editor filename buffer line error silent? help?)
  text-editor?
  ;; Name of the file currently being edited.
  (filename text-editor-filename text-editor-filename-set!)
  ;; List of strings representing all lines in the file.
  (buffer text-editor-buffer text-editor-buffer-set!)
  ;; Current line in the buffer.
  (line text-editor-line text-editor-line-set!)
  ;; Last error object encountered (for h and H command).
  (error text-editor-error text-editor-error-set!)
  ;; Whether the editor is in silent mode (ed -s option).
  (silent? text-editor-silent?)
  ;; Whether help mode is activated (H command).
  (help? text-editor-help? text-editor-help-set!))

(define (make-text-editor filename silent?)
  (let ((e (%make-text-editor filename '() 0 #f silent? #f)))
    (unless (empty-string? filename)
      (exec-read e (make-addr '(last-line)) filename))
    e))

(define (editor-start editor prompt)
  ;; If an invalid command is entered, ed shall write the string: "?\n"
  ;; (followed by an explanatory message if help mode has been enabled
  ;; via the H command) to standard output and shall continue in command
  ;; mode with the current line number unchanged.
  (define (eval-input input)
    (call-with-current-continuation
      (lambda (k)
        (with-exception-handler
          (lambda (eobj)
            (println "?")
            (text-editor-error-set! editor eobj)
            (when (text-editor-help? editor)
              (display-error eobj))
            (k '()))
          (lambda ()
            (let* ((s (string->parse-stream input))
                   (r (parse-fully parse-cmd s)))
              (apply (car r)
                     editor (cdr r))))))))

  (repl prompt eval-input))

;; Return the currently configured filename, if no default is given it
;; is an error if no filename is configured for the given editor.

(define editor-filename
  (case-lambda
    ((editor) (%editor-filename editor))
    ((editor default)
     (if (empty-string? default)
       (%editor-filename editor)
       default))))

(define (%editor-filename editor)
  (let ((fn (text-editor-filename editor)))
    (if (empty-string? fn)
      (error "no file name specified")
      fn)))

;; Print objs, but only if the editor is not in silent mode.

(define (editor-verbose editor . objs)
  (unless (text-editor-silent? editor)
    (apply fprintln (current-output-port) objs)))

;; Move editor cursor to specified line/address. Line 1 is the first
;; line, specifying 0 as a line moves the cursor **before** the first
;; line. Target can either be a line number or an address.

(define (editor-goto! editor target)
  (if (number? target)
    ;; Target is a line number.
    (text-editor-line-set! editor target)
    ;; Else: Target is an address.
    (text-editor-line-set! editor (addr->line editor target))))

(define (editor-range editor range)
  (define (%editor-range editor start end)
    (let ((sline (addr->line editor start))
          (eline (addr->line editor end)))
      (if (zero? sline)
        (error "ranges cannot start at address zero")
        (values sline eline))))

  ;; In the case of a <semicolon> separator, the current line ('.') shall
  ;; be set to the first address, and only then will the second address be
  ;; calculated. This feature can be used to determine the starting line
  ;; for forwards and backwards searches.
  (match range
    ((fst #\; snd)
     (editor-goto! fst)
     (%editor-range editor fst snd))
    ((fst #\, snd)
     (%editor-range editor fst snd))))

(define (editor-get-range editor range)
  (if (null? (text-editor-buffer editor))
    '()
    (let-values (((sline eline) (editor-range editor range)))
      (sublist (text-editor-buffer editor)
               (dec sline) eline))))

(define (editor-in-range editor range addr)
  (let-values (((sline eline) (editor-range editor range))
               ((line) (addr->line editor addr)))
    (and (>= line sline) (< line eline))))

;; Append the text at the current address, return line number
;; of last inserted line.

(define (editor-append! editor text)
  (let ((buf  (text-editor-buffer editor))
        (line (text-editor-line editor)))
    (text-editor-buffer-set! editor
                             (append
                               (take buf line)
                               text
                               (drop buf line)))
    (+ line (length text))))

(define (editor-join! editor range)
  (let-values (((sline eline) (editor-range editor range))
               ((buffer) (text-editor-buffer editor)))
    (text-editor-buffer-set! editor
      (append
        (take buffer (dec sline))
        (list (apply string-append (sublist buffer (dec sline) eline)))
        (drop buffer eline)))))

(define (editor-remove! editor range)
  (let-values (((sline eline) (editor-range editor range))
               ((buffer) (text-editor-buffer editor)))
    (text-editor-buffer-set! editor
      (append
        (sublist buffer 0 (dec sline))
        (sublist buffer eline (length buffer))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (%addr->line editor off line)
  (let* ((buffer (text-editor-buffer editor))
         (total-off (apply + off))
         (nline (+ total-off line)))
    (if (or
          (> 0 nline)
          (> nline (length buffer)))
      (error "invalid final address value")
      nline)))

(define addr->line
  (match-lambda*
    ((e ('(current-line) off))
     (%addr->line e off (text-editor-line e)))
    ((e ('(last-line) off))
     (%addr->line e off (length (text-editor-buffer e))))
    ((e (('nth-line . line) off))
     (%addr->line e off line))
    ;; TODO: marked-line
    ;; TODO: regex-forward
    ;; TODO: regex-backward
    ((e (('relative . rel) off))
     (%addr->line e off (+ (text-editor-line e) rel)))))

(define (input-mode-read)
  (let ((input (read-line)))
    (if (eof-object? input)
      (error "unexpected EOF")
      (if (equal? input ".")
        '()
        (cons input (input-mode-read))))))
