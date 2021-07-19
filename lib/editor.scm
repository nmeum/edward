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
  (%make-text-editor filename buffer line)
  text-editor?
  ;; Name of the file currently being edited.
  (filename text-editor-filename text-editor-filename-set!)
  ;; List of strings representing all lines in the file.
  (buffer text-editor-buffer text-editor-buffer-set!)
  ;; Current line in the buffer.
  (line text-editor-line text-editor-line-set!))

(define (make-text-editor filename)
  (let ((e (%make-text-editor filename '() 0)))
    (unless (empty-string? filename)
      (exec-read e (make-addr '(last-line)) filename))
    e))

(define (editor-filename editor)
  (let ((fn (text-editor-filename editor)))
    (if (empty-string? fn)
      (error "no file name specified")
      fn)))

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

;; Move editor cursor to specified line/address. Line 1 is the first
;; line, specifying 0 as a line moves the cursor **before** the first
;; line.

(define (goto editor line)
  (text-editor-line-set! editor line))
(define (goto-addr editor addr)
  (goto editor (addr->line editor addr)))

(define (get-range editor range)
  (define (%get-range editor start end)
    (if (null? (text-editor-buffer editor))
      '()
      (let ((sline (addr->line editor start))
            (eline (addr->line editor end)))
        (if (zero? sline)
          (error "ranges cannot start at address zero")
          (sublist (text-editor-buffer editor) (dec sline) eline)))))

  ;; In the case of a <semicolon> separator, the current line ('.') shall
  ;; be set to the first address, and only then will the second address be
  ;; calculated. This feature can be used to determine the starting line
  ;; for forwards and backwards searches.
  (match range
    ((fst #\; snd)
     (goto-addr fst)
     (%get-range editor fst snd))
    ((fst #\, snd)
     (%get-range editor fst snd))))

(define (append-text editor text)
  (let ((buf  (text-editor-buffer editor))
        (line (text-editor-line editor)))
  (text-editor-buffer-set! editor
    (apply append
      (list
        (take buf line)
        text
        (drop buf line))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (repl prompt proc)
  (unless (empty-string? prompt)
    (display prompt)
    (flush-output-port))

  (let ((input (read-line)))
    (unless (eof-object? input)
      (proc input)
      (repl prompt proc))))

(define (start-editor prompt filename)
  (let* ((editor (make-text-editor filename))
         (eval-input (lambda (input)
                       (let* ((s (string->parse-stream input))
                              (r (parse-fully parse-cmd s)))
                         (apply (car r)
                                (cons editor (cdr r)))))))
    (repl prompt eval-input)))

(define (input-mode-read)
  (let ((input (read-line)))
    (if (eof-object? input)
      (error "unexpected EOF")
      (if (equal? input ".")
        '()
        (cons input (input-mode-read))))))
