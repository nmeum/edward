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

;; Parse a single line without the terminating newline.

(define parse-input-line
  (parse-map
    (parse-seq
      (parse-as-string (parse-repeat+ (parse-not-char #\newline)))
      (parse-char #\newline))
    car))

;; Parse lines until end of input mode (<period> character on single line).

(define parse-input
  (parse-repeat
    (parse-with-context
      parse-input-line
      (lambda (line)
        (lambda (source index sk fk)
          (if (equal? line ".")
            (fk source index "end of input mode")
            (sk line source index fk)))))))

;; Return a list of input mode input lines (without the terminating
;; newline). See POSIX.1-2008 for more information on input mode.

(define parse-input-mode
  (parse-map
    (parse-seq
      parse-input
      (parse-string ".\n"))
    car))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type Input-Handler
  (%make-input-handler prompt-str prompt? stream index)
  input-handler?
  ;; Prompt string used for input prompt.
  (prompt-str input-handler-prompt-str)
  ;; Whether the prompt should be shown or hidden.
  (prompt? input-handler-prompt? input-handler-set-prompt!)
  ;; Parse stream used for the parser combinator.
  (stream input-handler-stream)
  ;; Last index in parse stream.
  (index input-handler-index input-handler-index-set!))

(define (make-input-handler prompt)
  (let ((prompt? (not (empty-string? prompt))))
    (%make-input-handler
      (if prompt? prompt "*")
      prompt?
      (make-parse-stream "stdin" (current-input-port))
      0)))

(define (input-handler-next-line! handler)
  (let ((idx (input-handler-index handler))
        (stream (input-handler-stream handler)))
    (input-handler-index-set! handler (inc idx))
    (unless (eqv? (parse-stream-ref stream idx) #\newline)
      (input-handler-next-line! handler))))

(define (input-handler-parse handler f)
  (call-with-parse
    f
    (input-handler-stream handler)
    (input-handler-index handler)
    (lambda (r s i fk)
      (input-handler-index-set! handler i)
      r)
    (lambda (s i reason)
      (input-handler-next-line! handler)
      (error reason))))

(define (input-handler-line handler)
  (car
    (parse-stream-debug-info
      (input-handler-stream handler)
      (input-handler-index handler))))

(define (input-handler-repl handler proc)
  (if (parse-stream-end?
            (input-handler-stream handler)
            (input-handler-index handler))
    (exit #t) ;; EOF
    (begin
      (when (input-handler-prompt? handler)
        (display (input-handler-prompt-str handler))
        (flush-output-port))

      (let ((c (input-handler-parse handler parse-cmds)))
        (proc c)
        (input-handler-repl handler proc)))))

(define (input-handler-read handler)
  (input-handler-parse handler parse-input-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type Text-Editor
  (%make-text-editor filename input buffer line error marks state re
                     replace modified? silent? help?)
  text-editor?
  ;; Name of the file currently being edited.
  (filename text-editor-filename text-editor-filename-set!)
  ;; Input handler for this text editor.
  (input text-editor-input-handler)
  ;; List of strings representing all lines in the file.
  (buffer text-editor-buffer text-editor-buffer-set!)
  ;; Current line in the buffer.
  (line text-editor-line text-editor-line-set!)
  ;; Last error message encountered (for h and H command).
  (error text-editor-error text-editor-error-set!)
  ;; Assoc lists of marks for this editor.
  ;; XXX: Since data is never deleted from an assoc list this leaks memory.
  (marks text-editor-marks text-editor-marks-set!)
  ;; Symbol with previous handler name executed by the editor or #f if none.
  (state text-editor-prevcmd text-editor-prevcmd-set!)
  ;; String representing last encountered RE.
  (re text-editor-re text-editor-re-set!)
  ;; String representing last used regex for the substitute command.
  (replace text-editor-last-replace text-editor-last-replace-set!)
  ;; Whether the editor has been modified since the last write.
  (modified? text-editor-modified? text-editor-modified-set!)
  ;; Whether the editor is in silent mode (ed -s option).
  (silent? text-editor-silent?)
  ;; Whether help mode is activated (H command).
  (help? text-editor-help? text-editor-help-set!))

(define (make-text-editor filename prompt silent?)
  (let* ((h (make-input-handler prompt))
         (e (%make-text-editor filename h '() 0 #f '() #f "" "" #f silent? #f)))
    (unless (empty-string? filename)
      (exec-read e (make-addr '(last-line)) filename)
      (text-editor-modified-set! e #f))
    e))

(define (editor-start editor)
  (define (handle-cmd cmd)
    (text-editor-prevcmd-set!
      editor
      (apply (car cmd) editor (cdr cmd))))

  (call-with-current-continuation
    (lambda (k)
      (with-exception-handler
        (lambda (eobj)
          (let* ((in (text-editor-input-handler editor))
                 (line (input-handler-line in))

                 (tty? (stdin-tty?))
                 (prefix (if tty?
                           ""
                           (string-append
                             "line " (number->string line) ": "))))
          (text-editor-prevcmd-set! editor #f)
          (editor-error
            editor
            (string-append prefix (error-object-message eobj)))

          ;; See "Consequences of Errors" section in POSIX.1-2008.
          (if tty?
            (k '())
            (exit #f))))
        (lambda ()
          (input-handler-repl
            (text-editor-input-handler editor)
            handle-cmd)))))

  (editor-start editor))

(define (editor-read-input editor)
  (let ((h (text-editor-input-handler editor)))
    (input-handler-read h)))

;; Returns the last RE encountered or the given bre string if it is not
;; empty. If it is empty and there is no last RE an error is raised.

(define (editor-regex editor bre)
  (if (empty-string? bre)
    (let ((last-re (text-editor-re editor)))
      (if (empty-string? last-re)
        (error "no previous pattern")
        last-re))
    (begin
      (text-editor-re-set! editor bre)
      bre)))

(define (editor-replace editor subst)
  (if (equal? subst "%")
    (let ((last-subst (text-editor-last-replace editor)))
      (if (empty-string? last-subst)
        (error "no previous replacement")
        last-subst))
    (begin
      (text-editor-last-replace-set! editor subst)
      subst)))

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
    (apply println objs)))

;; Print `?` followed by msg (if the editor is in help mode).
;; Also set the current editor error accordingly (if any).

(define (editor-error editor msg)
  (text-editor-error-set! editor msg)
  (println "?")
  (when (text-editor-help? editor)
    (println msg)))

(define (editor-mark-line editor line mark)
  (text-editor-marks-set! editor
    (alist-cons mark line (text-editor-marks editor))))

(define (editor-get-mark editor mark)
  (cdr (assv mark (text-editor-marks editor))))

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
  (text-editor-modified-set! editor #t)
  (let ((buf  (text-editor-buffer editor))
        (line (text-editor-line editor)))
    (text-editor-buffer-set! editor
                             (append
                               (take buf line)
                               text
                               (drop buf line)))
    (+ line (length text))))

;; Replace text in given range with given data. Return line number of
;; last inserted line.

(define (editor-replace! editor range data)
  (let ((saddr (addr->line editor (first range))))
    (editor-remove! editor range)
    (editor-goto! editor (max 0 (dec saddr)))
    (editor-append! editor data)))

(define (editor-join! editor range)
  (text-editor-modified-set! editor #t)
  (let-values (((sline eline) (editor-range editor range))
               ((buffer) (text-editor-buffer editor)))
    (text-editor-buffer-set! editor
      (append
        (take buffer (dec sline))
        (list (apply string-append (sublist buffer (dec sline) eline)))
        (drop buffer eline)))))

(define (editor-remove! editor range)
  (text-editor-modified-set! editor #t)
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

(define (match-line direction editor bre)
  (let ((buffer (text-editor-buffer editor))
        (regex (make-bre (editor-regex editor bre)))
        (func  (match direction
                      ('forward for-each-index)
                      ('backward for-each-index-right))))
    (call-with-current-continuation
      (lambda (exit)
        (func (lambda (idx elem)
                (when (bre-match? regex elem)
                  (exit (inc idx))))
              buffer
              (max (dec (text-editor-line editor)) 0))

        (error "no match")))))

(define addr->line
  (match-lambda*
    ((e ('(current-line) off))
     (%addr->line e off (text-editor-line e)))
    ((e ('(last-line) off))
     (%addr->line e off (length (text-editor-buffer e))))
    ((e (('nth-line . line) off))
     (%addr->line e off line))
    ((e (('marked-line . mark) off))
     (%addr->line e off (editor-get-mark e mark)))
    ((e (('regex-forward . bre) off))
     (%addr->line e off (match-line 'forward e bre)))
    ((e (('regex-backward . bre) off))
     (%addr->line e off (match-line 'backward e bre)))
    ((e (('relative . rel) off))
     (%addr->line e off (+ (text-editor-line e) rel)))))

;; Return list of line numbers for the given range.

(define (range->lines editor range)
  (let ((sline (addr->line editor (first range)))
        (eline (addr->line editor (last range))))
    (iota (inc (- eline sline)) sline)))
