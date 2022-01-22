(define-record-type Editor-Irritant
  (make-editor-irritant)
  editor-irritant?)

(define (editor-error-object? eobj)
  (let ((irritants (error-object-irritants eobj)))
    (if (and (list? irritants) (not (null? irritants)))
      (editor-irritant? (car irritants))
      #f)))

(define (editor-raise msg)
  (error msg (make-editor-irritant)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type Input-Handler
  (%make-input-handler prompt-str prompt? stream index)
  input-handler?
  ;; Prompt string used for input prompt.
  (prompt-str input-handler-prompt-str)
  ;; Whether the prompt should be shown or hidden.
  (prompt? input-handler-prompt? input-handler-set-prompt!)
  ;; Parse stream used for the parser combinator.
  (stream input-handler-stream input-handler-stream-set!)
  ;; Last index in parse stream.
  (index input-handler-index input-handler-index-set!))

(define (make-input-handler prompt)
  (let ((prompt? (not (empty-string? prompt))))
    (%make-input-handler
      (if prompt? prompt "*")
      prompt?
      (make-parse-stream "stdin" (current-input-port))
      0)))

(define (input-handler-state-set! handler source index)
  (input-handler-stream-set! handler source)
  (input-handler-index-set! handler index))

(define (input-handler-parse handler f sk fk)
  (define (stream-next-line source idx)
    (let* ((next-index  (parse-stream-next-index source idx))
           (next-source (parse-stream-next-source source idx)))
      (if (eqv? (parse-stream-ref source idx) #\newline)
        (cons next-source next-index) ;; first index after newline
        (stream-next-line
          next-source
          next-index))))

  (call-with-parse f
    (input-handler-stream handler)
    (input-handler-index handler)
    (lambda (r s i fk)
      (input-handler-state-set! handler s i)
      (sk (input-handler-line handler i) r))
    (lambda (s i reason)
      (let ((line (input-handler-line handler i))
            (next (stream-next-line (input-handler-stream handler) i)))
        (input-handler-state-set! handler (car next) (cdr next))
        (fk line reason)))))

(define (input-handler-line handler index)
  ;; Surprisingly, (chibi parse) line numbers start at zero.
  (inc
    (car
      (parse-stream-debug-info
        (input-handler-stream handler)
        index))))

(define (input-handler-repl handler sk fk)
  (when (input-handler-prompt? handler)
    (display (input-handler-prompt-str handler))
    (flush-output-port))

  (unless (parse-stream-end?
            (input-handler-stream handler)
            (input-handler-index handler))
      (input-handler-parse handler parse-cmd sk fk)
      (input-handler-repl handler sk fk)))

(define (input-handler-interactive handler)
  (input-handler-parse
    handler
    parse-interactive-cmd
    (lambda (line value) value)
    (lambda (line reason)
      (editor-raise "parsing of interactive command failed"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type Editor-Command
  (make-cmd symbol proc args)
  editor-cmd?
  (symbol cmd-symbol)
  (proc cmd-proc)
  (args cmd-args))

(define (cmd-reversible? cmd)
  (member (cmd-symbol cmd)
          '(append change delete global insert join move
            read substitute copy global-unmatched interactive
            interactive-unmatched)))

(define (editor-exec editor cmd)
  (apply (cmd-proc cmd) editor (cmd-args cmd)))

(define (editor-exec-cmdlist editor cmds)
  (for-each (lambda (cmd)
              (editor-exec editor cmd))
            cmds))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type Text-Editor
  (%make-text-editor filename input buffer line last-line error marks state re
                     lcmd replace modified? last-modified? silent? help?)
  text-editor?
  ;; Name of the file currently being edited.
  (filename text-editor-filename text-editor-filename-set!)
  ;; Input handler for this text editor.
  (input text-editor-input-handler)
  ;; List of strings representing all lines in the file.
  (buffer text-editor-buffer text-editor-buffer-set!)
  ;; Current line in the buffer.
  (line text-editor-line text-editor-line-set!)
  ;; Previous line in the buffer.
  (last-line text-editor-last-line text-editor-last-line-set!)
  ;; Last error message encountered (for h and H command).
  (error text-editor-error text-editor-error-set!)
  ;; Assoc lists of marks for this editor.
  ;; XXX: Since data is never deleted from an assoc list this leaks memory.
  (marks text-editor-marks text-editor-marks-set!)
  ;; Symbol with previous handler name executed by the editor or #f if none.
  (state text-editor-prevcmd text-editor-prevcmd-set!)
  ;; String representing last encountered RE.
  (re text-editor-re text-editor-re-set!)
  ;; Last command executed by the shell escape editor command or '() if none.
  (lcmd text-editor-last-cmd text-editor-last-cmd-set!)
  ;; Last used replacement for the substitute command or '() if none.
  (replace text-editor-last-replace text-editor-last-replace-set!)
  ;; Whether the editor has been modified since the last write.
  (modified? text-editor-modified? text-editor-modified-set!)
  ;; Whether the editor has been modified before the last command was executed.
  (last-modified? text-editor-last-modified?  text-editor-last-modified-set!)
  ;; Whether the editor is in silent mode (ed -s option).
  (silent? text-editor-silent?)
  ;; Whether help mode is activated (H command).
  (help? text-editor-help? text-editor-help-set!))

(define (make-text-editor filename prompt silent?)
  (let* ((h (make-input-handler prompt))
         (e (%make-text-editor filename h (make-buffer) 0 0 #f '() #f "" '() '() #f #f silent? #f)))
    (unless (empty-string? filename)
      ;; XXX: Don't print `?` if file doesn't exist.
      (exec-edit e filename))
    e))

(define (handle-error editor line msg)
  (let* ((in (text-editor-input-handler editor))
         (prefix (if (stdin-tty?)
                   ""
                   (string-append
                     "line " (number->string line) ": "))))
    (text-editor-prevcmd-set! editor #f)
    (editor-error
      editor
      (string-append prefix msg))))

(define (editor-start editor)
  (define (execute-command line cmd)
    (call-with-current-continuation
      (lambda (k)
        (with-exception-handler
          (lambda (eobj)
            (if (editor-error-object? eobj)
              (k (handle-error editor line (error-object-message eobj)))
              (raise eobj)))
          (lambda ()
            (editor-exec editor cmd)
            (text-editor-prevcmd-set! editor (cmd-symbol cmd)))))))

  (input-handler-repl
    (text-editor-input-handler editor)
    (lambda (line cmd)
      (when (cmd-reversible? cmd)
        (editor-snapshot editor))
      (execute-command line cmd))
    (lambda (line reason)
      (handle-error editor line reason))))

(define (editor-interactive editor)
  (let ((h (text-editor-input-handler editor)))
    (input-handler-interactive h)))

;; Returns the last executed shell command or raises an error if none.

(define (editor-shell-cmd editor)
  (let ((lcmd (text-editor-last-cmd editor)))
    (if (null? lcmd)
      (editor-raise "no previous command")
      lcmd)))

;; Returns the last RE encountered or the given bre string if it is not
;; empty. If it is empty and there is no last RE an error is raised.

(define (editor-regex editor bre)
  (if (empty-string? bre)
    (let ((last-re (text-editor-re editor)))
      (if (empty-string? last-re)
        (editor-raise "no previous pattern")
        last-re))
    (begin
      (text-editor-re-set! editor bre)
      bre)))

(define (editor-restr editor subst)
  (if (equal? subst 'previous-replace)
    (let ((last-subst (text-editor-last-replace editor)))
      (if (null? last-subst)
        (editor-raise "no previous replacement")
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
      (editor-raise "no file name specified")
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
    (println msg))

  ;; See "Consequences of Errors" section in POSIX.1-2008.
  (unless (stdin-tty?)
    (exit #f)))

;; Reset all file-specific state in the editor.

(define (editor-reset! editor)
  (text-editor-buffer-set! editor (make-buffer))
  (text-editor-marks-set! editor '()))

(define (editor-mark-line editor line mark)
  (text-editor-marks-set! editor
    (alist-cons mark line (text-editor-marks editor))))

(define (editor-get-mark editor mark)
  (let ((pair (assv mark (text-editor-marks editor))))
    (if pair
      (cdr pair)
      (editor-raise (string-append "unknown mark: " (string mark))))))

;; Move editor cursor to specified line/address. Line 1 is the first
;; line, specifying 0 as a line moves the cursor **before** the first
;; line. Target can either be a line number or an address.

(define (editor-goto! editor target)
  (text-editor-line-set! editor (->line editor target)))

(define (editor-range editor range)
  (define (%editor-range editor start end)
    (let ((sline (addr->line editor start))
          (eline (addr->line editor end)))
      (cond
        ((zero? sline)   (editor-raise "ranges cannot start at address zero"))
        ((> sline eline) (editor-raise "invalid range specification"))
        (else (values sline eline)))))

  ;; In the case of a <semicolon> separator, the current line ('.') shall
  ;; be set to the first address, and only then will the second address be
  ;; calculated. This feature can be used to determine the starting line
  ;; for forwards and backwards searches.
  (match range
    ((fst #\; snd)
     (editor-goto! editor fst)
     (%editor-range editor fst snd))
    ((fst #\, snd)
     (%editor-range editor fst snd))))

;; Find current line number for a given line in the editor buffer. False
;; is returned if the line does not exist in the editor buffer.
;;
;; XXX: This implementation assumes that eq? performs pointer comparision,
;; which is the case with CHICKEN but technically this is undefinied behaviour.

(define (editor-get-lnum editor line)
  (call-with-current-continuation
    (lambda (exit)
      (for-each
        (lambda (l num)
          (if (eq? line l)
            (exit num)))
        (buffer->list (text-editor-buffer editor))
        (iota (editor-lines editor) 1))
      #f)))

(define (editor-get-range editor range)
  (if (buffer-empty? (text-editor-buffer editor))
    '()
    (let-values (((sline eline) (editor-range editor range))
                 ((list) (buffer->list (text-editor-buffer editor))))
      (sublist list (dec sline) eline))))

(define (editor-in-range editor range addr)
  (let-values (((sline eline) (editor-range editor range))
               ((line) (addr->line editor addr)))
    (and (>= line sline) (< line eline))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Prepare execution of a new undoable operation.

(define (editor-snapshot editor)
  (text-editor-last-modified-set!
    editor
    (text-editor-modified? editor))

  (text-editor-last-line-set!
    editor
    (text-editor-line editor))

  (buffer-snapshot (text-editor-buffer editor)))

;; Undo the last operation on the buffer.

(define (editor-undo! editor)
  (text-editor-modified-set!
    editor
    (text-editor-last-modified? editor))

  (let ((cur-line (text-editor-line editor)))
    (text-editor-line-set!
      editor
      (text-editor-last-line editor))
    (text-editor-last-line-set! editor cur-line))
  (buffer-undo! (text-editor-buffer editor)))

;; Returns amount of lines in the buffer.

(define (editor-lines editor)
  (buffer-length (text-editor-buffer editor)))

;; Append the text at the current address, return line number
;; of last inserted line.

(define (editor-append! editor addr text)
  (text-editor-modified-set! editor #t)
  (let ((buf  (text-editor-buffer editor))
        (line (addr->line editor addr)))
    (buffer-append! buf line text)
    (+ line (length text))))

;; Replace text in given range with given data. Return line number of
;; last inserted line.

(define (editor-replace! editor range data)
  (text-editor-modified-set! editor #t)
  (let-values (((sline eline) (editor-range editor range))
               ((buffer) (text-editor-buffer editor)))
    (buffer-replace! buffer sline eline data)
    (+ (max 0 (dec sline)) (length data))))

;; Join lines in given range to single line. Return value is undefined.

(define (editor-join! editor range)
  (text-editor-modified-set! editor #t)
  (let-values (((sline eline) (editor-range editor range))
               ((buffer) (text-editor-buffer editor)))
    (buffer-join! buffer sline eline)))

;; Remove lines in given range. Return value is undefined.

(define (editor-remove! editor range)
  (text-editor-modified-set! editor #t)
  (let-values (((sline eline) (editor-range editor range))
               ((buffer) (text-editor-buffer editor)))
    (buffer-remove! buffer sline eline)))

;; Move lines in given range to given address. Returns
;; the address of the last inserted line.

(define (editor-move! editor range addr)
  (text-editor-modified-set! editor #t)
  (let-values (((sline eline) (editor-range editor range))
               ((target) (addr->line editor addr))
               ((buffer) (text-editor-buffer editor)))
    (buffer-move! buffer sline eline target)
    (min
      (editor-lines editor)
      (+ target (inc (- eline sline))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (%addr->line editor off line)
  (let* ((total-off (apply + off))
         (nline (+ total-off line)))
    (if (or
          (> 0 nline)
          (> nline (editor-lines editor)))
      (editor-raise "invalid final address value")
      nline)))

(define (match-line direction editor bre)
  (let ((lines (buffer->list (text-editor-buffer editor)))
        (regex (make-bre (editor-regex editor bre)))
        (cont-proc (match direction
                          ('forward inc)
                          ('backward dec))))
    (call-with-current-continuation
      (lambda (exit)
        (unless (zero? (editor-lines editor))
          (for-each-index
            (lambda (idx elem)
              (when (bre-match? regex elem)
                (exit (inc idx))))
            cont-proc
            lines
            ;; Forward/Backward search start at next/previous line.
            (modulo (cont-proc
                      ;; Convert line number to index.
                      (max (dec (text-editor-line editor)) 0))
                    (editor-lines editor))))
        (editor-raise "no match")))))

(define addr->line
  (match-lambda*
    ((e ('(current-line) off))
     (%addr->line e off (text-editor-line e)))
    ((e ('(last-line) off))
     (%addr->line e off (editor-lines e)))
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

(define (->line editor obj)
  (if (number? obj)
    ;; obj already is a line number.
    obj
    ;; else: assume obj is an address.
    (addr->line editor obj)))

;; Return list of line numbers for the given range.

(define (range->lines editor range)
  (let ((sline (addr->line editor (first range)))
        (eline (addr->line editor (last range))))
    (iota (inc (- eline sline)) sline)))
