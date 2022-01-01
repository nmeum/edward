;; This file implements a text editor on top of a line-buffer. The
;; buffer is just a list of string where each string represents a
;; line, newlines not included.

(define (file->buffer filename)
  (define (%file->buffer port lines numbytes)
    (let ((l (read-line port)))
      (if (eof-object? l)
        (values numbytes lines)
        (%file->buffer
          port
          (append lines (list l))
          ;; inc for newline stripped by read-line
          ;; XXX: Buggy if last line is not not terminated with \n.
          (inc (+ numbytes (count-bytes l)))))))

  (call-with-input-file filename
    (lambda (port)
      (%file->buffer port '() 0))))

(define (buffer->string buffer)
  (fold-right (lambda (x ys)
                (string-append x "\n" ys))
              "" buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
      (input-handler-parse handler parse-cmds sk fk)
      (input-handler-repl handler sk fk)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type Text-Editor
  (%make-text-editor filename input buffer line error marks state re
                     lcmd replace modified? silent? help?)
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
  ;; Last command executed by the shell escape editor command or '() if none.
  (lcmd text-editor-last-cmd text-editor-last-cmd-set!)
  ;; Last used replacement for the substitute command or '() if none.
  (replace text-editor-last-replace text-editor-last-replace-set!)
  ;; Whether the editor has been modified since the last write.
  (modified? text-editor-modified? text-editor-modified-set!)
  ;; Whether the editor is in silent mode (ed -s option).
  (silent? text-editor-silent?)
  ;; Whether help mode is activated (H command).
  (help? text-editor-help? text-editor-help-set!))

(define (make-text-editor filename prompt silent?)
  (let* ((h (make-input-handler prompt))
         (e (%make-text-editor filename h '() 0 #f '() #f "" '() '() #f silent? #f)))
    (unless (empty-string? filename)
      (exec-read e (make-addr '(last-line)) filename)
      (text-editor-modified-set! e #f))
    e))

(define (handle-error editor line msg)
  (let* ((in (text-editor-input-handler editor))
         (tty? (stdin-tty?))
         (prefix (if tty?
                   ""
                   (string-append
                     "line " (number->string line) ": "))))
    (text-editor-prevcmd-set! editor #f)
    (editor-error
      editor
      (string-append prefix msg))

    ;; See "Consequences of Errors" section in POSIX.1-2008.
    (if tty?
      '()
      (exit #f))))

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
            (text-editor-prevcmd-set!
              editor
              (editor-exec editor cmd)))))))

  (input-handler-repl
    (text-editor-input-handler editor)
    execute-command
    (lambda (line reason)
      (handle-error editor line reason))))

;; Execute a text editor command. A text editor command is a list
;; where the first element is a procedure and the remaining elements are
;; arguments to that procedure.
;;
;; The procedure takes an editor record as the first argument and is
;; executed with the given editor and the given additional arguments.

(define (editor-exec editor cmd)
  (apply (car cmd) editor (cdr cmd)))

;; Execute a list of commands (e.g. a command list as used by the
;; ed global command).

(define (editor-exec-cmdlist editor cmds)
  (for-each (lambda (cmd)
              (editor-exec editor cmd))
            cmds))

;; Returns the last executed shell comand or raises an error if none.

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
    (println msg)))

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
  (if (number? target)
    ;; Target is a line number.
    (text-editor-line-set! editor target)
    ;; Else: Target is an address.
    (text-editor-line-set! editor (addr->line editor target))))

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
     (editor-goto! fst)
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
        (text-editor-buffer editor)
        (iota (length (text-editor-buffer editor)) 1))
      #f)))

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
      (editor-raise "invalid final address value")
      nline)))

(define (match-line direction editor bre)
  (let ((buffer (text-editor-buffer editor))
        (regex (make-bre (editor-regex editor bre)))
        (cont-proc (match direction
                          ('forward inc)
                          ('backward dec))))
    (call-with-current-continuation
      (lambda (exit)
        (for-each-index
          (lambda (idx elem)
            (when (bre-match? regex elem)
              (exit (inc idx))))
          cont-proc
          buffer
          ;; Forward/Backward search start at next/previous line.
          (modulo (cont-proc
                    ;; Convert line number to index.
                    (max (dec (text-editor-line editor)) 0))
                  (length (text-editor-buffer editor))))
        (editor-raise "no match")))))

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
