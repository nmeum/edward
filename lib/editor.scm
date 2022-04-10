;; Editor-Error is a custom object raised to indicate a non-fatal
;; error condition handled according to the ed(1) POSIX specification.

(define-record-type Editor-Error
  (make-editor-error msg)
  editor-error?
  (msg editor-error-msg))

(define (editor-raise msg)
  (raise (make-editor-error msg)))

;; Editor-Command represents an ed(1) command as defined in
;; POSIX.1-2008. The command is identified by a unique symbol
;; and procedure which receives an editor object and the given
;; arguments as procedure arguments.

(define-record-type Editor-Command
  (make-cmd symbol default-addr proc args)
  editor-cmd?
  (default-addr cmd-default-addr)
  (symbol cmd-symbol)
  (proc cmd-proc)
  (args cmd-args))

;; Retruns true if the given command is a command which is reversible
;; according to the definition of the undo command in POSIX. For the
;; undo command itself, #f is returned.

(define (cmd-reversible? cmd)
  (member (cmd-symbol cmd)
          '(append change delete global insert join move
            read substitute copy global-unmatched interactive
            interactive-unmatched)))

;; Execute given command using given editor state.

(define (editor-exec editor addr cmd)
  (let* ((default-addr (cmd-default-addr cmd))
         ;; Convert given address to a single address or
         ;; a range address (depending on default address).
         (new-addr (if addr
                     (if (range? default-addr)
                       (addr->range addr)
                       (range->addr addr))
                     default-addr))
         ;; Only execute with address argument if default
         ;; address (specified for cmd) is not null.
         (args (if (null? default-addr)
                 (cmd-args cmd)
                 (append (list new-addr) (cmd-args cmd)))))
    (if (and (null? default-addr) addr)
      (editor-raise "unexpected address")
      (apply (cmd-proc cmd) editor args))))

;; Execute a list of commands using given editor state.

(define (editor-exec-cmdlist editor cmd-pairs)
  (for-each (lambda (cmd-pair)
              (editor-exec editor (car cmd-pair) (cdr cmd-pair)))
            cmd-pairs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type Text-Editor
  (%make-text-editor filename input buffer line last-line error marks state re
                     lcmd replace modified? last-modified? silent? help?)
  text-editor?
  ;; Name of the file currently being edited.
  (filename text-editor-filename text-editor-filename-set!)
  ;; Input repl for this text editor.
  (input text-editor-repl)
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
  ;; Symbol with previous repl name executed by the editor or #f if none.
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
  (let* ((h (make-repl prompt))
         (e (%make-text-editor filename h (make-buffer) 0 0 #f '() #f "" '() '() #f #f silent? #f)))
    (unless (empty-string? filename)
      ;; XXX: Don't print `?` if file doesn't exist.
      (exec-edit e filename))
    e))

(define (handle-error editor line msg)
  (let* ((in (text-editor-repl editor))
         (prefix (if (terminal-port? (current-input-port))
                   ""
                   (string-append
                     "line " (number->string line) ": "))))
    (text-editor-prevcmd-set! editor #f)
    (editor-error
      editor
      (string-append prefix msg))))

(define (handle-sighup editor)
  (when (text-editor-modified? editor)
    (let* ((buf (text-editor-buffer editor))
           (data (lines->string (buffer->list buf)))
           (success? (write-file "ed.hup" data)))
      (unless success?
        (write-file (path-join (user-home) "ed.hup") data))))
  (exit))

(define (editor-start editor)
  (define (execute-command line cmd addr)
    (call-with-current-continuation
      (lambda (k)
        (with-exception-handler
          (lambda (eobj)
            (if (editor-error? eobj)
              (k (handle-error editor line (editor-error-msg eobj)))
              (raise eobj)))
          (lambda ()
            (editor-exec editor addr cmd)
            (text-editor-prevcmd-set! editor (cmd-symbol cmd)))))))

  (signal-mask! signal/quit)
  (set-signal-handler!
    signal/hup
    (lambda (signum)
      (handle-sighup editor)))

  (repl-run
    (text-editor-repl editor)
    editor
    (lambda (line res)
      (let ((cmd  (cdr res))
            (addr (car res)))
        (when (cmd-reversible? cmd)
          (editor-snapshot editor))
        (execute-command line cmd addr)))
    (lambda (line reason)
      (handle-error editor line reason)))

  ;; XXX: POSIX requires to be the end-of-file character to be treated
  ;; like a quit command in command mode. For interactive usage, the
  ;; user would have to confirm this quit when the buffer was modified.
  ;; However, this is currently not possible with edward as we can't
  ;; read past eof with Scheme's read-char procedure.
  (%exec-quit editor))

(define (editor-interactive editor)
  (let ((h (text-editor-repl editor)))
    (repl-interactive h)))

;; Returns the last executed shell command or raises an error if none.

(define (editor-shell-cmd editor)
  (let ((lcmd (text-editor-last-cmd editor)))
    (if (null? lcmd)
      (editor-raise "no previous command")
      lcmd)))

;; Build a new regex object and handle regex syntax errors as editor
;; errors. If the provided pattern is empty, the last used pattern is
;; re-used, if there is no last-used pattern an editor error is raised.

(define (editor-make-regex editor pattern)
  (define (editor-pattern editor pattern)
    (if (empty-string? pattern)
      (let ((last-re (text-editor-re editor)))
        (if (empty-string? last-re)
          (editor-raise "no previous pattern")
          last-re))
      (begin
        (text-editor-re-set! editor pattern)
        pattern)))

  (let* ((pattern (editor-pattern editor pattern))
         (regex (call-with-current-continuation
                  (lambda (k)
                    (with-exception-handler
                      (lambda (eobj)
                        (k (error-object-message eobj)))
                      (lambda ()
                        (k (make-regex pattern))))))))
    (if (regex? regex)
      regex
      (editor-raise regex))))

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
  (unless (terminal-port? (current-input-port))
    (exit #f)))

;; Reset all file-specific state in the editor.

(define (editor-reset! editor)
  (text-editor-buffer-set! editor (make-buffer))
  (text-editor-marks-set! editor '()))

(define (editor-mark-line editor addr mark)
  (let ((lines (editor-get-range editor (make-range addr))))
    (if (null? lines)
      (editor-raise "invalid address")
      (text-editor-marks-set! editor
        (alist-cons mark (car lines) (text-editor-marks editor))))))

(define (editor-get-mark editor mark)
  (let ((pair (assv mark (text-editor-marks editor))))
    (if pair
      (let ((lnum (editor-get-lnum editor (cdr pair))))
        (if lnum
          lnum
          ;; XXX: Delete mark if it is found to be invalid (e.g. line deleted)?
          (editor-raise (string-append "invalid mark: " (string mark)))))
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
     (let* ((cur (make-addr '(current-line)))
            (old (addr->line editor cur)))
      (editor-goto! editor fst)
      (let-values (((sline eline) (%editor-range editor cur snd)))
        (editor-goto! editor old) ;; undo side-effect
        (values sline eline))))
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
  (unless (null? text)
    (text-editor-modified-set! editor #t))

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
        (regex (editor-make-regex editor bre))
        (cont-proc (match direction
                          ('forward inc)
                          ('backward dec))))
    (call-with-current-continuation
      (lambda (exit)
        (unless (zero? (editor-lines editor))
          (for-each-index
            (lambda (idx elem)
              (when (regex-match? regex elem)
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
  (let-values (((sline eline) (editor-range editor range)))
    (iota (inc (- eline sline)) sline)))
