;;>| Text Editor Object
;;>
;;> Object for storing, inspecting, and modifying the editor state.

;;> Create a new text editor object on a given (potentially) empty
;;> `filename` string. If non-empty, an `edit-proc` needs to be provided
;;> which implements the `E` command to initially read the file.
;;> Furthermore, a `prompt` string must be provided and it must be
;;> indicated whether the editor should start in `silent?` mode.

(define (make-text-editor edit-proc filename prompt silent?)
  (let* ((h (make-repl prompt))
         (b (make-buffer))
         (e (%make-text-editor filename h b 0 0 #f '() #f "" '() '() #f #f silent? #f)))
    (unless (empty-string? filename)
      ;; XXX: Don't print `?` if file doesn't exist.
      (edit-proc e filename))
    e))

;;> Record type encapsulating the text editor state.

(define-record-type Text-Editor
  (%make-text-editor filename input buffer line last-line error marks state re
                     lcmd replace modified? last-modified? silent? help?)
  ;;> Predicate which returns true if the given object was created using [make-text-editor](#make-text-editor).
  text-editor?
  ;; Name of the file currently being edited.
  (filename
    ;;> Returns the name of the file that is currently being edited.
    text-editor-filename

    ;;> Change the file that is currently being edited.
    text-editor-filename-set!)
  ;; Input repl for this text editor.
  (input text-editor-repl)
  ;; List of strings representing all lines in the file.
  (buffer text-editor-buffer text-editor-buffer-set!)
  ;; Current line in the buffer.
  (line
    ;;> Returns the current line in the internal editor buffer.
    text-editor-line
    text-editor-line-set!)
  ;; Previous line in the buffer.
  (last-line text-editor-last-line text-editor-last-line-set!)
  ;; Last error message encountered (for h and H command).
  (error
    ;;> Returns a string representing the last encountered error message.
    text-editor-error
    text-editor-error-set!)
  ;; Assoc lists of marks for this editor.
  ;; XXX: Since data is never deleted from an assoc list this leaks memory.
  (marks text-editor-marks text-editor-marks-set!)
  ;; Symbol with previous cmd name executed by the editor or #f if none.
  (state
    ;;> Returns the symbol of the command previously executed by
    ;;> the editor, on `#f` if no previous command was executed.
    text-editor-prevcmd
    text-editor-prevcmd-set!)
  ;; String representing last encountered RE.
  (re text-editor-re text-editor-re-set!)
  ;; Last command executed by the shell escape editor command or '() if none.
  (lcmd
    text-editor-last-cmd
    ;;> Update the last shell command executed via the shell escape editor command.
    text-editor-last-cmd-set!)
  ;; Last used replacement for the substitute command or '() if none.
  (replace text-editor-last-replace text-editor-last-replace-set!)
  ;; Whether the editor has been modified since the last write.
  (modified?
    ;;> Predicate which returns true if the current file has been
    ;;> modified since the last write to a file (i.e. has unwritten data).
    text-editor-modified?

    ;;> Modify the modified state of the current file.
    ;;> Set this to `#t` if the file has been modified.
    text-editor-modified-set!)
  ;; Whether the editor has been modified before the last command was executed.
  (last-modified? text-editor-last-modified?  text-editor-last-modified-set!)
  ;; Whether the editor is in silent mode (ed -s option).
  (silent? text-editor-silent?)
  ;; Whether help mode is activated.
  (help?
    ;;> Predicate which returns true if help mode is activated (`H` command).
    text-editor-help?

    ;;> Enable help mode by passing a truth value to this procedure.
    text-editor-help-set!))

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
  ;; Returns `#t` if writes to file succeeded and `#f` otherwise.
  (define (write-file filename lines)
    (guard
      (eobj
        ((file-error? eobj) #f))
      ;; TODO: If file exists behavior is unspecified
      (call-with-output-file filename
        (lambda (port)
          (lines->port lines port)))
      #t))

  (when (text-editor-modified? editor)
    (let* ((buf (text-editor-buffer editor))
           (lines (buffer->list buf))
           (success? (write-file "ed.hup" lines)))
      (unless success?
        (write-file (path-join (user-home) "ed.hup") lines))))
  (exit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;>| Editor Interface
;;>
;;> High-level text editor interfaces.

;;> Start the read-eval-print loop (REPL) of the editor. Within the
;;> REPL, command parsing is performed using the given `cmd-parser`.

(define (editor-start editor cmd-parser)
  (define (execute-command line cmd addr)
    (guard
      (eobj
        ((editor-error? eobj)
         (handle-error editor line (editor-error-msg eobj))))
      (if (cmd-reversible? cmd)
        (editor-with-undo editor
          (lambda ()
            (editor-exec editor addr cmd)))
        (editor-exec editor addr cmd))
      (text-editor-prevcmd-set! editor (cmd-symbol cmd))))

  (signal-mask! signal/quit)
  (set-signal-handler!
    signal/hup
    (lambda (signum)
      (handle-sighup editor)))

  (repl-run
    (text-editor-repl editor)
    cmd-parser
    ;; Success continuation.
    (lambda (line res)
      (let ((cmd  (cdr res))
            (addr (car res)))
        (execute-command line cmd addr)))
    ;; Failure continuation.
    (lambda (line reason)
      (handle-error editor line reason))
    ;; Interrupt continuation.
    (lambda ()
      (newline)
      (editor-error editor "Interrupt"))))

;;> Run an interactive command within the text editor.
;;> The command is parsed using the provided `cmd-parser`.

(define (editor-interactive editor cmd-parser)
  (let ((repl (text-editor-repl editor)))
    (repl-interactive repl
      cmd-parser
      (lambda (line reason)
        (editor-raise "parsing of interactive command failed")))))

;;> Toggle visibility of the REPL prompt.

(define (editor-toggle-prompt! editor)
  (let* ((repl (text-editor-repl editor))
         (prompt? (repl-prompt? repl)))
    (repl-set-prompt! repl (not prompt?))))

;;> Returns the last executed shell command or raises an error if none.

(define (editor-shell-cmd editor)
  (let ((lcmd (text-editor-last-cmd editor)))
    (if (null? lcmd)
      (editor-raise "no previous command")
      lcmd)))

;;> Build a new [regex][posix-regex regex] object and handle regex syntax
;;> errors as editor errors. If the provided pattern is empty, the last
;;> used pattern is re-used, if there is no last-used pattern an editor
;;> error is raised.
;;>
;;> [posix-regex regex]: https://wiki.call-cc.org/eggref/5/posix-regex#make-regex

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

;;> Access a replacement string in the editor context. If
;;> the provided replacement string `subst` is `'previous-replace`
;;> then the previously used replacement string is returned or an
;;> editor error is raised if there is no previous replacement string.
;;> Otherwise, (if `subst` is a string) then the previous replacement
;;> is updated and `subst` is returned.

(define (editor-restr editor subst)
  (if (equal? subst 'previous-replace)
    (let ((last-subst (text-editor-last-replace editor)))
      (if (null? last-subst)
        (editor-raise "no previous replacement")
        last-subst))
    (begin
      (text-editor-last-replace-set! editor subst)
      subst)))

;;> Return the currently configured filename, if no default is given it
;;> is an error if no filename is configured for the given editor.

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

;;> Print `objs`, but only if the editor is not in silent mode.

(define (editor-verbose editor . objs)
  (unless (text-editor-silent? editor)
    (apply println objs)))

;;> Print `?` optionally followed by `msg`, if the editor is in help mode.
;;> If standard input does not refer to a terminal device, the editor
;;> terminates with a non-zero exit status.

(define (editor-error editor msg)
  (text-editor-error-set! editor msg)
  (println "?")
  (when (text-editor-help? editor)
    (println msg))

  ;; See "Consequences of Errors" section in POSIX.1-2008.
  (unless (terminal-port? (current-input-port))
    (exit #f)))

;;> Raise an R7RS editor error exception with the given `msg`. This
;;> error is caught by an error handler and causes the `msg` to be
;;> printed using the [editor-error](#editor-error) procedure.

(define (editor-raise msg)
  (raise (make-editor-error msg)))

;; Editor-Error is a custom object raised to indicate a non-fatal
;; error condition handled according to the ed(1) POSIX specification.
(define-record-type Editor-Error
  (make-editor-error msg)
  editor-error?
  (msg editor-error-msg))

;;> Reset all file-specific state of the editor.

(define (editor-reset! editor)
  (text-editor-line-set! editor 0)
  (text-editor-last-line-set! editor 0)
  (text-editor-buffer-set! editor (make-buffer))
  (text-editor-marks-set! editor '()))

;;> Create an editor mark named `mark` which refers to the given `line`.

(define (editor-mark-line editor line mark)
  (let ((lines (editor-get-lines editor (cons line line))))
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

;;> Move editor cursor to specified line. Line 1 is the first line,
;;> specifying 0 as a line moves the cursor *before* the first line.

(define (editor-goto! editor line)
  (text-editor-line-set! editor line))

;; Intermediate range values can be invalid, e.g. "7,5,". This
;; parameter can be set to disable sanity checks on range values.
(define allow-invalid-ranges
  (make-parameter #f))

;;> Find current line number for a given line in the editor buffer. False
;;> is returned if the line does not exist in the editor buffer.
;;
;; XXX: This implementation assumes that eq? performs pointer comparision,
;; which is the case with CHICKEN but technically this is undefinied behaviour.

(define (editor-get-lnum editor line)
  (let ((buffer (text-editor-buffer editor)))
    (find
      (lambda (lnum)
        (eq? (buffer-ref buffer (dec lnum)) line))
      (iota (editor-lines editor) 1))))

;;> Return the content of the editor text buffer as a list of lines
;;> for the specified line pair `lines`. The start address of the
;;> pair is inclusive while the end address is exclusive.

(define (editor-get-lines editor lines)
  (if (buffer-empty? (text-editor-buffer editor))
    '()
    (let ((sline (car lines))
          (eline (cdr lines)))
      (buffer->list
        (text-editor-buffer editor)
        (max (dec sline) 0)
        eline))))

;;> Predicate which returns true if the given `line` is within
;;> the range specified by `lines`.

(define (editor-in-range? editor lines line)
  (let ((sline (car lines))
        (eline (cdr lines)))
    (and (>= line sline) (< line eline))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;>| Editor Operations
;;>
;;> Procedure which modify the editor text buffer. Provided operations
;;> are wrappers around operations of the internal text editor
;;> [line buffer][edward buffer] and additionally take care of updating
;;> the editor state (e.g. the [modified state][text-editor-modified?]).
;;>
;;> [text-editor-modified?]: #text-editor-modified?
;;> [edward buffer]: edward.buffer.html

;; Execute the given thunk and make all buffer operations and editor
;; state modifications performed in thunk undoable via editor-undo!.

(define (editor-with-undo editor thunk)
  (let ((m? (text-editor-modified? editor))
        (ll (text-editor-line editor)))
    (buffer-with-undo (text-editor-buffer editor) thunk)

    ;; buffer-with-undo succeeded → commit previous editor state.
    (text-editor-last-modified-set! editor m?)
    (text-editor-last-line-set! editor ll)))

;;> Undo the last operation on the buffer.

(define (editor-undo! editor)
  (unless (buffer-has-undo? (text-editor-buffer editor))
    (editor-raise "nothing to undo"))

  (text-editor-modified-set!
    editor
    (text-editor-last-modified? editor))

  (let ((cur-line (text-editor-line editor)))
    (text-editor-line-set!
      editor
      (text-editor-last-line editor))
    (text-editor-last-line-set! editor cur-line))
  (buffer-undo! (text-editor-buffer editor)))

;;> Returns amount of lines in the buffer.

(define (editor-lines editor)
  (buffer-length (text-editor-buffer editor)))

;;> Returns list of line numbers for given lines.

(define (editor-line-numbers lines)
  (let ((sline (car lines))
        (eline (cdr lines)))
    (iota (inc (- eline sline)) sline)))

;;> Append the text at the current address.
;;> Returns line number of last inserted line.

(define (editor-append! editor line text)
  (unless (null? text)
    (text-editor-modified-set! editor #t))

  (let ((buf (text-editor-buffer editor)))
    (buffer-append! buf line text)
    (+ line (length text))))

;;> Replace text of given lines with given data.
;;> Returns line number of last inserted line.

(define (editor-replace! editor lines data)
  (text-editor-modified-set! editor #t)
  (let* ((sline  (car lines))
         (eline  (cdr lines))
         (buffer (text-editor-buffer editor)))
    (buffer-replace! buffer sline eline data)
    (+ (max 0 (dec sline)) (length data))))

;;> Join given lines to single line. Return value is undefined.

(define (editor-join! editor lines)
  (text-editor-modified-set! editor #t)
  (let ((sline  (car lines))
        (eline  (cdr lines))
        (buffer (text-editor-buffer editor)))
    (buffer-join! buffer sline eline)))

;;> Remove given lines. Return value is undefined.

(define (editor-remove! editor lines)
  (text-editor-modified-set! editor #t)
  (let ((sline  (car lines))
        (eline  (cdr lines))
        (buffer (text-editor-buffer editor)))
    (buffer-remove! buffer sline eline)))

;;> Move given `lines` to given destination `dest-line`.
;;> Returns the address of the last inserted line.

(define (editor-move! editor lines dest-line)
  (text-editor-modified-set! editor #t)
  (let ((sline  (car lines))
        (eline  (cdr lines))
        (buffer (text-editor-buffer editor)))
    (buffer-move! buffer sline eline dest-line)
    (min
      (editor-lines editor)
      (let ((diff (- eline sline)))
        (+ dest-line
           ;; If we moved multiple lines, we need to increment
           ;; the destination lines by the amount of lines moved.
           (if (zero? diff) diff (inc diff)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;>| Address Translation
;;>
;;> Procedures for performing address translation. That is, procedures
;;> which convert an [edward ed addr][edward ed addr] to a line number
;;> (or a pair of line numbers) based on the current editor state. For
;;> example, the ed address `.` would be converted to the current line
;;> number (as tracked in the [editor object][section text-editor]).
;;> The resulting address can then be passed to an
;;> [editor operation][section operations].
;;>
;;> [edward ed addr]: edward.ed.addr.html
;;> [section text-editor]: #section-text-editor-object
;;> [section operations]: #section-editor-operations

;;> Convert a single address (i.e. as created via [make-addr][make-addr])
;;> to a single line number. This is a procedure which must be passed the
;;> text editor object and an edward ed address as procedure arguments.
;;>
;;> [make-addr]: edward.ed.addr.html#make-addr

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

(define (%addr->line editor off line)
  (let* ((total-off (apply + off))
         (nline (+ total-off line)))
    (if (or
          (> 0 nline)
          (> nline (editor-lines editor)))
      (editor-raise (string-append "invalid final address value: "
                                   (number->string nline)))
      nline)))

(define (match-line direction editor bre)
  (let ((lines (buffer->list (text-editor-buffer editor)))
        (regex (editor-make-regex editor bre))
        (cont-proc (case direction
                          ((forward) inc)
                          ((backward) dec))))
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

;;> Convert a `range` address (i.e. as created via [make-range][make-range])
;;> to a line pair. This procedure does not modify the current editor
;;> addresses, even for address range like `5;6`.
;;>
;;> [make-range]: edward.ed.addr.html#make-range

(define (range->lpair editor range)
  (let* ((cur (make-addr '(current-line)))
         (old (addr->line editor cur))
         (ret (range->lpair! editor range)))
    (editor-goto! editor old) ;; undo range->lpair! side-effect
    ret))

(define (range->lpair! editor range)
  (define (%range->lpair! editor start end)
    (let ((sline (addr->line editor start))
          (eline (addr->line editor end)))
      (if (and (not (allow-invalid-ranges))
               (> sline eline))
        (editor-raise "invalid range specification")
        (cons sline eline))))

  ;; In the case of a <semicolon> separator, the current line ('.') shall
  ;; be set to the first address, and only then will the second address be
  ;; calculated. This feature can be used to determine the starting line
  ;; for forwards and backwards searches.
  (match range
    ((fst #\; snd)
      (editor-goto! editor (addr->line editor fst)) ;; side-effect
      (%range->lpair! editor (make-addr '(current-line)) snd))
    ((fst #\, snd)
     (%range->lpair! editor fst snd))))

;; This procedure expands a list of addresses into a single pair of
;; concrete line numbers. As such, this procedure is responsible for
;; both applying the omission rules and discarding addresses.
;;
;; For example the address list for "7,5," is evaluted as follows:
;;
;;  7,5, [discard] -> 5, [expand] -> 5,5
;;
(define (%addrlst->lpair editor lst)
  (range->lpair!
    editor
    (expand-addr
      (fold (lambda (cur stk)
              (if (and (address-separator? cur)
                       (any address-separator? stk))
                ;; Intermediate range values can be invalid (e.g. "7,5,").
                (let ((lpair (parameterize ((allow-invalid-ranges #t))
                               (range->lpair! editor (expand-addr stk)))))
                  (list
                    (make-addr (cons 'nth-line (cdr lpair))) ;; discard car
                    cur))
                (append stk (list cur))))
            '() lst))))

;;> This procedure takes an addrlist, as returned by [parse-addrs][parse-addrs]
;;> and an editor object as an argument and returns a concrete line pair
;;> for this address. This line pair can then be passed to defined
;;> editor commands.
;;>
;;> [parse-addrs]: edward.ed.addr.html#parse-addrs

(define (addrlst->lpair editor lst)
  (let* ((cur (make-addr '(current-line)))
         (old (addr->line editor cur))
         (ret (%addrlst->lpair editor lst)))
    (editor-goto! editor old) ;; undo range->lpair! side-effect
    ret))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;>| Command Execution
;;>
;;> Based on defined [editor operations][section operations], it is
;;> possible to define custom editor commands which combine multiple
;;> operations. These commands can then be executed by the users via
;;> the REPL spawned by [editor-start][editor-start].
;;>
;;> **Warning:** The procedures documented here provide a low-level
;;> interface for defining custom editor commands. However, it is highly
;;> discouraged to use this interface directly. Instead, editor commands
;;> should be defined through the macros provided by [edward ed
;;> cmd][edward ed cmd].
;;>
;;> [editor-start]: #editor-start
;;> [section operations]: #section-editor-operations
;;> [edward ed cmd]: edward.ed.cmd.html

;;> Record type representing editor commands as defined in POSIX.1-2008.

(define-record-type Editor-Command
  ;;> Create a new editor command. The command is identified by a unique
  ;;> `symbol` and procedure `proc` which receives an editor object and
  ;;> the given `args` as procedure arguments.
  (make-cmd symbol default-addr proc args)
  ;;> Predicate which returns true if the given `obj` was created using
  ;;> the [make-cmd](#make-cmd) procedure.
  editor-cmd?
  (default-addr cmd-default-addr)
  (symbol cmd-symbol)
  (proc cmd-proc)
  (args
    ;;> Retrieve additional arguments defined for this command.
    ;;> The returned list value does not include the editor object.
    cmd-args))

;; Returns true if the given command is a command which is reversible
;; according to the definition of the undo command in POSIX. For the
;; undo command itself, #f is returned.

(define (cmd-reversible? cmd)
  (member (cmd-symbol cmd)
          '(append change delete global insert join move
            read substitute copy global-unmatched interactive
            interactive-unmatched)))

;;> Execute an editor command `cmd` using the given `editor` state
;;> on the addresses given by `addrlst`. The given addresses are
;;> translated to line addresses internally. If a command should
;;> be executed on a line address directly, use the
;;> [editor-xexec](#editor-xexec) procedure instead.

(define (editor-exec editor addrlst cmd)
  ;; XXX: Special handling for write command with empty buffer.
  ;; Without this, it would be impossible to use write with
  ;; an empty buffer since the default address is invalid then.
  ;;
  ;; TODO: Find a better way to deal with this edge case.
  (if (and (eqv? (cmd-symbol cmd) 'write)
           (not addrlst)
           (buffer-empty? (text-editor-buffer editor)))
      (editor-xexec editor '(1 . 1) cmd)
      (let* ((default-addr (cmd-default-addr cmd))
             ;; Convert addrlst to line pair (if any given) or
             ;; use default address and convert that (if any).
             (line-pair (if addrlst
                          (addrlst->lpair editor addrlst)
                          (and (not (null? default-addr))
                               (range->lpair editor (addr->range default-addr)))))
             ;; Convert given address (if any) to a single line
             ;; or a line pair (depending on default address).
             (line-addr (if (or (not line-pair) (range? default-addr))
                          line-pair
                          (cdr line-pair))))
          (editor-xexec editor line-addr cmd))))

;;> Execute given `cmd` using given `editor` state on the address
;;> `addr`. The address can either be a single line address, a
;;> line pair, or an empty list depending on the default address
;;> specified for `cmd`. If the command doesn't specify a default
;;> address (i.e. doesn't expect an address argument) then it is
;;> an error to pass anything other than the empty list as an
;;> `addr` value to this procedure.

(define (editor-xexec editor addr cmd)
  (let ((default-addr (cmd-default-addr cmd)))
    (when (and (null? default-addr) addr)
      (editor-raise "unexpected address"))
    (when (and (range? default-addr) (zero? (car addr)))
      (editor-raise "ranges cannot start at address zero"))

    (apply (cmd-proc cmd)
           editor
           (if (null? (cmd-default-addr cmd)) ;; doesn't expect address
             (cmd-args cmd)
             (append (list addr) (cmd-args cmd))))))

;;> Execute a list of commands using given editor state.

(define (editor-exec-cmdlist editor cmd-pairs)
  (for-each (lambda (cmd-pair)
              (editor-exec editor (car cmd-pair) (cdr cmd-pair)))
            cmd-pairs))
