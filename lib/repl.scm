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

;; Skip all buffered chunks, i.e. next read will block.

(define (input-handler-skip-chunks! handler)
  (define (%input-handler-skip-chunks! source i)
    (if (>= (+ i 1) (vector-length (parse-stream-buffer source)))
      (%input-handler-skip-chunks! (parse-stream-tail source) i) ;; go to last chunck
      (values
        source
        ;; inc to go beyond last char.
        (inc (parse-stream-max-char source)))))

  (let-values (((source i)
                (%input-handler-skip-chunks!
                  (input-handler-stream handler)
                  (input-handler-index handler))))
    (input-handler-state-set! handler source i)))

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
  (let ((s (input-handler-stream handler)))
    (inc ;; XXX: For some reason line start at zero.
      (+
        (parse-stream-line s)
        (car (parse-stream-count-lines s (parse-stream-max-char s)))))))

(define (input-handler-repl handler editor sk fk)
  (when (input-handler-prompt? handler)
    (display (input-handler-prompt-str handler))
    (flush-output-port))

  ;; Allow parsing itself (especially of input mode commands) to be
  ;; interrupted by SIGINT signals. See "Asynchronous Events" in ed(1).
  (let ((eof?
          (call-with-current-continuation
            (lambda (k)
              (set-signal-handler!
                signal/int
                (lambda (signum)
                  (newline)
                  (editor-error editor "Interrupt")
                  (input-handler-skip-chunks! handler)
                  (k #f)))
              (if (parse-stream-end?
                    (input-handler-stream handler)
                    (input-handler-index handler))
                (k #t)
                (begin
                  (input-handler-parse handler parse-cmd sk fk)
                  (k #f)))))))
    (unless eof?
      (input-handler-repl handler editor sk fk))))

(define (input-handler-interactive handler)
  (input-handler-parse
    handler
    parse-interactive-cmd
    (lambda (line value) value)
    (lambda (line reason)
      (editor-raise "parsing of interactive command failed"))))
