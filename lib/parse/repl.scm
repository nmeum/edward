(define-record-type Read-Eval-Print-Loop
  (%make-repl prompt-str prompt? stream index)
  repl?
  ;; Prompt string used for input prompt.
  (prompt-str repl-prompt-str)
  ;; Whether the prompt should be shown or hidden.
  (prompt? repl-prompt? repl-set-prompt!)
  ;; Parse stream used for the parser combinator.
  (stream repl-stream repl-stream-set!)
  ;; Last index in parse stream.
  (index repl-index repl-index-set!))

(define (make-repl prompt)
  (let ((prompt? (not (empty-string? prompt))))
    (%make-repl
      (if prompt? prompt "*")
      prompt?
      (make-parse-stream "stdin" (current-input-port))
      0)))

(define (repl-state-set! repl source index)
  (repl-stream-set! repl source)
  (repl-index-set! repl index))

;; Skip all buffered chunks, i.e. next read will block.

(define (repl-skip-chunks! repl)
  (define (%repl-skip-chunks! source i)
    (if (>= (+ i 1) (vector-length (parse-stream-buffer source)))
      (%repl-skip-chunks! (parse-stream-tail source) i) ;; go to last chunck
      (values
        source
        ;; inc to go beyond last char.
        (inc (parse-stream-max-char source)))))

  (let-values (((source i)
                (%repl-skip-chunks!
                  (repl-stream repl)
                  (repl-index repl))))
    (repl-state-set! repl source i)))

(define (repl-parse repl f sk fk)
  (define (stream-next-line source idx)
    (let* ((next-index  (parse-stream-next-index source idx))
           (next-source (parse-stream-next-source source idx))
           (char        (parse-stream-ref source idx)))
      (if (or (eof-object? char) (char=? char #\newline))
        (cons next-source next-index) ;; first index after newline/eof
        (stream-next-line
          next-source
          next-index))))

  (call-with-parse f
    (repl-stream repl)
    (repl-index repl)
    (lambda (r s i fk)
      (repl-state-set! repl s i)
      (sk (repl-line repl i) r))
    (lambda (s i reason)
      (let ((line (repl-line repl i))
            (next (stream-next-line (repl-stream repl) i)))
        (repl-state-set! repl (car next) (cdr next))
        (fk line reason)))))

(define (repl-line repl index)
  (let ((s (repl-stream repl)))
    (inc ;; XXX: For some reason line start at zero.
      (+
        (parse-stream-line s)
        (car (parse-stream-count-lines s (parse-stream-max-char s)))))))

(define (repl-run repl f sk fk ik)
  (when (repl-prompt? repl)
    (display (repl-prompt-str repl))
    (flush-output-port))

  ;; Allow parsing itself (especially of input mode commands) to be
  ;; interrupted by SIGINT signals. See "Asynchronous Events" in ed(1).
  (let ((eof?
          (call-with-current-continuation
            (lambda (k)
              (set-signal-handler!
                signal/int
                (lambda (signum)
                  (ik)
                  (repl-skip-chunks! repl)
                  (k #f)))
              (if (parse-stream-end?
                    (repl-stream repl)
                    (repl-index repl))
                (k #t)
                (begin
                  (repl-parse repl f sk fk)
                  (k #f)))))))
    (unless eof?
      (repl-run repl f sk fk ik))))

(define (repl-interactive repl f fk)
  (repl-parse repl f (lambda (line value) value) fk))
