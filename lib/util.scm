(define (inc n) (+ n 1))
(define (dec n) (- n 1))

;; Like display but prints multiple objects and adds trailing newline.

(define (fprintln port . objs)
  (for-each (lambda (obj) (display obj port)) objs)
  (newline port))

(define (println . objs)
  (apply fprintln (current-output-port) objs))

;; Print error with provided irritants (if any).

(define display-error
  (case-lambda
    ((eobj port)
     (%display-error eobj port))
    ((eobj)
     (%display-error eobj (current-output-port)))))

(define (%display-error eobj port)
  (let ((msg (error-object-message eobj)))
    (if (null? (error-object-irritants eobj))
      (fprintln port msg)
      (begin
        (display msg port)
        (display ": " port)
        (for-each (lambda (i) (write-simple i port))
                  (error-object-irritants eobj))
        (newline port)))))

;; Create a REPL and execute proc for each input.

(define (repl prompt proc)
  (unless (empty-string? prompt)
    (display prompt)
    (flush-output-port))

  (let ((input (read-line)))
    (unless (eof-object? input)
      (proc input)
      (repl prompt proc))))

;; Return true if the given string is the empty string.

(define (empty-string? str)
  (zero? (string-length str)))

;; Return sublist with start inclusive and end exclusive.

(define (sublist lst start end)
  (if (> start end)
    (error "invalid sublist specification")
    (let ((l (drop lst start)))
      (drop-right l (- (length lst) end)))))

;; Like init from Haskell, returns everything except the last list element.

(define (init lst)
  (define (%init lst)
    (if (eqv? 1 (length lst))
      '()
      (cons (car lst) (%init (cdr lst)))))

  (if (null? lst)
    (error "empty list")
    (%init lst)))
