(define (inc n) (+ n 1))
(define (dec n) (- n 1))

;; Like display but always adds a trailing newline.

(define println
  (case-lambda
    ((obj port) (%println obj port))
    ((obj) (%println obj (current-output-port)))))

(define (%println obj port)
  (display obj port)
  (newline port))

;; Return true if the given string is the empty string.

(define (empty-string? str)
  (zero? (string-length str)))

;; Return sublist with start inclusive and end exclusive.

(define (sublist lst start end)
  (if (>= start end)
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
