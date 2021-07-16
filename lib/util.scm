;; Like init from Haskell, returns everything except the last list element.

(define (init lst)
  (define (%init lst)
    (if (eqv? 1 (length lst))
      '()
      (cons (car lst) (%init (cdr lst)))))

  (if (null? lst)
    (error "empty list")
    (%init lst)))
