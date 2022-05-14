(define-record-type Stack
  (%make-stack store)
  stack?
  (store stack-store stack-store-set!))

(define (make-stack)
  (%make-stack '()))

(define (stack-clear! stack)
  (stack-store-set! stack '()))

(define (stack-size stack)
  (length (stack-store stack)))

(define (stack-empty? stack)
  (zero? (stack-size stack)))

(define (stack-push stack elem)
  (stack-store-set!
    stack
    (cons elem (stack-store stack))))

(define (stack-pop stack)
  (let* ((lst (stack-store stack))
         (top (car lst)))
    (stack-store-set! stack (cdr lst))
    top))

(define (stack-pops stack amount)
  (if (<= amount 1)
    (list (stack-pop stack))
    (cons
      (stack-pop stack)
      (stack-pops stack (dec amount)))))
