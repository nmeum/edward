(import (edward util)
        (chicken random)
        (micro-benchmark))

(define (repeat n thunk)
  (when (> n 0)
    (thunk)
    (repeat (dec n) thunk)))

(define (random-string . rest)
  (let ((siz (if (null? rest) 100 (car rest))))
    (list->string
      (generate-list
        (lambda ()
          (let ((ascii (+ (pseudo-random-integer 126) 33)))
            (integer->char ascii)))
        siz))))

(define (generate-list thunk size)
  (map
    (lambda (proc)
      (proc))
    (make-list size thunk)))

(define (msec-stat sym stats)
  (define (msec->sec msec)
    (/ msec 1000000))

  (define (round-off z n)
    (let ((power (expt 10 n)))
      (/ (round (* power z)) power)))

  (let ((el (assq sym stats)))
    (if (not el)
      (error (string-append "no element named '" (symbol->string sym) "' in statistics"))
      (round-off (msec->sec (cdr el)) 3))))

(define (run-benchmark name proc)
  (let* ((stats (benchmark-run (BENCHMARK_ITERATIONS) (proc))))
    (display name)
    (display ":\t")
    (display (msec-stat 'arithmetic-mean stats))
    (display "s")
    (display " +/- ")
    (display (msec-stat 'standard-deviation stats))
    (display "s")
    (newline)))

(define-syntax define-bench
  (syntax-rules ()
    ((define-bench (NAME) BODY ...)
     (run-benchmark
       (symbol->string (quote NAME))
       (lambda ()
         BODY ...)))))
