(import (scheme base)
        (edward buffer)
        (edward util)
        (chicken random)
        (micro-benchmark))

(define (repeat n thunk)
  (when (> n 0)
    (thunk)
    (repeat (dec n) thunk)))

(define (random-string)
  (let* ((str (make-string 5000)))
    (random-bytes str)
    str))

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
  (let* ((stats (benchmark-run (3) (proc (make-buffer)))))
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
    ((define-bench (NAME BUFFER) BODY ...)
     (run-benchmark
       (symbol->string (quote NAME))
       (lambda (BUFFER)
         BODY ...)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-bench (buffer-append-single-at-end buffer)
  (repeat
    10000
    (lambda ()
      (buffer-append!
        buffer
        (buffer-length buffer)
        (list (random-string))))))

(define-bench (buffer-append-multi-at-end buffer)
  (repeat
    10000
    (let ((next-size 1)
          (max-size 10))
      (lambda ()
        (buffer-append!
          buffer
          (buffer-length buffer)
          (generate-list random-string next-size))
        (set! next-size (modulo (inc next-size) max-size))))))

(define-bench (buffer-append-single-randomly buffer)
  (repeat
    10000
    (lambda ()
      (buffer-append!
        buffer
        (pseudo-random-integer (buffer-length buffer))
        (list (random-string))))))

(define-bench (buffer-remove-from-front buffer)
  (let ((elems 15000))
    (buffer-append! buffer 0 (generate-list random-string elems))
    (repeat elems
      (lambda ()
        (buffer-remove! buffer 1 1)))))

(define-bench (buffer-remove-from-back buffer)
  (let ((elems 15000))
    (buffer-append! buffer 0 (generate-list random-string elems))
    (repeat elems
            (lambda ()
              (let ((len (buffer-length buffer)))
                (buffer-remove! buffer len len))))))
