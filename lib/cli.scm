(define prompt "")
(define silent? #f)

(define (err msg)
  (let ((port (current-error-port)))
    (display "edward: " port)
    (display msg port)
    (newline port)))

(define prompt-opt
  (option
    '(#\p "prompt") #t #f
    (lambda (o n x vals)
      (set! prompt x)
      vals)))

(define silent-opt
  (option
    '(#\s "silent") #f #f
    (lambda (o n x vals)
      (set! silent? #t)
      vals)))

(define (parse-args args flags)
  (reverse
    (args-fold
      args
      flags
      (lambda (o n x vals)
        (error "unrecognized option" n))
      cons
      '())))

(define (run-editor filename)
  (let ((editor (make-text-editor exec-edit filename prompt silent?)))
    (editor-start editor (parse-cmd))))

(define (edward-main . args)
  (let* ((flags (list prompt-opt silent-opt))
         (argv  (if (null? args) (command-line) args))
         (files (cdr (parse-args argv flags))))
    (if prompt
      (case (length files)
        ((1)  (run-editor (car files)))
        ((0)  (run-editor ""))
        (else (err "specify one file or no files")))
      (err "missing prompt option argument"))))
