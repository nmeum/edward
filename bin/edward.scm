(import (r7rs))
(import (scheme base) (scheme process-context)
        (edward) (srfi 37) (matchable))

(define prompt "")
(define silent? #f)

(define prompt-opt
  (option
    '(#\p "prompt") #t #t
    (lambda (o n x vals)
      (set! prompt x)
      vals)))

(define silent-opt
  (option
    '(#\s "silent") #f #f
    (lambda (o n x vals)
      (set! silent? #t)
      vals)))

(define (parse-args flags)
  (reverse
    (args-fold
      (command-line)
      flags
      (lambda (o n x vals)
        (error "unrecognized option" n))
      cons
      '())))

(define (run-editor filename)
  (let ((editor (make-text-editor filename silent?)))
    (editor-start editor prompt)))

(let* ((files (cdr (parse-args (list prompt-opt silent-opt)))))
  (match files
    ((file)
     (run-editor file))
    (()
     (run-editor ""))
    (_
     (error "specify one file or no files"))))
