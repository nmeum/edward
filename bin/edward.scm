(import (r7rs))
(import (scheme base) (scheme process-context)
        (edward) (srfi 37) (matchable))

(define prompt-char "")
(define silent-mode #f)

(define prompt
  (option
    '(#\p "prompt") #t #t
    (lambda (o n x vals)
      (set! prompt-char x)
      vals)))

(define silent
  (option
    '(#\s "silent") #f #f
    (lambda (o n x vals)
      (set! silent-mode #t)
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
  (let ((editor (make-text-editor filename silent-mode)))
    (editor-start editor prompt-char)))

(let* ((files (cdr (parse-args (list prompt silent)))))
  (match files
    ((file)
     (run-editor file))
    (()
     (run-editor ""))
    (_
     (error "specify one file or no files"))))
