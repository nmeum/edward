(import (r7rs))
(import (scheme base) (scheme process-context)
        (edward) (srfi 37) (matchable))

(define prompt-char "")
(define silent-mode #f)

(define help
  (option
    '(#\h "help") #f #f
    (lambda _
      (usage))))

(define prompt
  (option
    '(#\p "prompt") #t #t
    (lambda (o n x vals)
      (set! prompt-char x)
      vals)))

(define silent
  (option
    '(#\p "prompt") #f #f
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

(let* ((files (cdr (parse-args (list help prompt silent)))))
  (match files
    ((file)
     (start-editor prompt-char file))
    (()
     (start-editor prompt-char ""))
    (_
     (error "specify one file or no files"))))