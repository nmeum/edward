(define handle-append
  (case-lambda
    ((addr)
     (begin
       (display "appending text at ") (display addr) (newline)))))
