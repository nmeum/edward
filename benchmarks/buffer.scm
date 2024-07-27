(import (edward buffer)
        (edward util))

(define-bench (buffer-append-single-at-end)
  (let ((buffer (make-buffer)))
    (repeat
      10000
      (lambda ()
        (buffer-append!
          buffer
          (buffer-length buffer)
          (list (random-string)))))))

(define-bench (buffer-append-multi-at-end)
  (let ((buffer (make-buffer)))
    (repeat
      10000
      (let ((next-size 1)
            (max-size 10))
        (lambda ()
          (buffer-append!
            buffer
            (buffer-length buffer)
            (generate-list random-string next-size))
          (set! next-size (modulo (inc next-size) max-size)))))))

(define-bench (buffer-append-single-randomly)
  (let ((buffer (make-buffer)))
    (repeat
      10000
      (lambda ()
        (buffer-append!
          buffer
          (pseudo-random-integer (buffer-length buffer))
          (list (random-string)))))))

(define-bench (buffer-remove-from-front)
  (let ((buffer (make-buffer))
        (elems 15000))
    (buffer-append! buffer 0 (generate-list random-string elems))
    (repeat elems
      (lambda ()
        (buffer-remove! buffer 1 1)))))

(define-bench (buffer-remove-from-back)
  (let ((buffer (make-buffer))
        (elems 15000))
    (buffer-append! buffer 0 (generate-list random-string elems))
    (repeat elems
            (lambda ()
              (let ((len (buffer-length buffer)))
                (buffer-remove! buffer len len))))))
