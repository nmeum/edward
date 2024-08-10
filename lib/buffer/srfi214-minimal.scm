;; Copyright (c) 2020-2021 Adam Nelson.
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to
;; deal in the Software without restriction, including without limitation the
;; rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
;; sell copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice (including the next
;; paragraph) shall be included in all copies or substantial portions of the
;; Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
;; IN THE SOFTWARE.

(define-record-type Flexvector
  (%make-flexvector fv-vector fv-length)
  flexvector?
  (fv-vector vec set-vec!)
  (fv-length flexvector-length set-flexvector-length!))

(define (grow! fv)
  (define old-vec (vec fv))
  (define new-vec (make-vector (quotient (* (vector-length old-vec) 3) 2)))
  (vector-copy! new-vec 0 old-vec)
  (set-vec! fv new-vec)
  new-vec)

(define (flexvector)
  (%make-flexvector (make-vector 4) 0))

(define (flexvector-add-all! fv i xs)
  (let* ((len (flexvector-length fv))
         (xv (list->vector xs))
         (xvlen (vector-length xv))
         (v (let lp ((v (vec fv)))
              (if (< (+ len xvlen) (vector-length v)) v (lp (grow! fv))))))
    (vector-copy! v (+ i xvlen) v i len)
    (vector-copy! v i xv 0 xvlen)
    (set-flexvector-length! fv (+ len xvlen))
    fv))

(define (flexvector-remove-range! fv start end)
  (let ((len (flexvector-length fv)))
    (when (< start 0) (set! start 0))
    (when (>= end len) (set! end len))
    (vector-copy! (vec fv) start (vec fv) end)
    (let ((new-len (- len (- end start))))
      (vector-fill! (vec fv) #f new-len len)
      (set-flexvector-length! fv new-len)))
  fv)

(define flexvector->list
  (case-lambda
    ((fv)
     (flexvector->list fv 0 (flexvector-length fv)))
    ((fv start)
     (flexvector->list fv start (flexvector-length fv)))
    ((fv start end)
     (vector->list (vector-copy (vec fv) start end)))))
