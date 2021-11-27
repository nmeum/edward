(import (chicken base) (chicken process))

;; The `chicken-install -test` command would normally run
;; all tests using csi(1). However, this would not allow
;; us to use the FFI in tests. As such, we invoke csc(1)
;; first from the tests and then run them.

(let ((r (system "csc -R r7rs -d3 tests.scm -o /tmp/edward-tests")))
  (if r
    (exit (system "/tmp/edward-tests"))
    (error "compilation failed")))
