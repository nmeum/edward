;; parse.scm -- Parser Combinators
;; Copyright (c) 2013 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;>| Parse Streams
;;>
;;> Parse streams are an abstraction to treat ports as proper streams
;;> so that we can backtrack from previous states.  A single
;;> Parse-Stream record represents a single buffered chunk of text.

(define-record-type Parse-Stream
  (%make-parse-stream
   filename port buffer cache offset prev-char line column tail fk)
  parse-stream?
  ;; The file the data came from, for debugging and error reporting.
  (filename parse-stream-filename)
  ;; The underlying port.
  (port parse-stream-port)
  ;; A vector of characters read from the port.  We use a vector
  ;; rather than a string for guaranteed O(1) access.
  (buffer parse-stream-buffer)
  ;; A vector of caches corresponding to parser successes or failures
  ;; starting from the corresponding char.  Currently each cache is
  ;; just an alist, optimized under the assumption that the number of
  ;; possible memoized parsers is relatively small.  Note that
  ;; memoization is only enabled explicitly.
  (cache parse-stream-cache)
  ;; The current offset of filled characters in the buffer.
  ;; If offset is non-zero, (vector-ref buffer (- offset 1)) is
  ;; valid.
  (offset parse-stream-offset parse-stream-offset-set!)
  ;; The previous char before the beginning of this Parse-Stream.
  ;; Used for line/word-boundary checks.
  (prev-char parse-stream-prev-char)
  ;; The debug info for the start line and column of this chunk.
  (line parse-stream-line)
  (column parse-stream-column)
  ;; The successor Parse-Stream chunk, created on demand and filled
  ;; from the same port.
  (tail %parse-stream-tail %parse-stream-tail-set!)
  ;; Initial fk as passed to call-with-parse. Retained as part of
  ;; the Parse-Stream for the parse-commit procedure.
  (fk parse-stream-fk parse-stream-fk-set!))

;; We want to balance avoiding reallocating buffers with avoiding
;; holding many memoized values in memory.
(define default-buffer-size 256)

;;> Create a parse stream open on the given `filename`, with a
;;> possibly already opened `port`.

(define (make-parse-stream filename . o)
  (let ((port (if (pair? o) (car o) (open-input-file filename)))
        (len (if (and (pair? o) (pair? (cdr o))) (cadr o) default-buffer-size)))
    (%make-parse-stream
     filename port (make-vector len #f) (make-vector len '()) 0 #f 0 0 #f #f)))

;;> Open `filename` and create a parse stream on it.

(define (file->parse-stream filename)
  (make-parse-stream filename (open-input-file filename)))

;;> Create a parse stream on a string `str`.

(define (string->parse-stream str)
  (make-parse-stream #f (open-input-string str)))

;;> Access the next buffered chunk of a parse stream.

(define (parse-stream-tail source)
  (or (%parse-stream-tail source)
      (let* ((len (vector-length (parse-stream-buffer source)))
             (line-info (parse-stream-count-lines source))
             (line (+ (parse-stream-line source) (car line-info)))
             (col (if (zero? (car line-info))
                      (+ (parse-stream-column source) (cadr line-info))
                      (cadr line-info)))
             (tail (%make-parse-stream (parse-stream-filename source)
                                       (parse-stream-port source)
                                       (make-vector len #f)
                                       (make-vector len '())
                                       0
                                       (parse-stream-last-char source)
                                       line
                                       col
                                       #f
                                       (parse-stream-fk source))))
        (%parse-stream-tail-set! source tail)
        tail)))

(define (parse-stream-fill! source i)
  (let ((off (parse-stream-offset source))
        (buf (parse-stream-buffer source))
        (src (parse-stream-port source)))
    (if (<= off i)
      ;; Optionally, the parse-stream-port can refer to a POSIX
      ;; file descriptor. In which case data will be accessed
      ;; using the (file-read) procedure. This is required in
      ;; order to read past EOF (required by ed(1)).
      ;;
      ;; TODO Currently a bit hacky, revisit after CHICKEN 6.
      (if (port? src)
        (do ((off off (+ off 1)))
            ((> off i) (parse-stream-offset-set! source off))
          (vector-set! buf off (read-char src)))
        (let* ((siz (inc (- i off)))
               (str (make-string siz))
               (num (cadr (file-read src siz str))))
          (if (zero? num)
            ;; When EOF was encountered, add one eof-object to
            ;; the buffer, then read past the EOF through recursion.
            (begin
              (vector-set! buf off (eof-object))
              (parse-stream-offset-set! source (inc off))
              (parse-stream-fill! source i))

            ;; Copy data retrieved from file (via file-read) to buffer.
            ;; XXX: Can't copy to vector buffer directly, unfortunately.
            (do ((off off (+ off 1)))
                ((> off i) (parse-stream-offset-set! source off))
              (vector-set! buf off (string-ref str (- i off)))))))
      #f)))

;;> Returns true iff `i` is the first character position in the
;;> parse stream `source`.

(define (parse-stream-start? source i)
  (and (zero? i) (not (parse-stream-prev-char source))))

;;> Returns true iff `i` is the last character position in the
;;> parse stream `source`.

(define (parse-stream-end? source i)
  (eof-object? (parse-stream-ref source i)))

;;> Returns the character in parse stream `source` indexed by
;;> `i`.

(define (parse-stream-ref source i)
  (parse-stream-fill! source i)
  (vector-ref (parse-stream-buffer source) i))

(define (parse-stream-last-char source)
  (let ((buf (parse-stream-buffer source)))
    (let lp ((i (min (- (vector-length buf) 1) (parse-stream-offset source))))
      (if (negative? i)
          (parse-stream-prev-char source)
          (let ((ch (vector-ref buf i)))
            (if (eof-object? ch)
                (lp (- i 1))
                ch))))))

(define (parse-stream-char-before source i)
  (if (> i (parse-stream-offset source))
      (parse-stream-ref source (- i 1))
      (parse-stream-prev-char source)))

(define (parse-stream-max-char source)
  (let ((buf (parse-stream-buffer source)))
    (let lp ((i (min (- (vector-length buf) 1)
                     (parse-stream-offset source))))
      (if (or (negative? i)
              (char? (vector-ref buf i)))
          i
          (lp (- i 1))))))

(define (parse-stream-count-lines source . o)
  (let* ((buf (parse-stream-buffer source))
         (end (if (pair? o) (car o) (vector-length buf))))
    (let lp ((i 0) (from 0) (lines 0))
      (if (>= i end)
          (list lines (- i from) from)
          (let ((ch (vector-ref buf i)))
            (cond
             ((not (char? ch))
              (list lines (- i from) from))
             ((eqv? ch #\newline)
              (lp (+ i 1) i (+ lines 1)))
             (else
              (lp (+ i 1) from lines))))))))

(define (parse-stream-end-of-line source i)
  (let* ((buf (parse-stream-buffer source))
         (end (vector-length buf)))
    (let lp ((i i))
      (if (>= i end)
          i
          (let ((ch (vector-ref buf i)))
            (if (or (not (char? ch)) (eqv? ch #\newline))
                i
                (lp (+ i 1))))))))

(define (parse-stream-debug-info s i)
  ;; i is the failed parse index, but we want the furthest reached
  ;; location
  (if (%parse-stream-tail s)
      (parse-stream-debug-info (%parse-stream-tail s) i)
      (let ((max-char (parse-stream-max-char s)))
        (if (< max-char 0)
            (list 0 0 "")
            (let* ((line-info
                    (parse-stream-count-lines s max-char))
                   (line (+ (parse-stream-line s) (car line-info)))
                   (col (if (zero? (car line-info))
                            (+ (parse-stream-column s) (cadr line-info))
                            (cadr line-info)))
                   (from (car (cddr line-info)))
                   (to (parse-stream-end-of-line s (+ from 1)))
                   (str (parse-stream-substring s from s to)))
              (list line col str))))))

(define (parse-stream-next-source source i)
  (if (>= (+ i 1) (vector-length (parse-stream-buffer source)))
      (parse-stream-tail source)
      source))

(define (parse-stream-next-index source i)
  (if (>= (+ i 1) (vector-length (parse-stream-buffer source)))
      0
      (+ i 1)))

(define (parse-stream-close source)
  (let ((src (parse-stream-port source)))
    (if (port? src)
      (close-input-port (parse-stream-port source))
      (file-close src))))

(define (vector-substring vec start . o)
  (let* ((end (if (pair? o) (car o) (vector-length vec)))
         (res (make-string (- end start))))
    (do ((i start (+ i 1)))
        ((= i end) res)
      (string-set! res (- i start) (vector-ref vec i)))))

(define (parse-stream-in-tail? s0 s1)
  (let ((s0^ (%parse-stream-tail s0)))
    (or (eq? s0^ s1)
        (and s0^ (parse-stream-in-tail? s0^ s1)))))

(define (parse-stream< s0 i0 s1 i1)
  (if (eq? s0 s1)
      (< i0 i1)
      (parse-stream-in-tail? s0 s1)))

;;> Returns a string composed of the characters starting at parse
;;> stream `s0` index `i0` (inclusive), and ending at `s1`
;;> index `i1` (exclusive).

(define (parse-stream-substring s0 i0 s1 i1)
  (cond
   ((eq? s0 s1)
    (parse-stream-fill! s0 i1)
    (vector-substring (parse-stream-buffer s0) i0 i1))
   (else
    (let lp ((s (parse-stream-tail s0))
             (res (list (vector-substring (parse-stream-buffer s0) i0))))
      (let ((buf (parse-stream-buffer s)))
        (cond
         ((eq? s s1)
          (apply string-append
                 (reverse (cons (vector-substring buf 0 i1) res))))
         (else
          (lp (parse-stream-tail s)
              (cons (vector-substring buf 0) res)))))))))

(define (parse-stream-cache-cell s i f)
  (assv f (vector-ref (parse-stream-cache s) i)))

(define (parse-stream-cache-set! s i f x)
  (let ((cache (vector-ref (parse-stream-cache s) i)))
    (cond
     ((assv f cache)
      => (lambda (cell)
           ;; prefer longer matches
           (if (and (pair? (cdr cell))
                    (parse-stream< (car (cddr cell)) (cadr (cddr cell)) s i))
               (set-cdr! cell x))))
     (else
      (vector-set! (parse-stream-cache s) i (cons (cons f x) cache))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;>| Parser Interface
;;>
;;> Procedures for operating on a created parse stream.

;;> Combinator to indicate failure.

(define (parse-failure s i reason)
  (let ((line+col (parse-stream-debug-info s i)))
    (error "incomplete parse at" (append line+col (list reason)))))

;;> Call the parser combinator `f` on the parse stream
;;> `source`, starting at index `index`, passing the result to
;;> the given success continuation `sk`, which should be a
;;> procedure of the form `(result source index fail)`.  The
;;> optional failure continuation should be a procedure of the form
;;> `(source index reason)`, and defaults to just returning
;;> `#f`.

(define (call-with-parse f source index sk . o)
  (let ((s (if (string? source) (string->parse-stream source) source))
        (fk (if (pair? o) (car o) (lambda (s i reason) #f))))
    (parse-stream-fk-set! s fk)
    (f s index sk fk)))

;;> Call the parser combinator `f` on the parse stream
;;> `source`, at index `index`, and return the result, or
;;> `#f` if parsing fails.

(define (parse f source . o)
  (let ((index (if (pair? o) (car o) 0)))
    (call-with-parse f source index (lambda (r s i fk) r))))

;;> Call the parser combinator `f` on the parse stream
;;> `source`, at index `index`.  If the entire source is not
;;> parsed, raises an error, otherwise returns the result.

(define (parse-fully f source . o)
  (let ((s (if (string? source) (string->parse-stream source) source))
        (index (if (pair? o) (car o) 0)))
    (call-with-parse
     f s index
     (lambda (r s i fk)
       (if (parse-stream-end? s i) r (fk s i "incomplete parse")))
     parse-failure)))

;;> The fundamental parse iterator.  Repeatedly applies the parser
;;> combinator `f` to `source`, starting at `index`, as
;;> long as a valid parse is found.  On each successful parse applies
;;> the procedure `kons` to the parse result and the previous
;;> `kons` result, beginning with `knil`.  If no parses
;;> succeed returns `knil`.

(define (parse-fold f kons knil source . o)
  (let lp ((p (if (string? source) (string->parse-stream source) source))
           (index (if (pair? o) (car o) 0))
           (acc knil))
    (f p index (lambda (r s i fk) (lp s i (kons r acc))) (lambda (s i r) acc))))

;;> Parse as many of the parser combinator `f` from the parse
;;> stream `source`, starting at `index`, as possible, and
;;> return the result as a list.

(define (parse->list f source . o)
  (let ((index (if (pair? o) (car o) 0)))
    (reverse (parse-fold f cons '() source index))))

;;> As `parse->list` but requires the entire source be parsed
;;> with no left over characters, signalling an error otherwise.

(define (parse-fully->list f source . o)
  (let lp ((s (if (string? source) (string->parse-stream source) source))
           (index (if (pair? o) (car o) 0))
           (acc '()))
    (f s index
       (lambda (r s i fk)
         (if (eof-object? r) (reverse acc) (lp s i (cons r acc))))
       (lambda (s i reason) (error "incomplete parse")))))

;;> Return a new parser combinator with the same behavior as `f`,
;;> but on failure replaces the reason with `reason`.  This can be
;;> useful to provide more descriptive parse failure reasons when
;;> chaining combinators.  For example, `parse-string` just
;;> expects to parse a single fixed string.  If it were defined in
;;> terms of `parse-char`, failure would indicate some char
;;> failed to match, but it's more useful to describe the whole string
;;> we were expecting to see.

(define (parse-with-failure-reason f reason)
  (lambda (r s i fk)
    (f r s i (lambda (s i r) (fk s i reason)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;>| Basic Parsing Combinators
;;>
;;> Combinators to construct new parsers.

;;> Parse nothing successfully.

(define parse-epsilon
  (lambda (source index sk fk)
    (sk #t source index fk)))

;;> Parse any single character successfully.  Fails at end of input.

(define parse-anything
  (lambda (source index sk fk)
    (if (parse-stream-end? source index)
        (fk source index "end of input")
        (sk (parse-stream-ref source index)
            (parse-stream-next-source source index)
            (parse-stream-next-index source index)
            fk))))

;;> Always fail to parse.

(define parse-nothing
  (lambda (source index sk fk)
    (fk source index "nothing")))

;;> The disjunction combinator.  Returns the first combinator that
;;> succeeds parsing from the same source and index.

(define (parse-or f . o)
  (if (null? o)
      f
      (let ((g (apply parse-or o)))
        (lambda (source index sk fk)
          (let ((fk2 (lambda (s i r)
                       (g source index sk fk
                          ;; (lambda (s2 i2 r2)
                          ;;   (fk s2 i2 `(or ,r ,r2)))
                          ))))
            (f source index sk fk2))))))

;;> The conjunction combinator.  If both `f` and `g` parse
;;> successfully starting at the same source and index, returns the
;;> result of `g`.  Otherwise fails.

(define (parse-and f g)
  (lambda (source index sk fk)
    (f source index (lambda (r s i fk) (g source index sk fk)) fk)))

;;> The negation combinator.  If `f` succeeds, fails, otherwise
;;> succeeds with `#t`.

(define (parse-not f)
  (lambda (source index sk fk)
    (f source index (lambda (r s i fk) (fk s i "not"))
       (lambda (s i r) (sk #t source index fk)))))

(define (parse-seq-list o)
  (cond
   ((null? o)
    parse-epsilon)
   ((null? (cdr o))
    (let ((f (car o)))
      (lambda (s i sk fk)
        (f s i (lambda (r s i fk)
                 (sk (if (eq? r ignored-value) '() (list r)) s i fk))
           fk))))
   (else
    (let* ((f (car o))
           (o (cdr o))
           (g (car o))
           (o (cdr o))
           (g (if (pair? o)
                  (apply parse-seq g o)
                  (lambda (s i sk fk)
                    (g s i (lambda (r s i fk)
                             (sk (if (eq? r ignored-value) '() (list r))
                                 s i fk))
                       fk)))))
      (lambda (source index sk fk)
        (f source
           index
           (lambda (r s i fk)
             (g s i (lambda (r2 s i fk)
                      (let ((r2 (if (eq? r ignored-value) r2 (cons r r2))))
                        (sk r2 s i fk)))
                fk))
           fk))))))

;;> The sequence combinator.  Each combinator is applied in turn just
;;> past the position of the previous.  If all succeed, returns a list
;;> of the results in order, skipping any ignored values.

(define (parse-seq . o)
  (parse-seq-list o))

;;> Convert the list of parser combinators `ls` to a
;;> `parse-seq` sequence.

(define (list->parse-seq ls)
  (if (null? (cdr ls)) (car ls) (parse-seq-list ls)))

;;> The optional combinator.  Parse the combinator `f` (in
;;> sequence with any additional combinator args `o`), and return
;;> the result, or parse nothing successully on failure.

(define (parse-optional f . o)
  (if (pair? o)
      (parse-optional (apply parse-seq f o))
      (lambda (source index sk fk)
        (f source index sk (lambda (s i r) (sk #f source index fk))))))

(define ignored-value (list 'ignore))

;;> The repetition combinator.  Parse `f` repeatedly and return a
;;> list of the results.  `lo` is the minimum number of parses
;;> (deafult 0) to be considered a successful parse, and `hi` is
;;> the maximum number (default infinite) before stopping.

(define (parse-repeat f . o)
  (let ((lo (if (pair? o) (car o) 0))
        (hi (and (pair? o) (pair? (cdr o)) (cadr o))))
    (lambda (source0 index0 sk fk)
      (let repeat ((source source0) (index index0) (fk fk) (j 0) (res '()))
        (let ((fk (if (>= j lo)
                      (lambda (s i r) (sk (reverse res) source index fk))
                      fk)))
          (if (and hi (= j hi))
              (sk (reverse res) source index fk)
              (f source
                 index
                 (lambda (r s i fk) (repeat s i fk (+ j 1) (cons r res)))
                 fk)))))))

;;> Parse `f` one or more times.

(define (parse-repeat+ f)
  (parse-repeat f 1))

;;> Parse `f` and apply the procedure `proc` to the result on success.

(define (parse-map f proc)
  (lambda (source index sk fk)
    (f source index (lambda (res s i fk) (sk (proc res) s i fk)) fk)))

;;> Parse `f` and apply the procedure `proc` to the substring
;;> of the parsed data.  `proc` defaults to the identity.

(define (parse-map-substring f . o)
  (let ((proc (if (pair? o) (car o) (lambda (res) res))))
    (lambda (source index sk fk)
      (f source
         index
         (lambda (res s i fk)
           (sk (proc (parse-stream-substring source index s i)) s i fk))
         fk))))

;;> Parses the same streams as `f` but ignores the result on
;;> success.  Inside a `parse-seq` the result will not be
;;> included in the list of results.  Useful for discarding
;;> boiler-plate without the need for post-processing results.

(define (parse-ignore f)
  (parse-map f (lambda (res) ignored-value)))

;;> Parse with `f` and further require `check?` to return true
;;> when applied to the result.

(define (parse-assert f check?)
  (lambda (source index sk fk)
    (f source
       index
       (lambda (res s i fk)
         (if (check? res) (sk res s i fk) (fk s i "assertion failed")))
       fk)))

;;> Parse with `f` once and keep the first result, not allowing
;;> further backtracking within `f`.

(define (parse-atomic f)
  (lambda (source index sk fk)
    (f source index (lambda (res s i fk2) (sk res s i fk)) fk)))

;;> Parse with `f` once, keep the first result, and commit to the
;;> current parse path, discarding any prior backtracking options.
;;> Can optionally be passed a failure reason with which all resulting
;;> failure messages will be prefixed.

(define (parse-commit f . o)
  (let ((prefix (if (pair? o) (string-append (car o) ": ") "")))
    (lambda (source index sk fk)
      (let ((commit-fk (parse-stream-fk source)))
        (f
          source
          index
          (lambda (res s i fk)
            (sk res s i (lambda (s i r)
                          (commit-fk s i (string-append prefix r)))))
          fk)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;>| Boundary Checks
;;>
;;> Procedures for performing boundary checks within a parser combinator.

;;> Returns true iff `index` is the first index of the first parse
;;> stream `source`.

(define parse-beginning
  (lambda (source index sk fk)
    (if (parse-stream-start? source index)
        (sk #t source index fk)
        (fk source index "expected beginning"))))

;;> Returns true iff `index` is the last index of the last parse
;;> stream `source`.

(define parse-end
  (lambda (source index sk fk)
    (if (parse-stream-end? source index)
        (begin
          (sk #t
              (parse-stream-next-source source index)
              (parse-stream-next-index source index)
              fk))
      (fk source index "expected end"))))

;;> Returns true iff `source`, `index` indicate the beginning
;;> of a line (or the entire stream).

(define parse-beginning-of-line
  (lambda (source index sk fk)
    (let ((before (parse-stream-char-before source index)))
      (if (or (not before) (eqv? #\newline before))
          (sk #t source index fk)
          (fk source index "expected beginning of line")))))

;;> Returns true iff `source`, `index` indicate the end of a
;;> line (or the entire stream).

(define parse-end-of-line
  (lambda (source index sk fk)
    (if (or (parse-stream-end? source index)
            (eqv? #\newline (parse-stream-ref source index)))
        (sk #t source index fk)
        (fk source index "expected end of line"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;>| Constant Parsers
;;>
;;> Underlying combinators which parse a constant input and, contrary to
;;> the parsers documented above, cannot be passed parser combinators as
;;> procedure arguments.

(define (parse-char-pred pred)
  (lambda (source index sk fk)
    (let ((ch (parse-stream-ref source index)))
      (if (and (char? ch) (pred ch))
          (sk ch
              (parse-stream-next-source source index)
              (parse-stream-next-index source index)
              fk)
          (fk source index "failed char pred")))))

(define (x->char-predicate x)
  (cond
   ((char? x)
    (lambda (ch) (eqv? ch x)))
   ((char-set? x)
    (lambda (ch) (and (char? ch) (char-set-contains? x ch))))
   ((procedure? x)
    (lambda (ch) (and (char? ch) (x ch))))
   (else
    (error "don't know how to handle char predicate" x))))

;;> Parse a single char which matches `x`, which can be a
;;> character, character set, or arbitrary procedure.

(define (parse-char x)
  (parse-char-pred (x->char-predicate x)))

;;> Parse a single char which does not match `x`, which can be a
;;> character, character set, or arbitrary procedure.

(define (parse-not-char x)
  (let ((pred (x->char-predicate x)))
    (parse-char-pred (lambda (ch) (not (pred ch))))))

;;> Parse the exact string `str`.

(define (parse-string str)
  (parse-map (parse-with-failure-reason
              (parse-seq-list (map parse-char (string->list str)))
              (string-append "expected '" str "'"))
             list->string))

;;> Parse a sequence of characters matching `x` as with
;;> `parse-char`, and return the resulting substring.

(define (parse-token x)
  ;; (parse-map (parse-repeat+ (parse-char x)) list->string)
  ;; Tokens are atomic - we don't want to split them at any point in
  ;; the middle - so the implementation is slightly more complex than
  ;; the above.  With a sane grammar the result would be the same
  ;; either way, but this provides a useful optimization.
  (let ((f (parse-char x)))
    (lambda (source0 index0 sk fk)
      (let lp ((source source0) (index index0))
        (f source
           index
           (lambda (r s i fk) (lp s i))
           (lambda (s i r)
             (if (and (eq? source source0) (eqv? index index0))
                 (fk s i r)
                 (sk (parse-stream-substring source0 index0 source index)
                     source index fk))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;>| Laziness and Memoization
;;>
;;> [Lazy evaluation][wikipedia lazy] of parser combinators and [memoization][wikipedia memoization].
;;>
;;> [wikipedia lazy]: https://en.wikipedia.org/wiki/Lazy_evaluation
;;> [wikipedia memoization]: https://en.wikipedia.org/wiki/Memoization

;;> A delayed combinator.  This is equivalent to the parser combinator
;;> `f`, but is delayed so it can be more efficient if never used
;;> and `f` is expensive to compute.  Moreover, it can allow
;;> self-referentiality as in:
;;>
;;>    (letrec* ((f (parse-lazy (parse-or (parse-seq g f) h))))
;;>      ...)

(define-syntax parse-lazy
  (syntax-rules ()
    ((parse-lazy f)
     (let ((g (delay f)))
       (lambda (source index sk fk)
         ((force g) source index sk fk))))))

;; Utility definitions for memoization.

;; debugging
(define *procedures* '())
(define (procedure-name f)
  (cond ((assq f *procedures*) => cdr) (else #f)))
(define (procedure-name-set! f name)
  (set! *procedures* (cons (cons f name) *procedures*)))

(define memoized-failure (list 'failure))

;;> Parse the same strings as `f`, but memoize the result at each
;;> source and index to avoid exponential backtracking.  `name` is
;;> provided for debugging only.

(define (parse-memoize name f)
  ;;(if (not (procedure-name f)) (procedure-name-set! f name))
  (lambda (source index sk fk)
    (cond
     ((parse-stream-cache-cell source index f)
      => (lambda (cell)
           (if (and (pair? (cdr cell)) (eq? memoized-failure (cadr cell)))
               (fk source index (cddr cell))
               (apply sk (append (cdr cell) (list fk))))))
     (else
      (f source
         index
         (lambda (res s i fk)
           (parse-stream-cache-set! source index f (list res s i))
           (sk res s i fk))
         (lambda (s i r)
           (if (not (pair? (parse-stream-cache-cell source index f)))
               (parse-stream-cache-set!
                source index f (cons memoized-failure r)))
           (fk s i r)))))))
