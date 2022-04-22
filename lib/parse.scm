;; parse.scm -- Parser Combinators
;; Copyright (c) 2013 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;> \section{Parse Streams}

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

;;> Create a parse stream open on the given \var{filename}, with a
;;> possibly already opened \var{port}.

(define (make-parse-stream filename . o)
  (let ((port (if (pair? o) (car o) (open-input-file filename)))
        (len (if (and (pair? o) (pair? (cdr o))) (cadr o) default-buffer-size)))
    (%make-parse-stream
     filename port (make-vector len #f) (make-vector len '()) 0 #f 0 0 #f #f)))

;;> Open \var{filename} and create a parse stream on it.

(define (file->parse-stream filename)
  (make-parse-stream filename (open-input-file filename)))

;;> Create a parse stream on a string \var{str}.

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
        (buf (parse-stream-buffer source)))
    (if (<= off i)
        (do ((off off (+ off 1)))
            ((> off i) (parse-stream-offset-set! source off))
          (vector-set! buf off (read-char (parse-stream-port source))))
        #f)))

;;> Returns true iff \var{i} is the first character position in the
;;> parse stream \var{source}.

(define (parse-stream-start? source i)
  (and (zero? i) (not (parse-stream-prev-char source))))

;;> Returns true iff \var{i} is the last character position in the
;;> parse stream \var{source}.

(define (parse-stream-end? source i)
  (eof-object? (parse-stream-ref source i)))

;;> Returns the character in parse stream \var{source} indexed by
;;> \var{i}.

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
  (close-input-port (parse-stream-port source)))

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
;;> stream \var{s0} index \var{i0} (inclusive), and ending at \var{s1}
;;> index \var{i1} (exclusive).

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

;;> \section{Parser Interface}

;;> Combinator to indicate failure.

(define (parse-failure s i reason)
  (let ((line+col (parse-stream-debug-info s i)))
    (error "incomplete parse at" (append line+col (list reason)))))

;;> Call the parser combinator \var{f} on the parse stream
;;> \var{source}, starting at index \var{index}, passing the result to
;;> the given success continuation \var{sk}, which should be a
;;> procedure of the form \scheme{(result source index fail)}.  The
;;> optional failure continuation should be a procedure of the form
;;> \scheme{(source index reason)}, and defaults to just returning
;;> \scheme{#f}.

(define (call-with-parse f source index sk . o)
  (let ((s (if (string? source) (string->parse-stream source) source))
        (fk (if (pair? o) (car o) (lambda (s i reason) #f))))
    (parse-stream-fk-set! s fk)
    (f s index sk fk)))

;;> Call the parser combinator \var{f} on the parse stream
;;> \var{source}, at index \var{index}, and return the result, or
;;> \scheme{#f} if parsing fails.

(define (parse f source . o)
  (let ((index (if (pair? o) (car o) 0)))
    (call-with-parse f source index (lambda (r s i fk) r))))

;;> Call the parser combinator \var{f} on the parse stream
;;> \var{source}, at index \var{index}.  If the entire source is not
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
;;> combinator \var{f} to \var{source}, starting at \var{index}, as
;;> long as a valid parse is found.  On each successful parse applies
;;> the procedure \var{kons} to the parse result and the previous
;;> \var{kons} result, beginning with \var{knil}.  If no parses
;;> succeed returns \var{knil}.

(define (parse-fold f kons knil source . o)
  (let lp ((p (if (string? source) (string->parse-stream source) source))
           (index (if (pair? o) (car o) 0))
           (acc knil))
    (f p index (lambda (r s i fk) (lp s i (kons r acc))) (lambda (s i r) acc))))

;;> Parse as many of the parser combinator \var{f} from the parse
;;> stream \var{source}, starting at \var{index}, as possible, and
;;> return the result as a list.

(define (parse->list f source . o)
  (let ((index (if (pair? o) (car o) 0)))
    (reverse (parse-fold f cons '() source index))))

;;> As \scheme{parse->list} but requires the entire source be parsed
;;> with no left over characters, signalling an error otherwise.

(define (parse-fully->list f source . o)
  (let lp ((s (if (string? source) (string->parse-stream source) source))
           (index (if (pair? o) (car o) 0))
           (acc '()))
    (f s index
       (lambda (r s i fk)
         (if (eof-object? r) (reverse acc) (lp s i (cons r acc))))
       (lambda (s i reason) (error "incomplete parse")))))

;;> Return a new parser combinator with the same behavior as \var{f},
;;> but on failure replaces the reason with \var{reason}.  This can be
;;> useful to provide more descriptive parse failure reasons when
;;> chaining combinators.  For example, \scheme{parse-string} just
;;> expects to parse a single fixed string.  If it were defined in
;;> terms of \scheme{parse-char}, failure would indicate some char
;;> failed to match, but it's more useful to describe the whole string
;;> we were expecting to see.

(define (parse-with-failure-reason f reason)
  (lambda (r s i fk)
    (f r s i (lambda (s i r) (fk s i reason)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;> \section{Basic Parsing Combinators}

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

;;> The conjunction combinator.  If both \var{f} and \var{g} parse
;;> successfully starting at the same source and index, returns the
;;> result of \var{g}.  Otherwise fails.

(define (parse-and f g)
  (lambda (source index sk fk)
    (f source index (lambda (r s i fk) (g source index sk fk)) fk)))

;;> The negation combinator.  If \var{f} succeeds, fails, otherwise
;;> succeeds with \var{#t}.

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

;;> Convert the list of parser combinators \var{ls} to a
;;> \scheme{parse-seq} sequence.

(define (list->parse-seq ls)
  (if (null? (cdr ls)) (car ls) (parse-seq-list ls)))

;;> The optional combinator.  Parse the combinator \var{f} (in
;;> sequence with any additional combinator args \var{o}), and return
;;> the result, or parse nothing successully on failure.

(define (parse-optional f . o)
  (if (pair? o)
      (parse-optional (apply parse-seq f o))
      (lambda (source index sk fk)
        (f source index sk (lambda (s i r) (sk #f source index fk))))))

(define ignored-value (list 'ignore))

;;> The repetition combinator.  Parse \var{f} repeatedly and return a
;;> list of the results.  \var{lo} is the minimum number of parses
;;> (deafult 0) to be considered a successful parse, and \var{hi} is
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

;;> Parse \var{f} one or more times.

(define (parse-repeat+ f)
  (parse-repeat f 1))

;;> Parse \var{f} and apply the procedure \var{proc} to the result on success.

(define (parse-map f proc)
  (lambda (source index sk fk)
    (f source index (lambda (res s i fk) (sk (proc res) s i fk)) fk)))

;;> Parse \var{f} and apply the procedure \var{proc} to the substring
;;> of the parsed data.  \var{proc} defaults to the identity.

(define (parse-map-substring f . o)
  (let ((proc (if (pair? o) (car o) (lambda (res) res))))
    (lambda (source index sk fk)
      (f source
         index
         (lambda (res s i fk)
           (sk (proc (parse-stream-substring source index s i)) s i fk))
         fk))))

;;> Parses the same streams as \var{f} but ignores the result on
;;> success.  Inside a \scheme{parse-seq} the result will not be
;;> included in the list of results.  Useful for discarding
;;> boiler-plate without the need for post-processing results.

(define (parse-ignore f)
  (parse-map f (lambda (res) ignored-value)))

;;> Parse with \var{f} and further require \var{check?} to return true
;;> when applied to the result.

(define (parse-assert f check?)
  (lambda (source index sk fk)
    (f source
       index
       (lambda (res s i fk)
         (if (check? res) (sk res s i fk) (fk s i "assertion failed")))
       fk)))

;;> Parse with \var{f} once and keep the first result, not allowing
;;> further backtracking within \var{f}.

(define (parse-atomic f)
  (lambda (source index sk fk)
    (f source index (lambda (res s i fk2) (sk res s i fk)) fk)))

;;> Parse with \var{f} once, keep the first result, and commit to the
;;> current parse path, discarding any prior backtracking options.

(define (parse-commit f)
  (lambda (source index sk fk)
    (f source index (lambda (res s i fk) (sk res s i (parse-stream-fk source))) fk)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;> \section{Boundary Checks}

;;> Returns true iff \var{index} is the first index of the first parse
;;> stream \var{source}.

(define parse-beginning
  (lambda (source index sk fk)
    (if (parse-stream-start? source index)
        (sk #t source index fk)
        (fk source index "expected beginning"))))

;;> Returns true iff \var{index} is the last index of the last parse
;;> stream \var{source}.

(define parse-end
  (lambda (source index sk fk)
    (if (parse-stream-end? source index)
        (sk #t source index fk)
      (fk source index "expected end"))))

;;> Returns true iff \var{source}, \var{index} indicate the beginning
;;> of a line (or the entire stream).

(define parse-beginning-of-line
  (lambda (source index sk fk)
    (let ((before (parse-stream-char-before source index)))
      (if (or (not before) (eqv? #\newline before))
          (sk #t source index fk)
          (fk source index "expected beginning of line")))))

;;> Returns true iff \var{source}, \var{index} indicate the end of a
;;> line (or the entire stream).

(define parse-end-of-line
  (lambda (source index sk fk)
    (if (or (parse-stream-end? source index)
            (eqv? #\newline (parse-stream-ref source index)))
        (sk #t source index fk)
        (fk source index "expected end of line"))))

(define (char-word? ch)
  (or (char-alphabetic? ch) (eqv? ch #\_)))

;;> Returns true iff \var{source}, \var{index} indicate the beginning
;;> of a word (or the entire stream).

(define parse-beginning-of-word
  (lambda (source index sk fk)
    (let ((before (parse-stream-char-before source index)))
      (if (and (or (not before) (not (char-word? before)))
               (not (parse-stream-end? source index))
               (char-word? (parse-stream-ref source index)))
          (sk #t source index fk)
          (fk source index "expected beginning of word")))))

;;> Returns true iff \var{source}, \var{index} indicate the end of a
;;> word (or the entire stream).

(define parse-end-of-word
  (lambda (source index sk fk)
    (let ((before (parse-stream-char-before source index)))
      (if (and before
               (char-word? before)
               (or (parse-stream-end? source index)
                   (not (char-word? (parse-stream-ref source index)))))
          (sk #t source index fk)
          (fk source index "expected end of word")))))

;;> Parse the combinator \var{word} (default a \scheme{parse-token} of
;;> \scheme{char-alphabetic?} or underscores), ensuring it begins and
;;> ends on a word boundary.

(define (parse-word . o)
  (let ((word (if (pair? o) (car o) (parse-token char-word?))))
    (lambda (source index sk fk)
      (parse-map
       (parse-seq parse-beginning-of-word
                  word
                  parse-end-of-word)
       cadr))))

;;> As \scheme{parse-word}, but instead of an arbitrary word
;;> combinator takes a character predicate \var{pred} (conjoined with
;;> \scheme{char-alphabetic?} or underscore), and parses a sequence of
;;> those characters with \scheme{parse-token}.  Returns the parsed
;;> substring.

(define (parse-word+ . o)
  (let ((pred (if (pair? o)
                  (lambda (ch) (and (char-word? ch) ((car o) ch)))
                  char-word?)))
    (parse-word (parse-token pred))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;> \section{Constant Parsers}

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

;;> Parse a single char which matches \var{x}, which can be a
;;> character, character set, or arbitrary procedure.

(define (parse-char x)
  (parse-char-pred (x->char-predicate x)))

;;> Parse a single char which does not match \var{x}, which can be a
;;> character, character set, or arbitrary procedure.

(define (parse-not-char x)
  (let ((pred (x->char-predicate x)))
    (parse-char-pred (lambda (ch) (not (pred ch))))))

;;> Parse the exact string \var{str}.

(define (parse-string str)
  (parse-map (parse-with-failure-reason
              (parse-seq-list (map parse-char (string->list str)))
              (string-append "expected '" str "'"))
             list->string))

;;> Parse a sequence of characters matching \var{x} as with
;;> \scheme{parse-char}, and return the resulting substring.

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
