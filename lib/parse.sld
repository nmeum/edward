;;> This is a stripped-down and minimally modified version of [chibi parse][chibi parse],
;;> a parser combinator library with optional memoization. This version of the library
;;> is specifically designed to faciliate building read-eval-print loops with parser
;;> combinators.
;;>
;;> Apart from [parse streams][stream section], the central abstraction of this libary
;;> is therefore a [REPL][repl section] type which continously reads data from standard
;;> input and parses this data with parsers constructed using provided parser combinators.
;;>
;;> An exemplary parser may be constructed as follows:
;;>
;;> ```
;;> (import (srfi 14) (edward parse))
;;>
;;> (define hexadecimal
;;>   (parse-map
;;>     (parse-seq
;;>       (parse-string "0x")
;;>       (parse-map
;;>         (parse-token char-set:hex-digit)
;;>         (lambda (str)
;;>           (string->number str 16))))
;;>     cadr))
;;> ```
;;>
;;> This parser recognizes hexademical integers using the character set definition
;;> provided by [SRFI 14][srfi 14] and transform these characters into a Scheme
;;> number type using [parse-map][parse-map]. Refer to the documentation below
;;> for more information on available combinators.
;;>
;;> [chibi parse]: https://synthcode.com/scheme/chibi/lib/chibi/parse.html
;;> [stream section]: #section-parse-streams
;;> [repl section]: #section-read–eval–print-loop
;;> [srfi 14]: https://srfi.schemers.org/srfi-14/srfi-14.html
;;> [parse-map]: #parse-map

(define-library (edward.parse)
  (import (scheme base)
          (scheme char)
          (scheme file)
          (scheme write)

          (srfi 1)
          (srfi 14)

          (edward util)

          (chicken file posix)
          (chicken process signal))

  ;; repl.scm
  (export make-repl repl? repl-run repl-interactive repl-prompt?  repl-set-prompt!)

  ;; parse.scm
  (export call-with-parse parse parse-fully parse-fold parse-failure
          parse->list parse-fully->list
          file->parse-stream string->parse-stream parse-stream-substring
          parse-stream-start? parse-stream-end? parse-stream-ref
          parse-anything parse-nothing parse-epsilon
          parse-seq parse-and parse-or parse-not list->parse-seq
          parse-repeat parse-repeat+ parse-optional
          parse-map parse-map-substring parse-ignore parse-assert
          parse-atomic parse-commit parse-lazy parse-memoize
          parse-char parse-not-char
          parse-string parse-token
          parse-beginning parse-end
          parse-beginning-of-line parse-end-of-line
          parse-with-failure-reason
          make-parse-stream)

  ;; parse-util.scm
  (export parse-fail parse-bind parse-as-string parse-digits parse-lowercase
          parse-default parse-newline parse-blank
          parse-blanks+ parse-blanks parse-between parse-esc
          parse-strip-blanks parse-blanks-seq parse-line parse-alist
          parse-with-context parse-regex-lit* parse-regex-lit)

  (include "parse/repl.scm"
           "parse/parse.scm"
           "parse/util.scm"))
