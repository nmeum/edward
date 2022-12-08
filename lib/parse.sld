(define-library edward.parse
  (import (scheme base)
          (scheme char)
          (scheme file)
          (scheme write)

          (srfi 1)
          (srfi 14)

          (edward util)

          (chicken file posix)
          (chicken process signal))

  ;; parse.scm
  (export call-with-parse parse parse-fully parse-fold parse-failure
          parse->list parse-fully->list
          file->parse-stream string->parse-stream parse-stream-substring
          parse-stream-start? parse-stream-end? parse-stream-ref
          parse-anything parse-nothing parse-epsilon
          parse-seq parse-and parse-or parse-not list->parse-seq
          parse-repeat parse-repeat+ parse-optional
          parse-map parse-map-substring parse-ignore parse-assert
          parse-atomic parse-commit parse-memoize
          parse-char parse-not-char
          parse-string parse-token
          parse-beginning parse-end
          parse-beginning-of-line parse-end-of-line
          parse-beginning-of-word parse-end-of-word
          parse-word parse-word+
          parse-with-failure-reason
          make-parse-stream)

  ;; parse-util.scm
  (export parse-fail parse-bind parse-as-string parse-digits
          parse-default parse-ignore-optional parse-newline parse-blank
          parse-blanks+ parse-blanks parse-between parse-esc
          parse-strip-blanks parse-blanks-seq parse-line parse-alist
          parse-with-context parse-regex-lit* parse-regex-lit)

  ;; repl.scm
  (export make-repl repl? repl-run repl-interactive repl-prompt?  repl-set-prompt!)

  (include "parse/parse.scm"
           "parse/util.scm"
           "parse/repl.scm"))
