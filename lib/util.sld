;;> This library provides various generic utility procedures.

(define-library (edward.util)
  (import (scheme base)
          (scheme write)
          (scheme file)
          (scheme process-context)

          (srfi 1)

          (chicken foreign)
          (only (chicken base) assert)
          (only (chicken fixnum) fx-)
          (only (chicken condition) condition-case)
          (only (chicken port) terminal-port? terminal-size)
          (only (chicken file posix) file-stat file-read port->fileno)
          (only (chicken io) read-lines))

  (export inc dec id alist-values fprintln println empty-string?
          pad-string string->human-readable path-join user-home
          count-bytes lines->port port->lines file-read-char)

  (include "util.scm"))
