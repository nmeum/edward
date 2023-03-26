;;> This library implements the editor commands manadated by the
;;> [POSIX.1-2008 ed(1)][posix commands] specification. This is achieved
;;> by leveraging the [edward ed cmd][edward ed cmd] abstraction. The
;;> executor for all POSIX ed commands is exported and can be reused to
;;> implement new commands.
;;>
;;> [posix commands]: https://pubs.opengroup.org/onlinepubs/9699919799/utilities/ed.html#tag_20_38_13_03
;;> [edward ed cmd]: edward.ed.cmd.html

(define-library (edward.ed.posix)
  (import (scheme base)
          (scheme write)
          (scheme process-context)

          (srfi 1)
          (srfi 14)

          (posix-regex)

          (chicken process)
          (chicken string)

          (edward util)
          (edward parse)
          (edward replace)
          (edward buffer)
          (edward ed cmd)
          (edward ed addr)
          (edward ed editor))

  (export %exec-edit %exec-quit exec-append exec-change exec-command
          exec-copy exec-delete exec-edit exec-filename exec-global
          exec-help exec-insert exec-interactive exec-join
          exec-line-number exec-list exec-mark exec-move exec-null
          exec-number exec-print exec-prompt exec-quit exec-read
          exec-subst exec-undo exec-write)

  (include "posix.scm"))
