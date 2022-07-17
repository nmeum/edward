## About

This is a largely POSIX-compatible implementation of the standard Unix
text editor [`ed(1)`][ed posix]. The editor is written entirely in
[R7RS][r7rs] [CHICKEN Scheme][chicken] and provides a Scheme library
interface for extending the editor with custom commands.

## Status

Regarding use of `edward` as an implementation of `ed(1)`, as defined in
POSIX.1-2008, I am currently not aware of any POSIX conformance issues
(at least for non-interactive usage). However, the library interface
for extending `edward` is still very experimental and subject to change.

Known bugs and limitations are documented in the `TODO.md` file.

## Design

This implementation relies on [parser combinators][parser combinators]
as well as [hygienic Scheme macros][hygienic macros] to ease the
implementation of ed commands. Each ed command is defined using a macro
(i.e. similar to how one would normally define procedures in Scheme) and
parsed through provided parser combinators. In a sense, thus employing
[language-oriented programming][language-oriented programming] to
implement ed commands and thereby making this ed implementation very
hackable and easy to extend. The implementation is also split into a
CHICKEN program and various library components which allows defining
custom commands (refer to the library interface documentation below).

## Installation

If a correctly configured CHICKEN 5.3 toolchain is available run:

	$ chicken-install

This will compile `edward` and add the binary to your `$PATH`.

### Building without installing

For development setups, one can build `edward` as follows:

	$ export CHICKEN_REPOSITORY_PATH="$(pwd):${CHICKEN_REPOSITORY_PATH}"
	$ chicken-install -n

This will create an executable binary in `./bin/edward`.

## Tests

This repository contains both unit tests and integration tests. The
latter require a reference implementation of a POSIX.1-2008 compatible
ed implementation. Currently, [GNU ed >= 1.18][gnu ed] is used for this
purpose.

Both unit and integration tests can be run using:

	$ ./run-tests.sh

## Usage

For interactive usage I can highly recommend using this software in
conjunction with a [readline][GNU readline] frontend such as
[rlwrap][rlwrap github]. This enables readline-like keybindings (e.g.
Ctrl+A, Ctrl+W, …) as well as input history support.

Detailed usage instructions for the `ed(1)` text editor can be found in
the [POSIX documentation][ed posix]. Additionally, a nice introduction
to the editor is provided in the book *The Unix Programming Environment*
by Brian W. Kernighan and Rob Pike (Appendix 1: Editor Summary). In
general, since ed is an old editor which hasn't changed much in the past
centuries, many historic documents explaining its usage are still
applicable today. Most notably, volume 2A of the seventh edition of the
[UNIX Programmer's Manual][unix v7vol2a] contains two documents which
provide a beginner-friendly introduction to the editor: *A Tutorial
Introduction to the UNIX Text Editor* and *Advanced Editing on UNIX*
both written by Brian W. Kernighan.

## Library Interface

Apart from an `ed(1)` implementation, `edward` also provides a library
interface for extending the editor with custom commands. While the
implementation provided here focuses solely on POSIX compatibility,
extension to the POSIX standard can be supplied separately. The library
interface is still in early stages of development, not well documented,
and subject to change.

The library interface can be used by creating a custom CHICKEN Scheme
program which imports the required `edward` libraries, defines custom
commands through provided hygienic macros, and executes `(edward-main)`
to start the editor. For example, an `edward` variant which also
provides the `z` command (included with many BSD implementations of
`ed(1)`) could be implemented as follows:

```scheme
(import (scheme base)
        (chicken port)

        (edward cli)
        (edward parse)
        (edward ed cmd)
        (edward ed addr)
        (edward ed editor))

;; Executor for the 'z' command.
(define (exec-scroll editor amount)
  (let* ((start  (addr->line editor (make-addr '(current-line))))
         (end    (addr->line editor (make-addr
                                      (cons 'nth-line
                                            (min
                                              (editor-lines editor)
                                              (+ start amount)))))))
    (exec-print editor (cons start end))))

;; Parser for the 'z' command.
(define-edit-cmd (scroll exec-scroll)
  (parse-cmd-char #\z)
  (parse-default
    parse-digits
    (let*-values (((port) (current-output-port))
                  ((rows _) (if (terminal-port? port)
                              (terminal-size port)
                              (values 22 72))))
      rows)))

;; Start the editor
(edward-main)
```

Save this code in `edward++.scm` and compile it as follows:

	$ csc -R r7rs edward++.scm

Afterwards, drop the resulting `edward++` binary somewhere in your
`$PATH` and invoke it as you normally would invoke `edward` and enjoy
scrolling through text with the new `z` command.

## Portability

The code was originally intended to be written in purely standard
conforming [R7RS Scheme][r7rs]. However, it turned out that
implementing `ed(1)` properly requires access to several POSIX functions
which are not standardized in R7RS or any [SRFI][srfi]. Initially, it
was attempted to overcome this limitation via a custom Foreign Function
Interface (FFI). Unfortunately, there is also no SRFI standardizing a
Scheme FFI and as such it was difficult to support multiple
implementations with the FFI. For this reason, this approach was
eventually abandoned and the code now uses several CHICKEN extensions
directly.

## License

This program is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
Public License for more details.

You should have received a copy of the GNU General Public License along
with this program. If not, see <https://www.gnu.org/licenses/>.

[ed posix]: https://pubs.opengroup.org/onlinepubs/9699919799/utilities/ed.html
[chicken]: https://call-cc.org
[chicken matchable]: https://wiki.call-cc.org/eggref/5/matchable
[chicken posix-regex]: https://wiki.call-cc.org/eggref/5/posix-regex
[chicken process signal]: https://api.call-cc.org/5/doc/chicken/process/signal
[gnu ed]: https://www.gnu.org/software/ed/
[srfi]: https://srfi.schemers.org/
[srfi 204]: https://srfi.schemers.org/srfi-204/
[r7rs]: https://small.r7rs.org/
[parser combinators]: https://en.wikipedia.org/wiki/Parser_combinator
[GNU readline]: https://tiswww.cwru.edu/php/chet/readline/rltop.html
[rlwrap github]: https://github.com/hanslub42/rlwrap
[unix v7vol2a]: https://s3.amazonaws.com/plan9-bell-labs/7thEdMan/v7vol2a.pdf
[hygienic macros]: https://doi.org/10.1145/319838.319859
[language-oriented programming]: https://doi.org/10.1145/3127323
