## Status

The goal of this project is creating an implementation of the
[`ed(1)`][ed posix] text editor, as defined in POSIX.1-2008, in R7RS
[CHICKEN Scheme][chicken]. All commands mandated by POSIX have been
implemented. However, this software is still in early stages of
development, expect bugs.

Known bugs and future ideas are documented in the `TODO.md` file.

## Design

This implementation relies on [parser combinators][parser combinators]
as well as [hygienic Scheme macros][hygienic macros] to ease the
implementation of ed commands. Each ed command is defined using a macro
(i.e. similar to how one would normally define procedures in Scheme) and
parsed through provided parser combinators. In a sense, thus employing
[language-oriented programming][language-oriented programming] to
implement ed commands and thereby making this ed implementation very
hackable and easy to extend. Presently, the implementation is also split
into a CHICKEN program and library component which may allow defining
custom commands through user-defined configuration files in the future.

## Installation

If a correctly configured CHICKEN toolchain is available run:

	$ chicken-install

This will compile `edward` and add the binary to your `$PATH`. If
creation of a statically linked and optimized binary is desired, pass
`-feature release`. This can be combined with the flags described below
to package this software for a distribution.

### Building without installing

For development setups, one can build `edward` as follows:

	$ export CHICKEN_REPOSITORY_PATH="${CHICKEN_REPOSITORY_PATH}:$(pwd)"
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

## Portability

The code was originally intended to be written in purely standard
confirming [R7RS Scheme][r7rs small]. However, as it turned out that
implementing `ed(1)` properly requires access to some POSIX functions
(which are not standardized in R7RS or any [SRFI][srfi]) this approach
was eventually abandoned.

Presently, the following CHICKEN Scheme specific stuff is used:

* The CHICKEN [matchable][chicken matchable] egg for pattern matching
  ([SRFI 204][srfi 204] is supposed to standardize pattern matching
  but is still in draft status).
* The CHICKEN Foreign Function Interface (FFI) for accessing POSIX
  functions required by `ed(1)` (e.g. `popen(3)`, `isatty(3)` and
  `regexec(3)`).
* The [`chicken process signal`][chicken process signal] is required
  to setup signal handlers for SIGINT, SIGHUP, and SIGQUIT.
* It is assumed that `eq?` performs pointer comparison for strings
  (undefined behaviour in R7RS but implemented in CHICKEN).
* It is assumed that `open-output-file` will truncate existing files
  (unspecified in R7RS but implemented in CHICKEN).

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

[ed posix]: https://pubs.opengroup.org/onlinepubs/009695399/utilities/ed.html
[chicken]: https://call-cc.org
[chicken matchable]: https://wiki.call-cc.org/eggref/5/matchable
[chicken process signal]: https://api.call-cc.org/5/doc/chicken/process/signal
[gnu ed]: https://www.gnu.org/software/ed/
[srfi]: https://srfi.schemers.org/
[srfi 204]: https://srfi.schemers.org/srfi-204/
[r7rs small]: https://small.r7rs.org/
[parser combinators]: https://en.wikipedia.org/wiki/Parser_combinator
[GNU readline]: https://tiswww.cwru.edu/php/chet/readline/rltop.html
[rlwrap github]: https://github.com/hanslub42/rlwrap
[unix v7vol2a]: https://s3.amazonaws.com/plan9-bell-labs/7thEdMan/v7vol2a.pdf
[hygienic macros]: https://doi.org/10.1145/319838.319859
[language-oriented programming]: https://doi.org/10.1145/3127323
