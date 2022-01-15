## Status

The goal of this project is creating an implementation of the
[`ed(1)`][ed posix] text editor, as defined in POSIX.1-2008, in R7RS
[CHICKEN Scheme][chicken]. All ed commands, mandated by POSIX, have been
implemented but some command implementations are not fully compatible
with the POSIX standard yet.

Apart from commands, some behaviour mandated by the standard is
currently not implemented. For example, handling of asynchronous events
(i.e. signals). Furthermore, the implementation hasn't been extensively
tested so far and thus bugs should be expected. Nonetheless, the present
implementation should already suffice for a lot of interactive and
scripting use cases.

Known bugs and future ideas are documented in the `TODO.txt` file.

## Design

This implementation relies on [parser combinators][parser combinators]
as well as hygienic Scheme macros to ease the implementation of ed
commands. Each ed command is defined using a macro (i.e. similar to how
one would normally define procedures in Scheme) and parsed through
provided parser combinators. Thereby making this ed implementation very
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
ed implementation. Currently, [GNU ed][gnu ed] is used for this purpose.

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
by Brian W. Kernighan and Rob Pike (Appendix 1: Editor Summary).

## Portability

The code is mostly written in standard [R7RS Scheme][r7rs small].
However, it presently uses [CHICKEN Scheme][chicken] specific code for
pattern matching. This code will be replaced by [SRFI 204][srfi 204] in
the near future. Furthermore, the code base relies heavily on a FFI for
`popen(3)`, `isatty(3)` and `regexec(3)`. Since there is no finalized
SRFI standardising a Scheme FFI, this code is presently
implementation-specific and currently only implemented for CHICKEN.
Furthermore, the implementation requires an `eq?` implementation which
performs pointer comparison (undefined behaviour in R7RS but implemented
in CHICKEN).

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
[gnu ed]: https://www.gnu.org/software/ed/
[srfi 204]: https://srfi.schemers.org/srfi-204/
[r7rs small]: https://srfi.schemers.org/srfi-204/
[parser combinators]: https://en.wikipedia.org/wiki/Parser_combinator
[GNU readline]: https://tiswww.cwru.edu/php/chet/readline/rltop.html
[rlwrap github]: https://github.com/hanslub42/rlwrap
