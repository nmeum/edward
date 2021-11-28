## Status

The goal of this project is creating an implementation of the
[`ed(1)`][ed posix] text editor, as defined in POSIX.1-2008, in R7RS
[CHICKEN Scheme][chicken]. The majority of ed commands have been
implemented. The following commands are still missing:

* Undo Command
* Global Command
* Interactive Global Command
* Global Non-Matched Command
* Interactive Global Non-Matched Command

Apart from commands, some behaviour mandated by the standard is
currently not implemented. For example, handling of asynchronous events
(i.e.  signals). Nonetheless, the present implementation should already
suffice for a lot of interactive and scripting use cases. For instance,
it should already be possible to use this software to apply patches in
`diff -e` format.

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
ed implementation. For instance, [GNU ed][gnu ed]. The reference
implementation is determined using the `REF_IMPL` environment variable.

Both unit and integration tests can be run using:

	$ ./run-tests.sh

## Portability

The code is mostly written in standard [R7RS Scheme][r7rs small].
However, it presently uses [CHICKEN Scheme][chicken] specific code for
pattern matching. This code will be replaced by [SRFI 204][srfi 204] in
the near future. Furthermore, the code base relies heavily on a FFI for
`popen(3)`, `isatty(3)` and `regexec(3)`. Since there is no finalized
SRFI standardising a Scheme FFI, this code is presently
implementation-specific and currently only implemented for CHICKEN.

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
