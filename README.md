# edward

An implementation of the [`ed(1)`][ed posix] text editor in [CHICKEN Scheme][chicken].

## Status

The basic structure works more or less, however, only a few commands
have been implemented so far. Ideally, all commands mandated by
POSIX.1-2008 should be implemented at some point. Furthermore, it should
also be possible to define custom commands in the future.

## Installation

If a correctly configured chicken toolchain is available run:

	$ chicken-install

This will compile `edward` and add the binary to your `$PATH`.

### Building without installing

For development setups, one can build `edward` as follows:

	$ export CHICKEN_REPOSITORY_PATH="${CHICKEN_REPOSITORY_PATH}:$(pwd)"
	$ chicken-install -n

This will create an executable binary in `./bin/edward`.

## Tests

This repository contains both unit tests and integration tests. The
latter require a reference implementation of a POSIX.1-2008 compatible
ed implementation. For instance, [GNU ed][gnu ed]. The reference
implementation is determined from the `REF_IMPL` environment variable.

Both unit and integration tests can be run using:

	$ ./run-tests.sh

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
