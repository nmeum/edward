% Edward API
% author(s)
% date

# Edward Documentation

This is the API documentation for the extensible [edward][edward github] text editor.

## Libraries

The edward [CHICKEN][chicken web] [egg][chicken eggs] provides the following R7RS libraries:

* [(edward ed editor)][edward editor]: The central text editor abstraction.
* [(edward ed cmd)][edward cmd]: Marcos for defining new editor commands.
* [(edward ed posix)][edward posix]: The implementation of POSIX editor commands.
* [(edward ed addr)][edward addr]: Implementation of POSIX ed addressing.
* [(edward parse)][edward parse]: The edward parser combinator library.
* [(edward cli)][edward cli]: The command line interface abstraction.
* [(edward replace)][edward replace]: Implementation of regex replacements.
* [(edward util)][edward util]: Various utility procedures.
* [(edward buffer)][edward buffer]: Line buffer implementation used internally.

[chicken web]: https://call-cc.org
[chicken eggs]: https://wiki.call-cc.org/eggs
[edward github]: https://github.com/nmeum/edward
[r7rs scheme]: https://small.r7rs.org/
[edward editor]: edward.ed.editor.html
[edward cmd]: edward.ed.cmd.html
[edward addr]: edward.ed.addr.html
[edward posix]: edward.ed.posix.html
[edward cli]: edward.cli.html
[edward util]: edward.util.html
[edward parse]: edward.parse.html
[edward replace]: edward.replace.html
[edward buffer]: edward.buffer.html
