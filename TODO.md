# POSIX Compatibility

* Allow terminating the G command with SIGINT signal
* "Any number of addresses can be provided to commands taking addresses;"
	* Example: 1,2,3,4,5p
	* Example: 3;/foo/;+2p
* Allow omitting "delimiter of an RE or of a replacement string in a g, G, s, v, or V command"
	* s/s1/s2 → s/s1/s2/p
	* g/s1 → g/s1/p
	* ?si → ?si?

* align handling of end-of-file character with POSIX
	* Scheme's read-char procedure does not allow
	  reading past EOF but ed requires this
	* Problem: Reading a full UTF8 character, not a
	  byte with a POSIX API from CHICKEN is annoying
	* Using fgetsws(3) might be possible but requires
	  depending on SRFI 4 for u32vector to wrap wchar_t*
	* Implementing this might get easier with
	  https://wiki.call-cc.org/unicode-transition if the
	  transition adds an incremental UTF-8 decoder to CHICKEN
* make sure zero address is handled correctly everywhere
* implement asynchronous event section from POSIX.1-2008

# Build Process

* Improve compilation speed
* Figure out why edward is slower when compiled statically with eggs
* Figure out if the library/program distinction is actually useful
	* Maybe possible to extend edward with custom commands etc.
	  through the library but currently not supported
* Fix build on OpenBSD
	* matchable egg doesn't support OpenBSD
* Integration tests:
	* compare exit status
	* consider running GNU ed in traditional mode (-G)

# Miscellaneous

* Add more tests for addresses ranges with semicolon character
