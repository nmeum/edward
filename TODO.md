# POSIX Compatibility

* Fix use of substitute command with global command
	* Global command executes substitute command
	  for each selected line
	* If the selected line doesn't match the substitute
	  command an error is raised (as per POSIX)
	* How is this supposed to be handled?
* Figure out how multiline substitute commands are supposed to be
  handled in global command
	* Both use escaped newlines: The global command uses them
	  in the command list, the substitute command uses them
	  to allow for multiline replacements
	* How is this supposed to be handled?
* Global command: "The '.' terminating input mode can be omitted if it
  would be the last line of the command list."
* Allow terminating the G command with SIGINT signal
* "Any number of addresses can be provided to commands taking addresses;"
	* Example: 1,2,3,4,5p
	* Example: 3;/foo/;+2p

* align handling of end-of-file character with POSIX
	* Scheme's read-char procedure does not allow
	  reading past EOF but ed requires this
* make sure zero address is handled correctly everywhere
* implement asynchronous event section from POSIX.1-2008

# Build Process

* Improve compilation speed
* Figure out why edward is slower when compiled statically with eggs
* Figure out if the library/program distinction is actually useful
	* Maybe possible to extend edward with custom commands etc.
	  through the library but currently not supported
* Integration tests:
	* compare exit status
	* consider running GNU ed in traditional mode (-G)

# Miscellaneous

* Add more tests for addresses ranges with semicolon character
