# Interactive tests

The test cases in this directory covers behavior which requires standard input to be a tty.
For example, EOF in input mode or command mode.
The tests require `tmux` to be installed.
Contrary to the default integration tests, these tests require an `expected` output file.
