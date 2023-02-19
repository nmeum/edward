#!/usr/bin/env python3
#
# Send keys, defined in an input file, to an existing tmux session.
# This script is written in Python to avoid shell word splitting issues.

import sys
import itertools
import subprocess


def parse_line(line):
    length = len(line)
    if length == 0:
        raise ValueError("input string must not be empty")

    if line[-1] == '\n':
        if length > 1 and line[-2] == '\\':
            return [line[0:-2].strip()] # TODO: remove .strip()
        else:
            return [line[0:-1], 'Enter']

    raise ValueError("line not newline-terminated")

def get_keys(file):
    return itertools.chain.from_iterable(map(parse_line, file.readlines()))

def send_keys(session, keys):
    for key in keys:
        subprocess.run(["tmux", "send-keys", "-t", session, key])

def main():
    if len(sys.argv) <= 2:
        print(f"Usage: {sys.argv[0]} SESSION FILE", file=sys.stderr)
        sys.exit(1)

    session  = sys.argv[1]
    filepath = sys.argv[2]

    with open(filepath, 'r') as f:
        keys = get_keys(f)
        send_keys(session, keys)


if __name__ == "__main__":
    main()
