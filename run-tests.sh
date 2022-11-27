#!/bin/sh
set -e

export CHICKEN_REPOSITORY_PATH="$(pwd):${CHICKEN_REPOSITORY_PATH}"

banner() {
	printf "##\n# %s\n##\n\n" "${1}"
}

banner "Build"
chicken-install -n "$@"
echo

banner "Unit Tests"
chicken-install -n "$@" -test

banner "Integration Tests"
./tests/integration/run.sh

if [ -z "$TMUX" ] && command -v tmux 1>/dev/null 2>&1; then
	echo
	banner "Interactive Tests"
	./tests/interactive/run.sh
fi
