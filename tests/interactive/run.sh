#!/bin/sh

cd "$(dirname "$0")"

if [ -n "$TMUX" ] || ! command -v tmux 1>/dev/null 2>&1; then
	echo "Skipping interactive tests" 1>&2
	exit 0
fi

EDWARD="${EDWARD:-$(pwd)/../../bin/edward}"
[ -x "${EDWARD}" ] || \
	abort "Couldn't find edward executable '${EDWARD}'."

session="ed-test"
cmd="sh -c 'stty -echo && tmux wait-for -S term-setup; env CHICKEN_REPOSITORY_PATH="${CHICKEN_REPOSITORY_PATH}" ${EDWARD}'"

read_cmd() {
	[ $# -eq 0 ] || return 1

	# Terminate all lines with an Enter key unless the line escapes
	# the newline with a '\' which is useful for sending control keys.
	sed -e 's/\([^\]\)$/\1\nEnter/' -e 's/\\$//'
}

run_tmux() {
	tmux new-session -d -s "${session}" "${cmd}" \;                   \
		set-option -t "${session}" -w remain-on-exit "on" \;      \
		set-option -t "${session}" -w remain-on-exit-format "" \; \
		                                                          \
		set-hook -w pane-died                                     \
			"wait-for -S exit-channel" \;                     \
		                                                          \
		wait-for term-setup \;                                    \
		send-keys -t "${session}" $(read_cmd) \;                  \
		                                                          \
		wait-for exit-channel \;                                  \
		capture-pane -S - -E - -t "${session}" -p -C
}

ret=0
for test in *; do
	[ -d "${test}" ] || continue

	name="${test##*/}"
	printf "Running test case '%s': " "${name}"

	run_tmux < "${test}/cmds" | awk '
	# Filter out all empty lines between the last non-empty line and EOF.
	{
		output = output $0
		if (length($0) != 0) {
			print(output)
			output = ""
		}
	}
	' | cmp -s "${test}/expected" -

	if [ $? -eq 0 ]; then
		printf "OK\n"
	else
		printf "FAIL\n"
		ret=1
	fi

	tmux kill-session -t "${session}"
done

exit "${ret}"
