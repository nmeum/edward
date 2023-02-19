#!/bin/sh

cd "$(dirname "$0")"

EDWARD="${EDWARD:-$(pwd)/../../bin/edward}"
[ -x "${EDWARD}" ] || \
	abort "Couldn't find edward executable '${EDWARD}'."

session="ed-test"
cmd="sh -c 'tmux wait-for -S term-setup; env CHICKEN_REPOSITORY_PATH="${CHICKEN_REPOSITORY_PATH}" ${EDWARD}' && echo exit"

run_tmux() {
	tmux new-session -d -s "${session}" "${cmd}" \;                       \
		set-option -t "${session}" -w remain-on-exit "on" \;          \
		set-option -t "${session}" -w remain-on-exit-format "" \;     \
		                                                              \
		set-hook -w pane-died                                         \
			"wait-for -S exit-channel" \;                         \
		                                                              \
		wait-for term-setup \;                                        \
		run-shell -t "${session}" "$(pwd)/send.py ${session} ${1}" \; \
		                                                              \
		display-message -d 0 -t "${session}" "here" \;  \
		wait-for exit-channel \;                                      \
		capture-pane -S - -E - -t "${session}" -p -C
}

ret=0
for test in *; do
	[ -d "${test}" ] || continue

	name="${test##*/}"
	printf "Running test case '%s': " "${name}"

	run_tmux "$(pwd)/${test}/cmds" | awk '
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
