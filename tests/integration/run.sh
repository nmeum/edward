#!/bin/sh

cd "$(dirname "$0")"

abort() {
	printf "${1}\n" 1>&2
	exit 1
}

run_editor() {
	[ $# -eq 4 ] || exit 1
	cmd="${1}"
	cwd="${2}"
	dat="${3}"
	opt="${4}"

	mkdir "${cwd}"
	cp -r "${dat}" "${cwd}"
	cd "${cwd}"

	if [ -s "${opt}" ]; then
		set -- $(cat "${opt}")
		"${cmd}" $@
	else
		"${cmd}"
	fi

	cd - >/dev/null
	unset cmd cwd dat opt
}

EDWARD="${EDWARD:-$(pwd)/../../bin/edward}"
[ -x "${EDWARD}" ] || \
	abort "Couldn't find edward executable '${EDWARD}'."

REF_IMPL="$(command -v ed)"
[ -x "${REF_IMPL}" ] || \
	abort "Reference implementation '${REF_IMPL}' not found."

TESTDIR="/tmp/edward-test"
TESTCWD="${TESTDIR}/cwd"
EXPECTED="${TESTDIR}/expected"
ACTUAL="${TESTDIR}/actual"

mkdir -p "${TESTDIR}"
trap "rm -rf '${TESTDIR}'" INT EXIT

for test in *; do
	[ -d "${test}" ] || continue

	name="${test##*/}"
	printf "Running test case '%s': " "${name}"

	cmds="$(pwd)/${test}/cmds"
	opts="$(pwd)/${test}/opts"

	run_editor "${REF_IMPL}" "${TESTCWD}.expected" "${test}/testdata" "${opts}" \
		> "${EXPECTED}" < "${cmds}"
	run_editor "${EDWARD}" "${TESTCWD}.actual" "${test}/testdata" "${opts}" \
		> "${ACTUAL}" < "${cmds}"

	diff=$(diff -u "${EXPECTED}" "${ACTUAL}")
	if [ $? -ne 0 ]; then
		printf "FAIL: Standard output didn't match.\n\n"
		printf "%s\n" "${diff}"
		exit 1
	fi

	diff=$(diff -ur "${TESTCWD}.expected" "${TESTCWD}.actual")
	if [ $? -ne 0 ]; then
		printf "FAIL: Modified files differ.\n\n"
		printf "%s\n" "${diff}"
		exit 1
	fi

	printf "OK.\n"
	rm -rf "${TESTCWD}.actual" "${TESTCWD}.expected"
done
