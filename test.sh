#!/bin/bash

## GENERAL CONSTANTS
readonly BIN='./209poll'
readonly TMP='/tmp/209poll_test'

readonly SUCCESS=0
readonly FAILURE=84

readonly RED="\033[0;31m"


## COMPILATION
(make > /dev/null) || exit 1


## TESTS OUTPUT
readonly TESTS_PARAMS1="10000 500 42.24"
readonly TESTS_PARAMS2="10000 100 45"

readonly EXPECTED_RES1="examples/example1.txt"
readonly EXPECTED_RES2="examples/example2.txt"

$BIN $TESTS_PARAMS1 > $TMP
diff $TMP $EXPECTED_RES1
echo

$BIN $TESTS_PARAMS2 > $TMP
diff $TMP $EXPECTED_RES2
echo


## TESTS ERROR HANDLING
function print_err {
	echo -e $RED"$1"
}

function should_fail {
	$BIN $1 > /dev/null

	if [[ $? -ne $FAILURE ]]; then
		print_err "Should have failed: $1"
	fi
}

should_fail ""
should_fail "1000"
should_fail "1000 500"
should_fail "1000 500 50 100"
should_fail "abc 500 50"
should_fail "-1000 500 50"
should_fail "1000.5 500 50"
should_fail "0 0 50"
should_fail "1000 abc 50"
should_fail "1000 -500 50"
should_fail "1000 1500 50"
should_fail "1000 1500 abc"
should_fail "1000 1500 -"
should_fail "1000 1500 1."
should_fail "1000 1500 .5"
should_fail "1000 1500 150"


## CLEANUP
rm $TMP
