#!/bin/bash

readonly BIN='./209poll'
readonly TMP='/tmp/209poll_test'

(make > /dev/null) || exit 1


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

rm $TMP
