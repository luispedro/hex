#!/usr/bin/env zsh

make --quiet
./hextest
for sub in `ls tests`; do
    function perform_diff() {
        diff -u tests/$sub/`basename $1 hex`output -
    }
    if test $sub = "dvidecode"; then
        function run() {
            ./hex $1 | ./dvidecode | perform_diff $1
        }
    elif test $sub = "no-crash"; then
        function run() {
            ./hex $1 >/dev/null
        }
    else
        function run() {
            ./hex --mode=$sub $1 | perform_diff $1
        }
    fi
    for t in tests/$sub/*.hex; do
        if test "$1" = "-v"; then
            echo testing $sub $t ...
        fi
        run $t
    done
done

