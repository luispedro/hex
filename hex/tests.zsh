#!/usr/bin/env zsh

make --quiet
./hextest
for sub in `ls tests`; do
    if test $sub = "dvidecode"; then
        function run() {
            ./hex $1 | ./dvidecode
        }
    else
        function run() {
            ./hex --mode=$sub $1
        }
    fi
    for t in tests/$sub/*.hex; do
        if test "$1" = "-v"; then
            echo testing $sub $t ...
        fi
        run $t | diff -u - tests/$sub/`basename $t hex`output
    done
done

