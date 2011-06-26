#!/usr/bin/env zsh

make --quiet
./hextest
for sub in `ls tests`; do
    for t in tests/$sub/*.hex; do
        if test "$1" = "-v"; then
            echo testing $sub $t ...
        fi
        ./hex --mode=$sub $t | diff -u - tests/$sub/`basename $t hex`output
    done
done

