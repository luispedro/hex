#!/usr/bin/env zsh

make --quiet
./hextest
for sub in `ls tests`; do
    for t in tests/$sub/*.hex; do
        ./hex $sub $t | diff -u - tests/$sub/`basename $t hex`output
    done
done

