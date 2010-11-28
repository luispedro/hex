#!/usr/bin/env zsh

make -q
./hextest
for sub in `ls tests`; do 
    for t in tests/$sub/*.hex; do
        ./hex $sub $t | diff - tests/$sub/`basename $t hex`output 
    done
done

