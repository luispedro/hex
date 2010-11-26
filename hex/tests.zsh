#!/usr/bin/env zsh

make -q
./hextest
./hex tests/chars/one.hex | diff - tests/chars/one.output
