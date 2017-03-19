#!/bin/bash

## Test a simple program's output against a correct reference output.  The
## program should succeed in running and not crash.

## Usage: correct.sh <program>
## The files, <program> and <program>.correct, should exist.

prog=./$1
correct=$1.correct
output=$1.out

echo
echo "### Correctness Test: $1 ###"

sleep 0.2
eval $prog > $output
code=$?

if [ "$code" != "0" ]; then
    echo "Fail: program terminated with code $code."
    exit 1
fi

result=`diff $correct $output`

if [ "$result" = "" ]; then
    echo "Pass."
    echo
    exit 0
else
    echo "Fail: Output differed from reference."
    diff $correct $output
    exit 1
fi

