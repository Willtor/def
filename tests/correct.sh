#!/bin/bash

prog=./$1
correct=$1.correct
output=$1.out

echo
echo "### Correctness Test: $1 ###"

sleep 0.2
eval $prog > $output

result=`diff $correct $output`

if [ "$result" = "" ]; then
    echo "Pass."
    exit 0
else
    echo "Fail:"
    diff $correct $output
    exit 1
fi

