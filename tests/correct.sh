#!/bin/bash

## Test a simple program's output against a correct reference output.  The
## program should succeed in running and not crash.

## Usage: correct.sh <program>
## The files, <program> and <program>.correct, should exist.

default_color='\033[0m'
red_color='\033[1;31m'
green_color='\033[1;32m'

prog=./$1
correct=$1.correct
output=$1.out

echo
echo "### Correctness Test: $1 ###"

sleep 0.1
eval $prog > $output
code=$?

if [ "$code" != "0" ]; then
    echo -e "${red_color}Fail: program terminated with code $code.${default_color}"
    exit 1
fi

result=`diff $correct $output`

if [ "$result" = "" ]; then
    echo -e "${green_color}Pass.${default_color}"
    echo
    exit 0
else
    echo "${red_color}Fail: Output differed from reference.${default_color}"
    diff $correct $output
    exit 1
fi

