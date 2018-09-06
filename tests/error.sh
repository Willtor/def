#!/bin/bash

## Test compiler output against the specified error output.

default_color='\033[0m'
red_color='\033[1;31m'
green_color='\033[1;32m'

errout=$1.error
actual=$1.actual

diff=`diff $errout $actual`

if [ "$diff" != "" ]; then
    echo -e "${red_color}Fail: Unexpected negative output.${default_color}"
    echo "$diff"
    exit 1
else
    echo -e "${green_color}Pass.${default_color}"
    echo
    exit 0
fi
