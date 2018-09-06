#!/bin/bash

## Test for a set of keywords, along with associated counts, within an LLVM
## IR file.  Match the output against a .keywords file.

## Usage: keywordsearch.sh <basename>
## The files, <basename>.ll and <basename>.keywords, should exist.

default_color='\033[0m'
red_color='\033[1;31m'
green_color='\033[1;32m'

basename=$1
test=$basename.ll
keyfile=$basename.keywords

echo
echo "### Keyword Test: $1 ###"

while IFS='' read -r line || [[ -n "$line" ]]; do
    keyword=`echo $line | cut -d ' ' -f 1`
    expected=`echo $line | cut -d ' ' -f 2`
    regex="\\b$keyword\\b"
    count=`grep -P "$regex" $test | wc -l`
    if [ "$expected" -ne "$count" ]; then
        echo -e "${red_color}Fail: on keyword $keyword.${default_color}"
        exit 1
    fi
done < $keyfile

echo -e "${green_color}Pass.${default_color}"
