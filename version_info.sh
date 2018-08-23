#!/bin/bash

# Script for keeping/recording version number and keeping it consistent
# across the compiler and libraries.

VERSION_MAJOR=0
VERSION_MINOR=17
VERSION_PATCH=0
VERSION_SUFFIX=a
VERSION_BUILD=`git rev-parse HEAD | cut -c -12`
BUILD_DATE=`date -I`
PROJECT_SUPPORT="willtor@mit.edu"

if [ "$1" = "ocaml" ]; then
    echo "let version_maj = $VERSION_MAJOR"
    echo "let version_min = $VERSION_MINOR"
    echo "let version_patch = $VERSION_PATCH"
    echo "let version_suffix = \"$VERSION_SUFFIX\""
    echo "let version_build = \"$VERSION_BUILD\""
    echo "let build_date = \"$BUILD_DATE\""
    echo "let project_support = \"$PROJECT_SUPPORT\""
elif [ "$1" = "major" ]; then
    echo "def-$VERSION_MAJOR$VERSION_SUFFIX"
elif [ "$1" = "minor" ]; then
    echo "def-$VERSION_MAJOR.$VERSION_MINOR$VERSION_SUFFIX"
elif [ "$1" = "patch" ]; then
    echo "def-$VERSION_MAJOR.$VERSION_MINOR.$VERSION_PATCH$VERSION_SUFFIX"
fi
