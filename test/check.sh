#!/bin/bash

# Generate docs for erldocs, display the diff

set -e
mkdir -p doc
rm -rf doc/* >/dev/null
make -j escript
./erldocs -o doc/ $(pwd)
rm -r doc/.xml/
errors=$(git status --porcelain -- doc/ | wc -l)
git status --porcelain -- doc/
exit $errors
