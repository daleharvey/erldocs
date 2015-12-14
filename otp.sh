#!/bin/bash

# Generate erldocs docs from Erlang/OTP's tarball

[[ $# -eq 0 ]] && echo "Usage: $0  ‹path to untar'ed OTP›⁺" && exit 1

idir="${1%%/}" # Remove trailing slash if exists
release="$(basename "$idir" | sed 's/otp_src_//')"
odir="docs-$release"
erldocs='./erldocs'

mkdir -p  "$odir"
rm    -rf "$odir"/*
[[ ! -x "$erldocs" ]] && [[ ! -L $"$erldocs" ]] && \
    echo "$erldocs executable not found!" && exit 1

## ./configure && make && make docs
# If you didn't: test -f lib/xmerl/doc/src/xmerl.xml is false

"$erldocs"          \
    -o "$odir"      \
    "$idir"/lib/*   \
    "$idir"/erts*   \
    | tee _"$release"

rm -rf "$odir"/.xml
tar jcf "$odir".tar.bz2 "$odir"

shift && [[ "$1" = '' ]] && exit 0 || $0 $*
