#!/bin/bash

# Generate erldocs for Erlang/OTP's maint branch.

[[ $# -ne 1 ]] && echo "Usage: $0  ‹path to cloned repo›" && exit 1
idir="${1%%/}" # Remove trailing slash if exists
[[ ! -d "$idir"/.git ]] && echo "$idir is not a Git repo!" && exit 1
[[ ! -x "$idir"/otp_build ]] && echo "$idir is not the OTP repo!" && exit 1

erldocs='./erldocs'
[[ ! -x "$erldocs" ]] && [[ ! -L "$erldocs" ]] && \
    echo "$erldocs executable not found!" && exit 1

release="maint"
odir="docs-$release"

site_root='site.git'
site="$site_root/$release"
[[ ! -d "$site" ]] && echo "maint site not found" && exit 1
archive="$site_root/archives/${odir}.tar.bz2"


mkdir -p  "$odir"
rm    -rf "$odir"/*

CONFIGURE_OPTIONS=${CONFIGURE_OPTIONS:-''}
cd "$idir"
echo "Commencing pull & build of $release branch" \
    && rm -rf maint_rel/* \
    && git checkout maint \
    && make clean \
    && git pull origin maint \
    && ./otp_build autoconf -a $CONFIGURE_OPTIONS \
    && ./otp_build configure   $CONFIGURE_OPTIONS \
    && ./configure && make
if [[ $? -ne 0 ]]; then
    echo "Could not make $release"
    cd -
    exit 2
fi
cd -

includes="-I $idir"/erts/include
for dir in "$idir"/lib/*/include "$idir"/bootstrap/lib/*/include; do
    includes="$includes -I $dir"
done

"$erldocs"          \
    -o "$odir"      \
    $includes       \
    "$idir"/lib/*   \
    "$idir"/erts    \
    | tee _"$release"
[[ $? -ne 0 ]] && exit 3

rm  -rf "$odir"/.xml
tar jcf "$odir".tar.bz2 "$odir"

rm -rf "$site"
mv -v  "$odir" "$site"
mv -v  "$odir".tar.bz2 "$archive"
mv -v  _"$release" "$site"/log-"$release".txt

modifs=$(cd "$site_root" && git status --porcelain | wc -l)
git status --porcelain
[[ "$modifs" -eq 2 ]] && echo "No interesting changes to push." && exit 0
cd "$site_root" \
    && git add "$release" \
    && git add  archives/ \
    && git commit -m "Update OTP's $release branch" \
    && git pull origin gh-pages \
    && git push origin gh-pages
cd -
