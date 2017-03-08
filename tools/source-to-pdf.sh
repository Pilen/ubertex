#!/bin/sh

set -e
base=$(dirname $(readlink -f $(dirname $0)));
workdir=$(mktemp -d)
echo $workdir;
cd $workdir;

for file in $base/emacs/*.el; do
    output=$(basename "$file")
    enscript $file -p "$output.ps" -j -c
    ps2pdf "$output.ps"
done
pdfunite *.el.pdf $base/elisp-source.pdf


for file in $base/worker/*.{h,c}; do
    output=$(basename "$file")
    enscript $file -p "$output.ps" -j -c --highlight=c --color=1
    ps2pdf "$output.ps";
done;
pdfunite *.h.pdf *.c.pdf $base/c-source.pdf

cd $base;
rm -rf $workdir;
