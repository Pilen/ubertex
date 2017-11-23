
set -eu
file="den-store-av-bog.tex"
hash=$(git rev-parse --short HEAD)
found=$(git ls-files -m "$file")

if [ "$found" == "$file" ] ; then
    echo "$hash MODIFIED"
else
    echo "$hash"
fi
