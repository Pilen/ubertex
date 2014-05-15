#!/bin/sh

set -e

CLASSPATH="/usr/share/processing/core/library/core.jar:.:..:./sketches"

echo "Compiling"
javac -cp $CLASSPATH *.java
echo "Compiling sketches"
javac -cp $CLASSPATH sketches/*.java

echo "Running"
java -cp $CLASSPATH Zeigen "$@"
