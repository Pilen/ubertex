#!/bin/sh

set -e

CLASSPATH="/usr/share/processing/core/library/core.jar:.:.."

echo "Compiling"
javac -cp $CLASSPATH *.java
echo "Compiling sketches"
javac -cp "$CLASSPATH:./sketches/" sketches/*.java

echo "Running"
/usr/share/processing/java/bin/java -cp $CLASSPATH Zeigen "$@"
