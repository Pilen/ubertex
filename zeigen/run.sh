#!/bin/sh

set -e

CLASSPATH="/usr/share/processing/core/library/core.jar:.:..:./video.jar:/usr/share/processing/modes/java/libraries/video/library/gstreamer-java.jar:/usr/share/processing/modes/java/libraries/video/library/jna.jar"
# ./video.jar:./gstreamer-java.jar:./jna.jar"

echo "Compiling"
javac -cp $CLASSPATH *.java
echo "Compiling sketches"
javac -cp "$CLASSPATH:./sketches/" sketches/*.java

echo "Running"
/usr/share/processing/java/bin/java -cp $CLASSPATH Zeigen "$@"
# java -cp $CLASSPATH Zeigen "$@"
