#!/bin/sh

set -e
DIR=`dirname $0`
PROCESSING="$DIR/processing-2.2.1"
CLASSPATH="$PROCESSING/core/library/core.jar:$DIR:$DIR/..:./video.jar:$PROCESSING/modes/java/libraries/video/library/gstreamer-java.jar:$PROCESSING/modes/java/libraries/video/library/jna.jar"
# ./video.jar:./gstreamer-java.jar:./jna.jar"

echo $CLASSPATH
echo "Compiling"
javac -cp $CLASSPATH *.java
echo "Compiling sketches"
javac -cp "$CLASSPATH:./sketches/" sketches/*.java

echo "Running"
$PROCESSING/java/bin/java -cp $CLASSPATH Zeigen "$@"
# java -cp $CLASSPATH Zeigen "$@"
