#!/bin/sh

set -e
DIR=`dirname $0`

PROCESSING="/usr/share/processing"
if [ ! -d "$PROCESSING" ]; then
    echo "not found"
    PROCESSING="$DIR/processing-2.2.1"
fi

CLASSPATH="$PROCESSING/core/library/core.jar:$DIR:$DIR/..:./video.jar:$PROCESSING/modes/java/libraries/video/library/gstreamer-java.jar:$PROCESSING/modes/java/libraries/video/library/jna.jar"
# ./video.jar:./gstreamer-java.jar:./jna.jar"

echo "Compiling"
javac -cp $CLASSPATH *.java
echo "Compiling sketches"
javac -cp "$CLASSPATH:./modules/" modules/*.java

echo "Running"
$PROCESSING/java/bin/java -cp $CLASSPATH Zeigen "$@"
# java -cp $CLASSPATH Zeigen "$@"
