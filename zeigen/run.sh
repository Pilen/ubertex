#!/bin/sh

set -e

DIR=$(dirname $(readlink -f $0))

PROCESSING="/usr/share/processing"
if [ ! -d "$PROCESSING" ]; then
    echo "$PROCESSING not found"
    PROCESSING="$DIR/processing-2.2.1"
    echo "Trying $PROCESSING"
fi

CLASSPATH="$PROCESSING/core/library/core.jar:$DIR:$DIR/..:$DIR/default-modules/:./video.jar:$PROCESSING/modes/java/libraries/video/library/gstreamer-java.jar:$PROCESSING/modes/java/libraries/video/library/jna.jar"
# ./video.jar:./gstreamer-java.jar:./jna.jar"

echo "Compiling"
javac -cp $CLASSPATH *.java

echo "Compiling sketches"
javac -cp "$CLASSPATH" default-modules/*.java
if [ -d "$DIR/modules/" ]; then
    javac -cp "$CLASSPATH" modules/*.java
fi

if [[ $# == 1 ]]; then
    echo "Deploying modules"
    # TODO: copy only .class files
    echo "Transferring modules"
    cp -u -r "$DIR/modules/." "$1/modules/"
    sh trans.sh
fi

if [[ $# == 3 ]]; then
    if [ -d "$DIR/modules/." ]; then
        echo "Deploying modules"
        cp -u -r "$DIR/modules/." "$3/modules/"
    fi
    echo "Running"
    $PROCESSING/java/bin/java -cp $CLASSPATH Zeigen "$@"
    # java -cp $CLASSPATH Zeigen "$@"
fi
