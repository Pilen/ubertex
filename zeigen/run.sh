#!/bin/sh

set -e

DIR=$(dirname $(readlink -f $0))

if [ -z $DISPLAY ]; then
    echo "No DISPLAY set, assuming DISPLAY=:0"
    export DISPLAY=:0
fi


PROCESSING="/usr/share/processing"

if [ ! -d "$PROCESSING" ]; then
    echo "$PROCESSING not found"
    PROCESSING="$DIR/processing-2.2.1"
    echo "Trying $PROCESSING"
fi

CLASSPATH="$DIR:$DIR/..:$DIR/default-modules/:./video.jar:$PROCESSING/core/library/core.jar:$PROCESSING/modes/java/libraries/video/library/gstreamer-java.jar:$PROCESSING/modes/java/libraries/video/library/jna.jar:$PROCESSING/modes/java/libraries/video/library/video.jar"
# ./video.jar:./gstreamer-java.jar:./jna.jar"

echo "Compiling"
javac -cp $CLASSPATH *.java

echo "Compiling sketches"
javac -cp "$CLASSPATH" default-modules/*.java
if [ -d "$DIR/modules/" ]; then
    javac -cp "$CLASSPATH" modules/*.java
fi

if [ $# == 1 ]; then
    # echo "Deploying modules"
    # TODO: copy only .class files
    cp -u -r "$DIR/modules/." "$1/modules/"
    echo "Compiling modules"
    CLASSPATH="$CLASSPATH:$3/modules/"
    for file in $1/modules/*.java
    do
        javac -cp "$CLASSPATH" "$file"
    done
    # echo "Transferring modules"
    # sh trans.sh


fi

if [ $# == 3 ]; then
    if [ -d "$DIR/modules/." ]; then
        echo "Deploying modules"
        cp -u -r "$DIR/modules/." "$3/modules/"
    fi
    echo "Compiling modules"
    CLASSPATH="$CLASSPATH:$3/modules/"
    for file in $3/modules/*.java
    do
        javac -cp "$CLASSPATH" "$file"
    done
    echo "Running"
    $PROCESSING/java/bin/java -Xms512m -Xmx2048m -cp $CLASSPATH Zeigen "$@"
    # java -cp $CLASSPATH Zeigen "$@"
fi
