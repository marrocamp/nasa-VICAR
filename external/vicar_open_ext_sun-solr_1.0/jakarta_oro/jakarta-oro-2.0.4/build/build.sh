#!/bin/sh

#
# $Id: build.sh,v 1.1 2001/05/16 08:53:34 dfs Exp $
#

# --------------------------------------------
# Default == jar
# "lib"            target builds the library
# "examples"       target builds the example programs
# "tools"          target builds the tools
# "clean"          target removes generated files
# "jar"            target builds lib + jar
# "javadocs"       target builds the javadoc
# "package"        target builds lib + jar + javadoc + distribution
# --------------------------------------------


PROGRAM=$0
PROGRAM_NAME="`basename $PROGRAM`"
PROGRAM_DIR="`dirname $PROGRAM`"
JAR_DIR=.
BUILDFILE=./build.xml
JAKARTA_SITE2=../../jakarta-site2
JAKARTA_SITE2_LIB="${JAKARTA_SITE2}/lib"

cd "$PROGRAM_DIR"

#--------------------------------------------
# No need to edit anything past here
#--------------------------------------------
if test -z "${JAVA_HOME}" ; then
    echo "ERROR: JAVA_HOME not found in your environment."
    echo "Please, set the JAVA_HOME variable in your environment to match the"
    echo "location of the Java Virtual Machine you want to use."
    exit
fi

if test -f "${JAVA_HOME}/lib/tools.jar" ; then
    CLASSPATH="${CLASSPATH}:${JAVA_HOME}/lib/tools.jar"
fi

if test -d "$JAKARTA_SITE2" ; then
    for jar in "$JAKARTA_SITE2_LIB"/*.jar; do
	CLASSPATH="${CLASSPATH}:$jar"
    done
fi

for jar in "$JAR_DIR"/*.jar; do
    CLASSPATH="${CLASSPATH}:$jar"
done

echo "Now building ${TARGET}..."

echo "CLASSPATH: ${CLASSPATH}"
echo "JAVA_HOME: ${JAVA_HOME}"


"${JAVA_HOME}/bin/java" -classpath "$CLASSPATH" \
    -Djakarta-site2.dir="$JAKARTA_SITE2" \
   org.apache.tools.ant.Main -buildfile "$BUILDFILE" "$@"
