#!/bin/bash
# Java bootstrapping demo: loads Hydra modules from JSON and generates code.
# Demonstrates that Java can independently load and process Hydra modules
# from a language-independent JSON representation.
#
# Usage: ./java-bootstrap.sh --target <haskell|java|python>

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../.." && pwd )"
HYDRA_JAVA_DIR="$HYDRA_ROOT/hydra-java"

echo "=========================================="
echo "Java Bootstrapping Demo"
echo "=========================================="
echo ""

# Build hydra-java
echo "Building hydra-java..."
cd "$HYDRA_ROOT"
./gradlew :hydra-java:compileJava 2>&1 | tail -3
echo ""

# Build classpath
GRADLE_CACHE="$HOME/.gradle/caches/modules-2/files-2.1"
JAVA_CP="$HYDRA_JAVA_DIR/build/classes/java/main"
JAVA_CP="$JAVA_CP:$(find $GRADLE_CACHE -name 'json-io-4.14.1.jar' | head -1)"
JAVA_CP="$JAVA_CP:$(find $GRADLE_CACHE -name 'commons-text-1.10.0.jar' | head -1)"
JAVA_CP="$JAVA_CP:$(find $GRADLE_CACHE -name 'commons-lang3-3.12.0.jar' | head -1)"

# Run the Java bootstrap
java -cp "$JAVA_CP" hydra.Bootstrap "$@" --json-dir "$HYDRA_ROOT/hydra-haskell/src/gen-main/json"
