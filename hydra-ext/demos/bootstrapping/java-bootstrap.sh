#!/bin/bash
# Java bootstrapping demo: loads Hydra modules from JSON and generates code.
# Demonstrates that Java can independently load and process Hydra modules
# from a language-independent JSON representation.
#
# Usage: ./java-bootstrap.sh --target <haskell|java|python> [--types-only] [--kernel-only] [--output <dir>]
#
# The detailed step-by-step output (timing, file counts, etc.) is provided
# by the Java Bootstrap.main() class itself.

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../.." && pwd )"
HYDRA_JAVA_DIR="$HYDRA_ROOT/hydra-java"

echo "=========================================="
echo "Java Bootstrapping Demo"
echo "=========================================="
echo ""

# Step 0: Build hydra-java
echo "Building hydra-java..."
BUILD_START=$(date +%s%N 2>/dev/null || python3 -c 'import time; print(int(time.time()*1e9))')
cd "$HYDRA_ROOT"
./gradlew :hydra-java:compileJava 2>&1 | tail -3
BUILD_END=$(date +%s%N 2>/dev/null || python3 -c 'import time; print(int(time.time()*1e9))')
BUILD_MS=$(( (BUILD_END - BUILD_START) / 1000000 ))
echo "  Build time: ${BUILD_MS}ms"
echo ""

# Build classpath
GRADLE_CACHE="$HOME/.gradle/caches/modules-2/files-2.1"
JAVA_CP="$HYDRA_JAVA_DIR/build/classes/java/main"
JAVA_CP="$JAVA_CP:$(find $GRADLE_CACHE -name 'json-io-4.14.1.jar' | head -1)"
JAVA_CP="$JAVA_CP:$(find $GRADLE_CACHE -name 'commons-text-1.10.0.jar' | head -1)"
JAVA_CP="$JAVA_CP:$(find $GRADLE_CACHE -name 'commons-lang3-3.12.0.jar' | head -1)"

# Run the Java bootstrap (its output includes detailed timing and file counts)
java -Xss16m -cp "$JAVA_CP" hydra.Bootstrap "$@" --json-dir "$HYDRA_ROOT/hydra-haskell/src/gen-main/json"
