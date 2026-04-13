#!/bin/bash
# Java bootstrapping demo: loads Hydra modules from JSON and generates code.
# Demonstrates that Java can independently load and process Hydra modules
# from a language-independent JSON representation.
#
# Usage: ./invoke-java-host.sh --target <haskell|java|python> [--include-tests] [--kernel-only] [--types-only] [--output <dir>]
#
# The detailed step-by-step output (timing, file counts, etc.) is provided
# by the Java Bootstrap.main() class itself.

set -eo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../.." && pwd )"
HYDRA_JAVA_DIR="$HYDRA_ROOT/heads/java"

# Warn if running an x86_64 JDK under Rosetta on Apple Silicon (causes ~20x slowdown)
if [ "$(uname -s)" = "Darwin" ] && [ "$(uname -m)" = "arm64" ]; then
    JAVA_CMD="${JAVA_HOME:+$JAVA_HOME/bin/}java"
    if command -v "$JAVA_CMD" > /dev/null 2>&1 && file "$(command -v "$JAVA_CMD")" | grep -q x86_64; then
        echo "WARNING: x86_64 JDK detected on Apple Silicon. This runs under Rosetta 2"
        echo "  and will be ~20x slower than a native arm64 JDK."
        echo "  Current JDK: $("$JAVA_CMD" -version 2>&1 | head -1)"
        echo ""
    fi
fi

# Build hydra-java
echo "Building hydra-java..."
BUILD_START=$(date +%s)
cd "$HYDRA_ROOT"
if ! ./gradlew :hydra-java:compileJava 2>&1; then
    echo ""
    echo "ERROR: Java compilation failed. See errors above."
    exit 1
fi
BUILD_END=$(date +%s)
echo "  Build time: $((BUILD_END - BUILD_START))s"
echo ""

# Build classpath
GRADLE_CACHE="$HOME/.gradle/caches/modules-2/files-2.1"
JAVA_CP="$HYDRA_ROOT/packages/hydra-java/build/classes/java/main"
JAVA_CP="$JAVA_CP:$(find $GRADLE_CACHE -name 'json-io-4.14.1.jar' | head -1)"
JAVA_CP="$JAVA_CP:$(find $GRADLE_CACHE -name 'commons-text-1.10.0.jar' | head -1)"
JAVA_CP="$JAVA_CP:$(find $GRADLE_CACHE -name 'commons-lang3-3.12.0.jar' | head -1)"

# Run the Java bootstrap (its output includes detailed timing and file counts)
# Large stack needed for deeply-nested polymorphic type traversals in eta expansion.
# Increase heap to accommodate the large stack + code generation data structures.
java -Xss256m -Xmx2g -cp "$JAVA_CP" hydra.Bootstrap "$@" --json-dir "$HYDRA_ROOT/dist/json/hydra-kernel/src/main/json"
