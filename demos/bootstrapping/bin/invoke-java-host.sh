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

source "$HYDRA_ROOT/bin/lib/common.sh"
check_native_jdk

# Ensure every coder/ext package that packages/hydra-java's source set
# depends on has a Java distribution. The hydra-java gradle build pulls
# sources from dist/java/hydra-{kernel,haskell,java,python,scala,lisp,
# pg,rdf,ext}; missing dirs cause "package hydra.X does not exist"
# compile errors (e.g. EdgeBuilder.java importing hydra.pg.model).
# sync-default() doesn't generate non-language packages, so on a fresh
# checkout pg/rdf/lisp/scala may be missing. Assemble them on demand —
# warm-cache short-circuits in seconds. Redirect output to a log file so
# the assembler's per-package "Done: N main..." lines don't confuse the
# bootstrap-all log parser.
ASSEMBLE_LOG="${ASSEMBLE_LOG:-/tmp/hydra-java-host-coder-assembly.log}"
: > "$ASSEMBLE_LOG"
for coder_pkg in hydra-haskell hydra-java hydra-python hydra-scala hydra-lisp hydra-pg hydra-rdf; do
    coder_base="$HYDRA_ROOT/dist/java/$coder_pkg/src/main/java"
    if [ ! -d "$coder_base/hydra" ]; then
        echo "Java host needs $coder_pkg; generating (see $ASSEMBLE_LOG)..."
        (cd "$HYDRA_ROOT" && heads/java/bin/assemble-distribution.sh "$coder_pkg") >> "$ASSEMBLE_LOG" 2>&1
    fi
done

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
java -Xss256m -Xmx2g -cp "$JAVA_CP" hydra.Bootstrap "$@" --json-dir "$HYDRA_ROOT/dist/json"
