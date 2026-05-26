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

# Coder-package assembly used to be done here on demand. That logic was
# hoisted into bootstrap-all.sh's pre-sync step in #309 (audit flaw F8);
# the bin/sync.sh call there derives the matrix from --hosts/--targets and
# uses per-package digest caching. Calling this script standalone (outside
# bootstrap-all.sh) requires bin/sync.sh to have been run first.

# Build hydra-java. We need both the main source set (kernel + every per-package
# coder distribution) and the headsExtras source set, which carries the developer
# drivers (hydra.Bootstrap, hydra.Generation, HydraTestBase) and demos that the
# kernel artifact intentionally doesn't bundle.
echo "Building hydra-java..."
BUILD_START=$(date +%s)
cd "$HYDRA_JAVA_DIR"
if ! ./gradlew :hydra-java:compileJava :hydra-java:compileHeadsExtrasJava 2>&1; then
    echo ""
    echo "ERROR: Java compilation failed. See errors above."
    exit 1
fi
BUILD_END=$(date +%s)
echo "  Build time: $((BUILD_END - BUILD_START))s"
echo ""

# Build classpath. main = kernel + coders; headsExtras = Bootstrap, Generation,
# demos. The bootstrap demo's entry point (hydra.Bootstrap) lives in headsExtras.
# Use Gradle as the source of truth for the resolved classpath (compiled
# outputs + every transitive jar) — see :hydra-java:printHeadsExtrasRuntimeClasspath.
JAVA_CP=$(./gradlew --quiet :hydra-java:printHeadsExtrasRuntimeClasspath)

# Run the Java bootstrap (its output includes detailed timing and file counts)
# Large stack needed for deeply-nested polymorphic type traversals in eta expansion.
# Increase heap to accommodate the large stack + code generation data structures.
java -Xss256m -Xmx2g -cp "$JAVA_CP" hydra.Bootstrap "$@" --json-dir "$HYDRA_ROOT/dist/json"
