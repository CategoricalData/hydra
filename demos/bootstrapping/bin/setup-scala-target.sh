#!/bin/bash
# Clean output directory and copy static resources for a Scala bootstrap target.
#
# Usage: ./setup-scala-target.sh <output-dir>

set -e

OUTPUT_DIR="$1"
if [ -z "$OUTPUT_DIR" ]; then
    echo "Usage: $0 <output-dir>"
    exit 1
fi

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../.." && pwd )"
# Scala layout: sbt project lives under packages/hydra-scala/; the hand-written
# runtime sources (lib/, tests) live under overlay/scala/hydra-kernel/ post-#434
# (the generation drivers Bootstrap/Generation remain under heads/scala/). The
# bootstrap demo uses its own self-contained build.sbt (under resources/scala/)
# that does not reference the parent repo's dist/, so it copies the runtime
# straight out of the overlay tree into its flat output. This copy is a
# legitimate overlay reader (like the per-target copy-kernel-runtime.sh scripts).
HYDRA_SCALA_PKG="$HYDRA_ROOT/packages/hydra-scala"
HYDRA_SCALA_OVERLAY="$HYDRA_ROOT/overlay/scala/hydra-kernel"
SCALA_RESOURCES="$SCRIPT_DIR/../resources/scala"

# Clean and create output directory
echo "Preparing output directory: $OUTPUT_DIR"
rm -rf "$OUTPUT_DIR"
mkdir -p "$OUTPUT_DIR"

# Copy static resources
echo "Copying static resources for Scala target..."

# Build files
echo "  Copying build files..."
cp "$SCALA_RESOURCES/build.sbt" "$OUTPUT_DIR/"
mkdir -p "$OUTPUT_DIR/project"
cp "$HYDRA_SCALA_PKG/project/build.properties" "$OUTPUT_DIR/project/" 2>/dev/null || true

# Hand-written source files (primitive libraries) from the overlay tree.
# Note: Generation.scala and Bootstrap.scala (generation drivers) are NOT here —
# they stay under heads/scala/ and reference hydra.* coder modules not present in
# the bootstrapping target, so the demo deliberately omits them.
echo "  Copying hand-written source files..."
if [ -d "$HYDRA_SCALA_OVERLAY/src/main/scala" ]; then
    mkdir -p "$OUTPUT_DIR/src/main/scala/hydra"
    # Copy the hydra.overlay.scala.* runtime (native primitive IMPLEMENTATIONS + Libraries.scala
    # registry). #501 moved these under hydra/overlay/scala/ (was hydra/scala/lib pre-#501, and
    # hydra/lib pre-#473). The generated consumers + registry call hydra.overlay.scala.lib.*;
    # without them the cell fails to compile with "value overlay is not a member of hydra".
    # Blanket-copy the whole overlay subtree (as the Java/Python cells do) so it stays correct
    # across future namespace moves rather than cherry-picking paths that a rename can orphan.
    # (The hydra.lib.* PrimitiveDefinition def-modules are NOT here — they are generated into
    # dist/scala/hydra-kernel/hydra/lib/ and arrive via the dist copy above.)
    if [ -d "$HYDRA_SCALA_OVERLAY/src/main/scala/hydra/overlay" ]; then
        cp -r "$HYDRA_SCALA_OVERLAY/src/main/scala/hydra/overlay" "$OUTPUT_DIR/src/main/scala/hydra/"
    fi
fi

# Test runner (TestSuiteRunner.scala + test/testEnv.scala) from the overlay tree.
echo "  Copying test runner..."
if [ -d "$HYDRA_SCALA_OVERLAY/src/test/scala" ]; then
    mkdir -p "$OUTPUT_DIR/src/test/scala"
    cp -r "$HYDRA_SCALA_OVERLAY/src/test/scala/hydra" "$OUTPUT_DIR/src/test/scala/"
fi

# #546: overlay hydra-build's generated main+test (hydra.build.* + hydra.test.build.*),
# referenced by the kernel testSuite but absent from the hydra-kernel dist tree.
HYDRA_SCALA_BUILD_DIST="$HYDRA_ROOT/dist/scala/hydra-build"
if [ -d "$HYDRA_SCALA_BUILD_DIST/src/main/scala/hydra" ]; then
    mkdir -p "$OUTPUT_DIR/src/main/scala/hydra"
    cp -r "$HYDRA_SCALA_BUILD_DIST/src/main/scala/hydra/." "$OUTPUT_DIR/src/main/scala/hydra/"
fi
if [ -d "$HYDRA_SCALA_BUILD_DIST/src/test/scala/hydra" ]; then
    mkdir -p "$OUTPUT_DIR/src/test/scala/hydra"
    cp -r "$HYDRA_SCALA_BUILD_DIST/src/test/scala/hydra/." "$OUTPUT_DIR/src/test/scala/hydra/"
fi

# Summary
STATIC_COUNT=$(find "$OUTPUT_DIR/src/main" -name "*.scala" 2>/dev/null | wc -l | tr -d ' ')
echo "  Static resources: $STATIC_COUNT files"
echo ""
