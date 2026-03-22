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
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../../.." && pwd )"
HYDRA_SCALA_DIR="$HYDRA_ROOT/hydra-scala"

# Clean and create output directory
echo "Preparing output directory: $OUTPUT_DIR"
rm -rf "$OUTPUT_DIR"
mkdir -p "$OUTPUT_DIR"

# Copy static resources
echo "Copying static resources for Scala target..."

# Build files
echo "  Copying build files..."
cp "$HYDRA_SCALA_DIR/build.sbt" "$OUTPUT_DIR/"
mkdir -p "$OUTPUT_DIR/project"
cp "$HYDRA_SCALA_DIR/project/build.properties" "$OUTPUT_DIR/project/" 2>/dev/null || true

# Hand-written source files (primitive libraries)
echo "  Copying hand-written source files..."
if [ -d "$HYDRA_SCALA_DIR/src/main/scala" ]; then
    mkdir -p "$OUTPUT_DIR/src/main/scala"
    cp -r "$HYDRA_SCALA_DIR/src/main/scala/hydra" "$OUTPUT_DIR/src/main/scala/"
fi

# Test runner
echo "  Copying test runner..."
if [ -d "$HYDRA_SCALA_DIR/src/test/scala" ]; then
    mkdir -p "$OUTPUT_DIR/src/test/scala"
    cp -r "$HYDRA_SCALA_DIR/src/test/scala/hydra" "$OUTPUT_DIR/src/test/scala/"
fi

# Summary
STATIC_COUNT=$(find "$OUTPUT_DIR/src/main" -name "*.scala" 2>/dev/null | wc -l | tr -d ' ')
echo "  Static resources: $STATIC_COUNT files"
echo ""
