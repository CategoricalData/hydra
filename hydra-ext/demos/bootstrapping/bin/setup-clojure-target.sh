#!/bin/bash
# Clean output directory and copy static resources for a Clojure bootstrap target.
# This is host-language-independent: the same static resources are needed
# regardless of which host generated the code.
#
# Usage: ./setup-clojure-target.sh <output-dir>

set -e

OUTPUT_DIR="$1"
if [ -z "$OUTPUT_DIR" ]; then
    echo "Usage: $0 <output-dir>"
    exit 1
fi

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../../.." && pwd )"
HYDRA_CLOJURE_DIR="$HYDRA_ROOT/hydra-lisp/hydra-clojure"

# Clean and create output directory
echo "Preparing output directory: $OUTPUT_DIR"
rm -rf "$OUTPUT_DIR"
mkdir -p "$OUTPUT_DIR"

# Copy static resources
echo "Copying static resources for Clojure target..."

# Build file
echo "  Copying build files..."
cp "$HYDRA_CLOJURE_DIR/deps.edn" "$OUTPUT_DIR/"

# Hand-written source files (primitive libraries, DSL, user.clj)
echo "  Copying hand-written source files..."
mkdir -p "$OUTPUT_DIR/src/main/clojure"
cp -r "$HYDRA_CLOJURE_DIR/src/main/clojure/hydra" "$OUTPUT_DIR/src/main/clojure/"
cp "$HYDRA_CLOJURE_DIR/src/main/clojure/user.clj" "$OUTPUT_DIR/src/main/clojure/"

# Test infrastructure
echo "  Copying test infrastructure..."
mkdir -p "$OUTPUT_DIR/src/test/clojure"
cp "$HYDRA_CLOJURE_DIR/src/test/clojure/run_tests.clj" "$OUTPUT_DIR/src/test/clojure/"
if [ -f "$HYDRA_CLOJURE_DIR/src/test/clojure/run_generation_tests.clj" ]; then
    cp "$HYDRA_CLOJURE_DIR/src/test/clojure/run_generation_tests.clj" "$OUTPUT_DIR/src/test/clojure/"
fi
if [ -d "$HYDRA_CLOJURE_DIR/src/test/clojure/hydra" ]; then
    cp -r "$HYDRA_CLOJURE_DIR/src/test/clojure/hydra" "$OUTPUT_DIR/src/test/clojure/"
fi

# Summary
STATIC_COUNT=$(find "$OUTPUT_DIR/src/main" -name "*.clj" 2>/dev/null | wc -l | tr -d ' ')
echo "  Static resources: $STATIC_COUNT files"
echo ""
