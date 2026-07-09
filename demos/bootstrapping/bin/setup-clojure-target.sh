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
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../.." && pwd )"
HYDRA_CLOJURE_DIR="$HYDRA_ROOT/heads/lisp/clojure"
# Shipped runtime (hand-written overlay + generated) lives under dist/, not heads/, since #434
# migrated the Clojure runtime out of the head into overlay/clojure (copied into dist/clojure by sync).
# The head retains only generation drivers + test infra, which we still source from $HYDRA_CLOJURE_DIR.
HYDRA_CLOJURE_DIST="$HYDRA_ROOT/dist/clojure/hydra-kernel"
CLOJURE_RESOURCES="$SCRIPT_DIR/../resources/clojure"

# Clean and create output directory
echo "Preparing output directory: $OUTPUT_DIR"
rm -rf "$OUTPUT_DIR"
mkdir -p "$OUTPUT_DIR"

# Copy static resources
echo "Copying static resources for Clojure target..."

# Build file. The head's deps.edn uses ../../../dist/clojure/hydra-kernel/...
# paths that are correct from heads/lisp/clojure/ but wrong from the demo's
# /tmp/hydra-bootstrapping-demo-*/<host>-to-clojure/. Use the self-contained
# demo deps.edn that finds generated + hand-written content under the demo's
# own src/main and src/test trees.
echo "  Copying build files..."
cp "$CLOJURE_RESOURCES/deps.edn" "$OUTPUT_DIR/"

# Hand-written runtime + generated kernel sources (primitive libraries, DSL).
# Sourced from dist/ (= overlay + generated) since #434 moved the hand-written runtime
# (incl. hydra/lib/libraries.clj) out of the head; copying from heads/ would miss it.
echo "  Copying hand-written source files..."
mkdir -p "$OUTPUT_DIR/src/main/clojure"
cp -r "$HYDRA_CLOJURE_DIST/src/main/clojure/hydra" "$OUTPUT_DIR/src/main/clojure/"

# Test infrastructure. run_tests.clj is the head's own runner entry point (stays in heads/);
# the hydra/ test tree (incl. hand-written helpers testEnv.clj/testGraph.clj migrated to overlay
# by #434, plus generated test modules) comes from dist/ — copying it from heads/ would miss them.
echo "  Copying test infrastructure..."
mkdir -p "$OUTPUT_DIR/src/test/clojure"
cp "$HYDRA_CLOJURE_DIR/src/test/clojure/run_tests.clj" "$OUTPUT_DIR/src/test/clojure/"
if [ -d "$HYDRA_CLOJURE_DIST/src/test/clojure/hydra" ]; then
    cp -r "$HYDRA_CLOJURE_DIST/src/test/clojure/hydra" "$OUTPUT_DIR/src/test/clojure/"
fi

# #546: hydra-build owns hydra.build.* + hydra.test.build.*, relocated out of hydra-kernel.
# The kernel's generated testSuite references hydra.test.build.*, which reference hydra.build.*;
# neither is in the hydra-kernel dist tree. Overlay hydra-build's generated main+test onto the cell.
HYDRA_CLOJURE_BUILD_DIST="$HYDRA_ROOT/dist/clojure/hydra-build"
if [ -d "$HYDRA_CLOJURE_BUILD_DIST/src/main/clojure/hydra" ]; then
    cp -r "$HYDRA_CLOJURE_BUILD_DIST/src/main/clojure/hydra/." "$OUTPUT_DIR/src/main/clojure/hydra/"
fi
if [ -d "$HYDRA_CLOJURE_BUILD_DIST/src/test/clojure/hydra" ]; then
    cp -r "$HYDRA_CLOJURE_BUILD_DIST/src/test/clojure/hydra/." "$OUTPUT_DIR/src/test/clojure/hydra/"
fi

# Summary
STATIC_COUNT=$(find "$OUTPUT_DIR/src/main" -name "*.clj" 2>/dev/null | wc -l | tr -d ' ')
echo "  Static resources: $STATIC_COUNT files"
echo ""
