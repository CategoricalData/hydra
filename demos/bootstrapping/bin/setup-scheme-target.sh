#!/bin/bash
# Clean output directory and copy static resources for a Scheme bootstrap target.
#
# Usage: ./setup-scheme-target.sh <output-dir>

set -e

OUTPUT_DIR="$1"
if [ -z "$OUTPUT_DIR" ]; then
    echo "Usage: $0 <output-dir>"
    exit 1
fi

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../.." && pwd )"
HYDRA_SCHEME_DIR="$HYDRA_ROOT/heads/lisp/scheme"
# Shipped runtime + shims + test tree (hand-written overlay + generated) live under dist/, not heads/,
# since #434 migrated the Scheme sources into overlay/scheme (copied into dist/scheme by sync). The head
# retains only generation drivers + the run-tests entry point, still sourced from $HYDRA_SCHEME_DIR.
HYDRA_SCHEME_DIST="$HYDRA_ROOT/dist/scheme/hydra-kernel"

# Clean and create output directory
echo "Preparing output directory: $OUTPUT_DIR"
rm -rf "$OUTPUT_DIR"
mkdir -p "$OUTPUT_DIR"

# Copy static resources
echo "Copying static resources for Scheme target..."

# Hand-written runtime + generated kernel sources (primitives, loader, prims), incl. the vhash-based
# hydra/scheme/lib/{maps,sets}.scm (relocated by #473), the bundled srfi/ implementations, and the
# (scheme ...) compatibility shims. All sourced from dist/ (= overlay + generated) since #434 moved
# the hand-written Scheme runtime out of the head; copying from heads/ would miss them.
echo "  Copying hand-written source files..."
mkdir -p "$OUTPUT_DIR/src/main/scheme"
cp -r "$HYDRA_SCHEME_DIST/src/main/scheme/hydra" "$OUTPUT_DIR/src/main/scheme/"
if [ -d "$HYDRA_SCHEME_DIST/src/main/scheme/srfi" ]; then
    cp -r "$HYDRA_SCHEME_DIST/src/main/scheme/srfi" "$OUTPUT_DIR/src/main/scheme/"
fi
if [ -d "$HYDRA_SCHEME_DIST/src/main/scheme/scheme" ]; then
    cp -r "$HYDRA_SCHEME_DIST/src/main/scheme/scheme" "$OUTPUT_DIR/src/main/scheme/"
fi
for f in "$HYDRA_SCHEME_DIST/src/main/scheme"/*.scm "$HYDRA_SCHEME_DIST/src/main/scheme"/*.sld; do
    [ -f "$f" ] && cp "$f" "$OUTPUT_DIR/src/main/scheme/"
done

# Test entry point
echo "  Copying test infrastructure..."
mkdir -p "$OUTPUT_DIR/src/test/scheme"
for f in "$HYDRA_SCHEME_DIST/src/test/scheme"/*.scm "$HYDRA_SCHEME_DIST/src/test/scheme"/*.sld; do
    [ -f "$f" ] && cp "$f" "$OUTPUT_DIR/src/test/scheme/"
done
if [ -d "$HYDRA_SCHEME_DIST/src/test/scheme/hydra" ]; then
    cp -r "$HYDRA_SCHEME_DIST/src/test/scheme/hydra" "$OUTPUT_DIR/src/test/scheme/"
fi

# #546: overlay hydra-build's generated main+test (hydra.build.* + hydra.test.build.*),
# referenced by the kernel testSuite but absent from the hydra-kernel dist tree.
HYDRA_SCHEME_BUILD_DIST="$HYDRA_ROOT/dist/scheme/hydra-build"
if [ -d "$HYDRA_SCHEME_BUILD_DIST/src/main/scheme/hydra" ]; then
    cp -r "$HYDRA_SCHEME_BUILD_DIST/src/main/scheme/hydra/." "$OUTPUT_DIR/src/main/scheme/hydra/"
fi
if [ -d "$HYDRA_SCHEME_BUILD_DIST/src/test/scheme/hydra" ]; then
    cp -r "$HYDRA_SCHEME_BUILD_DIST/src/test/scheme/hydra/." "$OUTPUT_DIR/src/test/scheme/hydra/"
fi

# Top-level run-tests entry point
cp "$HYDRA_SCHEME_DIR/run-tests.scm" "$OUTPUT_DIR/" 2>/dev/null || true

# Summary
STATIC_COUNT=$(find "$OUTPUT_DIR/src/main" -name "*.scm" -o -name "*.sld" 2>/dev/null | wc -l | tr -d ' ')
echo "  Static resources: $STATIC_COUNT files"
echo ""
