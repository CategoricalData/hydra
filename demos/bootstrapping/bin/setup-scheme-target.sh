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

# Clean and create output directory
echo "Preparing output directory: $OUTPUT_DIR"
rm -rf "$OUTPUT_DIR"
mkdir -p "$OUTPUT_DIR"

# Copy static resources
echo "Copying static resources for Scheme target..."

# Hand-written source files (primitives, loader, prims)
echo "  Copying hand-written source files..."
mkdir -p "$OUTPUT_DIR/src/main/scheme"
cp -r "$HYDRA_SCHEME_DIR/src/main/scheme/hydra" "$OUTPUT_DIR/src/main/scheme/"
# The src/main maps.scm and sets.scm use Guile-specific vhash which requires
# (ice-9 vlist). For standalone targets, replace with the portable versions.
echo "  Replacing vhash-based maps/sets with portable alist versions..."
cp "$HYDRA_SCHEME_DIR/src/main/scheme/hydra/lib/maps.scm" "$OUTPUT_DIR/src/main/scheme/hydra/lib/maps.scm"
cp "$HYDRA_SCHEME_DIR/src/main/scheme/hydra/lib/sets.scm" "$OUTPUT_DIR/src/main/scheme/hydra/lib/sets.scm"
# Copy bundled SRFI implementations (e.g., SRFI-151 for bitwise ops)
if [ -d "$HYDRA_SCHEME_DIR/src/main/scheme/srfi" ]; then
    cp -r "$HYDRA_SCHEME_DIR/src/main/scheme/srfi" "$OUTPUT_DIR/src/main/scheme/"
fi
# Copy compatibility shims (e.g., (scheme bytevector) for Guile)
if [ -d "$HYDRA_SCHEME_DIR/src/main/scheme/scheme" ]; then
    cp -r "$HYDRA_SCHEME_DIR/src/main/scheme/scheme" "$OUTPUT_DIR/src/main/scheme/"
fi
for f in "$HYDRA_SCHEME_DIR/src/main/scheme"/*.scm "$HYDRA_SCHEME_DIR/src/main/scheme"/*.sld; do
    [ -f "$f" ] && cp "$f" "$OUTPUT_DIR/src/main/scheme/"
done

# Test entry point
echo "  Copying test infrastructure..."
mkdir -p "$OUTPUT_DIR/src/test/scheme"
for f in "$HYDRA_SCHEME_DIR/src/test/scheme"/*.scm "$HYDRA_SCHEME_DIR/src/test/scheme"/*.sld; do
    [ -f "$f" ] && cp "$f" "$OUTPUT_DIR/src/test/scheme/"
done
if [ -d "$HYDRA_SCHEME_DIR/src/test/scheme/hydra" ]; then
    cp -r "$HYDRA_SCHEME_DIR/src/test/scheme/hydra" "$OUTPUT_DIR/src/test/scheme/"
fi

# Top-level run-tests entry point
cp "$HYDRA_SCHEME_DIR/run-tests.scm" "$OUTPUT_DIR/" 2>/dev/null || true

# Summary
STATIC_COUNT=$(find "$OUTPUT_DIR/src/main" -name "*.scm" -o -name "*.sld" 2>/dev/null | wc -l | tr -d ' ')
echo "  Static resources: $STATIC_COUNT files"
echo ""
