#!/bin/bash
# Clean output directory and copy static resources for a Common Lisp bootstrap target.
#
# Usage: ./setup-common-lisp-target.sh <output-dir>

set -e

OUTPUT_DIR="$1"
if [ -z "$OUTPUT_DIR" ]; then
    echo "Usage: $0 <output-dir>"
    exit 1
fi

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../../../.." && pwd )"
HYDRA_CL_DIR="$HYDRA_ROOT/packages/hydra-lisp/hydra-common-lisp"

# Clean and create output directory
echo "Preparing output directory: $OUTPUT_DIR"
rm -rf "$OUTPUT_DIR"
mkdir -p "$OUTPUT_DIR"

# Copy static resources
echo "Copying static resources for Common Lisp target..."

# Hand-written source files (primitives, loader, prelude)
echo "  Copying hand-written source files..."
mkdir -p "$OUTPUT_DIR/src/main/common-lisp"
cp -r "$HYDRA_CL_DIR/src/main/common-lisp/hydra" "$OUTPUT_DIR/src/main/common-lisp/"

# Test infrastructure
echo "  Copying test infrastructure..."
mkdir -p "$OUTPUT_DIR/src/test/common-lisp"
for f in "$HYDRA_CL_DIR/src/test/common-lisp"/*.lisp; do
    [ -f "$f" ] && cp "$f" "$OUTPUT_DIR/src/test/common-lisp/"
done
if [ -d "$HYDRA_CL_DIR/src/test/common-lisp/hydra" ]; then
    cp -r "$HYDRA_CL_DIR/src/test/common-lisp/hydra" "$OUTPUT_DIR/src/test/common-lisp/"
fi

# Summary
STATIC_COUNT=$(find "$OUTPUT_DIR/src/main" -name "*.lisp" 2>/dev/null | wc -l | tr -d ' ')
echo "  Static resources: $STATIC_COUNT files"
echo ""
