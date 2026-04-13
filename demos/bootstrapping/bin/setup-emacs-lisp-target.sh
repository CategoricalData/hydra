#!/bin/bash
# Clean output directory and copy static resources for an Emacs Lisp bootstrap target.
#
# Usage: ./setup-emacs-lisp-target.sh <output-dir>

set -e

OUTPUT_DIR="$1"
if [ -z "$OUTPUT_DIR" ]; then
    echo "Usage: $0 <output-dir>"
    exit 1
fi

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../.." && pwd )"
HYDRA_ELISP_DIR="$HYDRA_ROOT/packages/hydra-lisp/hydra-emacs-lisp"

# Clean and create output directory
echo "Preparing output directory: $OUTPUT_DIR"
rm -rf "$OUTPUT_DIR"
mkdir -p "$OUTPUT_DIR"

# Copy static resources
echo "Copying static resources for Emacs Lisp target..."

# Hand-written source files (primitives, loader, prims)
echo "  Copying hand-written source files..."
mkdir -p "$OUTPUT_DIR/src/main/emacs-lisp"
cp -r "$HYDRA_ELISP_DIR/src/main/emacs-lisp/hydra" "$OUTPUT_DIR/src/main/emacs-lisp/"

# Test infrastructure
echo "  Copying test infrastructure..."
mkdir -p "$OUTPUT_DIR/src/test/emacs-lisp"
if [ -d "$HYDRA_ELISP_DIR/src/test/emacs-lisp/hydra" ]; then
    cp -r "$HYDRA_ELISP_DIR/src/test/emacs-lisp/hydra" "$OUTPUT_DIR/src/test/emacs-lisp/"
fi

# Top-level run-tests entry point
cp "$HYDRA_ELISP_DIR/run-tests.el" "$OUTPUT_DIR/" 2>/dev/null || true

# Summary
STATIC_COUNT=$(find "$OUTPUT_DIR/src/main" -name "*.el" 2>/dev/null | wc -l | tr -d ' ')
echo "  Static resources: $STATIC_COUNT files"
echo ""
