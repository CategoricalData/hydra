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
HYDRA_ELISP_DIR="$HYDRA_ROOT/heads/lisp/emacs-lisp"
# Shipped runtime + test hydra/ tree (hand-written overlay + generated) live under dist/, not heads/,
# since #434 migrated the Emacs Lisp runtime/test helpers into overlay/emacs-lisp (copied into
# dist/emacs-lisp by sync). The head retains generation drivers + the run-tests.el entry point.
HYDRA_ELISP_DIST="$HYDRA_ROOT/dist/emacs-lisp/hydra-kernel"

# Clean and create output directory
echo "Preparing output directory: $OUTPUT_DIR"
rm -rf "$OUTPUT_DIR"
mkdir -p "$OUTPUT_DIR"

# Copy static resources
echo "Copying static resources for Emacs Lisp target..."

# Hand-written source files (primitives, loader, prims).
# Skip bootstrap.el: it is a self-exiting standalone script, so when
# (hydra-load-gen-main) walks the demo's hydra/ tree it would terminate
# the Emacs session. The demo doesn't need it — the bootstrap step is
# done by the Haskell bootstrap-from-json executable.
echo "  Copying hand-written source files..."
mkdir -p "$OUTPUT_DIR/src/main/emacs-lisp"
cp -r "$HYDRA_ELISP_DIST/src/main/emacs-lisp/hydra" "$OUTPUT_DIR/src/main/emacs-lisp/"
rm -f "$OUTPUT_DIR/src/main/emacs-lisp/hydra/bootstrap.el"

# Test infrastructure. EL's test tree is only PARTIALLY migrated by #434: the generated test modules
# + most helpers live in dist/, but a couple of hand-written test files (annotation_bindings.el,
# test_runner.el) remain in the head. Copy the dist tree first, then overlay the head-only files.
echo "  Copying test infrastructure..."
mkdir -p "$OUTPUT_DIR/src/test/emacs-lisp"
if [ -d "$HYDRA_ELISP_DIST/src/test/emacs-lisp/hydra" ]; then
    cp -r "$HYDRA_ELISP_DIST/src/test/emacs-lisp/hydra" "$OUTPUT_DIR/src/test/emacs-lisp/"
fi

# #546: overlay hydra-build's generated main+test (hydra.build.* + hydra.test.build.*),
# referenced by the kernel testSuite but absent from the hydra-kernel dist tree.
HYDRA_ELISP_BUILD_DIST="$HYDRA_ROOT/dist/emacs-lisp/hydra-build"
if [ -d "$HYDRA_ELISP_BUILD_DIST/src/main/emacs-lisp/hydra" ]; then
    cp -r "$HYDRA_ELISP_BUILD_DIST/src/main/emacs-lisp/hydra/." "$OUTPUT_DIR/src/main/emacs-lisp/hydra/"
fi
if [ -d "$HYDRA_ELISP_BUILD_DIST/src/test/emacs-lisp/hydra" ]; then
    cp -r "$HYDRA_ELISP_BUILD_DIST/src/test/emacs-lisp/hydra/." "$OUTPUT_DIR/src/test/emacs-lisp/hydra/"
fi
# Overlay head-only hand-written test files not present in dist.
for f in annotation_bindings.el test_runner.el; do
    if [ -f "$HYDRA_ELISP_DIR/src/test/emacs-lisp/hydra/$f" ]; then
        cp "$HYDRA_ELISP_DIR/src/test/emacs-lisp/hydra/$f" "$OUTPUT_DIR/src/test/emacs-lisp/hydra/$f"
    fi
done

# Top-level run-tests entry point
cp "$HYDRA_ELISP_DIR/run-tests.el" "$OUTPUT_DIR/" 2>/dev/null || true

# Summary
STATIC_COUNT=$(find "$OUTPUT_DIR/src/main" -name "*.el" 2>/dev/null | wc -l | tr -d ' ')
echo "  Static resources: $STATIC_COUNT files"
echo ""
