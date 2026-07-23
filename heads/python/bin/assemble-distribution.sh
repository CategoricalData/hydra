#!/usr/bin/env bash
# Layer 2 assembler: produce a complete Python distribution for one package.
#
# Usage:
#   assemble-distribution.sh <pkg> [--dist-root <dir>]
#
# Produces <dist-root>/<pkg>/ (default: ../../dist/python/<pkg>/) by:
#   1. Calling Layer 1 transform-json-to-python.sh for main modules
#   2. Calling Layer 1 transform-json-to-python.sh for test modules
#   3. Applying package-specific post-processing:
#      - hydra-kernel: copy test_env.py (the runtime counterpart of
#        hydra.test.testEnv, filtered from emitted output) and the
#        hand-written runtime support (lib/, dsl/, sources/, tools.py,
#        py.typed) so the published wheel is self-contained.
#   4. Generating a per-package pyproject.toml so each dist/python/<pkg>/
#      is a standalone publishable wheel build.
#
# Assemblers do NOT run tests. Test invocation is Layer 2.5's
# test-distribution.sh.

set -euo pipefail

if [ $# -lt 1 ]; then
    echo "Usage: $0 <package> [--dist-root <dir>]" >&2
    exit 1
fi

PACKAGE="$1"
shift

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_PYTHON_HEAD="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_PYTHON_HEAD/../.." && pwd )"

DIST_ROOT="$HYDRA_ROOT_DIR/dist/python"

while [ $# -gt 0 ]; do
    case "$1" in
        --dist-root) DIST_ROOT="$2"; shift 2 ;;
        *) shift ;;
    esac
done

OUT_DIR="$DIST_ROOT/$PACKAGE"
OUT_MAIN="$OUT_DIR/src/main/python"
OUT_TEST="$OUT_DIR/src/test/python"
DIST_JSON_ROOT="$HYDRA_ROOT_DIR/dist/json"
INPUT_DIGEST_MAIN="$DIST_JSON_ROOT/$PACKAGE/build/main/digest.json"
INPUT_DIGEST_TEST="$DIST_JSON_ROOT/$PACKAGE/build/test/digest.json"
OUTPUT_DIGEST_MAIN="$OUT_DIR/build/main/digest.json"
OUTPUT_DIGEST_TEST="$OUT_DIR/build/test/digest.json"
TEST_JSON_DIR="$DIST_JSON_ROOT/$PACKAGE/src/test/json"

echo "=== Assembling Python distribution: $PACKAGE ==="
echo "  Output: $OUT_DIR"
echo ""

# Per-source-set freshness check via digest-check. See
# heads/java/bin/assemble-distribution.sh for the pattern; same shape
# across every target language.
source "$HYDRA_ROOT_DIR/bin/lib/common.sh"
source "$HYDRA_ROOT_DIR/bin/lib/assemble-common.sh"

# Per-target generator stamp; see assemble-common.sh and #347.
export_generation_env python

# Step 0: copy any hand-written Python overlay source for this package BEFORE
# generation, recording the copied files in a keep-paths manifest so #357 prune
# in bootstrap-from-json --prune-stale below won't remove them. Was Step 3a/3b in
# the pre-#357 layout.
#
# Generalized from the kernel-only runtime copy (#418) to any package (#511):
# copy-overlay.sh is a no-op for packages with no overlay/python/<pkg>/ tree, and
# copies the overlay onto the dist for those that have one (hydra-kernel runtime;
# hydra-pg / hydra-rdf binding source folded into overlay; etc.).
KEEP_MANIFEST="$(mktemp -t hydra-keep-paths-python.XXXXXX)"
trap 'rm -f "$KEEP_MANIFEST"' EXIT
echo "Step 0: Copying hand-written Python overlay source into $PACKAGE dist (if any)..."
"$SCRIPT_DIR/copy-overlay.sh" "$PACKAGE" --dist-root "$DIST_ROOT" --manifest "$KEEP_MANIFEST"
echo ""

# Step 1: Main modules.
if assemble_check_fresh "$INPUT_DIGEST_MAIN" "$OUT_MAIN" "$OUTPUT_DIGEST_MAIN" "$KEEP_MANIFEST"; then
    echo "Step 1: Main modules unchanged; skipping main regeneration."
else
    rm -f "$OUTPUT_DIGEST_MAIN"
    echo "Step 1: Generating main Python modules..."
    run_layer1_transform python "$PACKAGE" main \
        --output "$DIST_ROOT" --include-dsls \
        --prune-stale --keep-paths-from "$KEEP_MANIFEST"
    assemble_refresh_digest "$INPUT_DIGEST_MAIN" "$OUT_MAIN" "$OUTPUT_DIGEST_MAIN"
fi

# Step 2: Test modules. Any package can have a test source set (just
# `dist/json/<pkg>/src/test/json/`); the per-source-set digest mechanism
# is uniform — adding a test dir for any package automatically wires it
# into the build.
echo ""
if [ ! -d "$TEST_JSON_DIR" ]; then
    echo "Step 2: No test sources for $PACKAGE; skipping."
else
    if assemble_check_fresh "$INPUT_DIGEST_TEST" "$OUT_TEST" "$OUTPUT_DIGEST_TEST"; then
        echo "Step 2: Test modules unchanged; skipping test regeneration."
    else
        rm -f "$OUTPUT_DIGEST_TEST"
        echo "Step 2: Generating test Python modules..."
        run_layer1_transform python "$PACKAGE" test \
            --output "$DIST_ROOT" \
            --prune-stale --keep-paths-from "$KEEP_MANIFEST"
        assemble_refresh_digest "$INPUT_DIGEST_TEST" "$OUT_TEST" "$OUTPUT_DIGEST_TEST"
    fi
fi

# Step 4: Generate per-package pyproject.toml so each dist/python/<pkg>/
# is a standalone publishable wheel build.
echo ""
echo "Step 4: Generating per-package pyproject.toml..."
HYDRA_ROOT_DIR="$HYDRA_ROOT_DIR" "$HYDRA_ROOT_DIR/bin/lib/generate-python-package-build.py" \
    "$PACKAGE" --out-dir "$OUT_DIR"

echo ""

echo "=== Done. $PACKAGE assembled under $OUT_DIR ==="
