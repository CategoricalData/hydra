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
INPUT_DIGEST_MAIN="$DIST_JSON_ROOT/$PACKAGE/src/main/digest.json"
INPUT_DIGEST_TEST="$DIST_JSON_ROOT/$PACKAGE/src/test/digest.json"
OUTPUT_DIGEST_MAIN="$OUT_DIR/src/main/digest.json"
OUTPUT_DIGEST_TEST="$OUT_DIR/src/test/digest.json"
TEST_JSON_DIR="$DIST_JSON_ROOT/$PACKAGE/src/test/json"

echo "=== Assembling Python distribution: $PACKAGE ==="
echo "  Output: $OUT_DIR"
echo ""

HASKELL_BIN="$HYDRA_ROOT_DIR/heads/haskell/bin"

# Per-source-set freshness check via digest-check. See
# heads/java/bin/assemble-distribution.sh for the pattern; same shape
# across every target language.
source "$HYDRA_ROOT_DIR/bin/lib/common.sh"
source "$HYDRA_ROOT_DIR/bin/lib/assemble-common.sh"

# Per-target generator stamp; see assemble-common.sh and #347.
export HYDRA_GENERATOR_STAMP=$(compute_generator_stamp python)

# Step 1: Main modules.
if assemble_check_fresh "$INPUT_DIGEST_MAIN" "$OUT_MAIN" "$OUTPUT_DIGEST_MAIN"; then
    echo "Step 1: Main modules unchanged; skipping main regeneration."
else
    rm -f "$OUTPUT_DIGEST_MAIN"
    echo "Step 1: Generating main Python modules..."
    "$HASKELL_BIN/transform-json-to-python.sh" "$PACKAGE" main \
        --output "$DIST_ROOT" --include-dsls
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
        "$HASKELL_BIN/transform-json-to-python.sh" "$PACKAGE" test \
            --output "$DIST_ROOT"
        assemble_refresh_digest "$INPUT_DIGEST_TEST" "$OUT_TEST" "$OUTPUT_DIGEST_TEST"
    fi
fi

# Step 3: Package-specific post-processing.
# - hydra-kernel: copy test_env.py (the Python runtime counterpart of
#   hydra.test.testEnv, filtered from emitted output by
#   testSkipEmitNamespaces) into the dist test tree, then copy the
#   hand-written runtime support (lib/, dsl/, sources/, tools.py,
#   py.typed) into dist/python/hydra-kernel/ so the published wheel is
#   self-contained.
case "$PACKAGE" in
    hydra-kernel)
        # Copy test_env.py from heads/python into dist/. The generated
        # test_graph.py imports hydra.test.test_env, which must resolve
        # under the dist tree at test time.
        TEST_ENV_SRC="$HYDRA_PYTHON_HEAD/src/test/python/hydra/test/test_env.py"
        TEST_ENV_DST="$OUT_TEST/hydra/test/test_env.py"
        if [ -f "$TEST_ENV_SRC" ]; then
            echo ""
            echo "Step 3a: Copying test_env.py from heads/python/..."
            mkdir -p "$(dirname "$TEST_ENV_DST")"
            cp "$TEST_ENV_SRC" "$TEST_ENV_DST"
        fi

        # Step 3b: Copy hand-written Python runtime so the published kernel
        # wheel is self-contained.
        echo "Step 3b: Copying hand-written Python runtime into hydra-kernel dist..."
        "$SCRIPT_DIR/copy-kernel-runtime.sh" --dist-root "$DIST_ROOT"
        ;;
esac

# Step 4: Generate per-package pyproject.toml so each dist/python/<pkg>/
# is a standalone publishable wheel build.
echo ""
echo "Step 4: Generating per-package pyproject.toml..."
HYDRA_ROOT_DIR="$HYDRA_ROOT_DIR" "$HYDRA_ROOT_DIR/bin/lib/generate-python-package-build.py" \
    "$PACKAGE" --out-dir "$OUT_DIR"

echo ""

echo "=== Done. $PACKAGE assembled under $OUT_DIR ==="
