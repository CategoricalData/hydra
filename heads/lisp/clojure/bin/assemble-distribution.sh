#!/usr/bin/env bash
# Layer 2 assembler: produce a complete Clojure distribution for one package.
#
# Usage:
#   assemble-distribution.sh <pkg> [--dist-root <dir>]
#
# Produces <dist-root>/<pkg>/ (default: ../../../dist/clojure/<pkg>/).

set -euo pipefail

if [ $# -lt 1 ]; then
    echo "Usage: $0 <package> [--dist-root <dir>]" >&2
    exit 1
fi

PACKAGE="$1"
shift

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_CLOJURE_HEAD="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_CLOJURE_HEAD/../../.." && pwd )"

DIST_ROOT="$HYDRA_ROOT_DIR/dist/clojure"

while [ $# -gt 0 ]; do
    case "$1" in
        --dist-root) DIST_ROOT="$2"; shift 2 ;;
        *) shift ;;
    esac
done

OUT_DIR="$DIST_ROOT/$PACKAGE"
OUT_MAIN="$OUT_DIR/src/main/clojure"
OUT_TEST="$OUT_DIR/src/test/clojure"
DIST_JSON_ROOT="$HYDRA_ROOT_DIR/dist/json"
INPUT_DIGEST_MAIN="$DIST_JSON_ROOT/$PACKAGE/src/main/digest.json"
INPUT_DIGEST_TEST="$DIST_JSON_ROOT/$PACKAGE/src/test/digest.json"
OUTPUT_DIGEST_MAIN="$OUT_DIR/src/main/digest.json"
OUTPUT_DIGEST_TEST="$OUT_DIR/src/test/digest.json"
TEST_JSON_DIR="$DIST_JSON_ROOT/$PACKAGE/src/test/json"

echo "=== Assembling Clojure distribution: $PACKAGE ==="
echo "  Output: $OUT_DIR"
echo ""

HASKELL_BIN="$HYDRA_ROOT_DIR/heads/haskell/bin"

# Per-source-set freshness check via digest-check. See
# heads/java/bin/assemble-distribution.sh for the pattern; same shape
# across every target language.
source "$HYDRA_ROOT_DIR/bin/lib/assemble-common.sh"

# Step 1: Main modules.
if assemble_check_fresh "$INPUT_DIGEST_MAIN" "$OUT_MAIN" "$OUTPUT_DIGEST_MAIN"; then
    echo "Step 1: Main modules unchanged; skipping main regeneration."
else
    rm -f "$OUTPUT_DIGEST_MAIN"
    echo "Step 1: Generating main Clojure modules..."
    "$HASKELL_BIN/transform-json-to-lisp.sh" "$PACKAGE" clojure main \
        --output "$DIST_ROOT"
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
        echo "Step 2: Generating test Clojure modules..."
        "$HASKELL_BIN/transform-json-to-lisp.sh" "$PACKAGE" clojure test \
            --output "$DIST_ROOT"
        assemble_refresh_digest "$INPUT_DIGEST_TEST" "$OUT_TEST" "$OUTPUT_DIGEST_TEST"
    fi
fi

# Step 3: Package-specific post-processing.
# (Clojure exception: do NOT copy heads/lisp/clojure/.../lib/ into
# dist/clojure/hydra-kernel/. heads/lisp/clojure/deps.edn lists both
# heads/lisp/clojure/src/main/clojure AND dist/clojure/hydra-kernel/...
# on :paths, so a copy would create namespace conflicts at load time.)
case "$PACKAGE" in
    hydra-kernel)
        # Copy hand-written testEnv.clj into dist tree. The kernel
        # filters hydra.test.testEnv from emitted output (via
        # testSkipEmitNamespaces), so the generated testGraph.clj's
        # (hydra.test.testEnv :refer :all) require resolves against
        # this hand-written counterpart. Mirrors the role of
        # heads/python/.../test_env.py and heads/lisp/scheme/.../test_env.scm.
        TEST_ENV_SRC="$HYDRA_CLOJURE_HEAD/src/test/clojure/hydra/test/testEnv.clj"
        TEST_ENV_DST="$OUT_DIR/src/test/clojure/hydra/test/testEnv.clj"
        if [ -f "$TEST_ENV_SRC" ]; then
            echo ""
            echo "Step 3: Copying testEnv.clj from heads/lisp/clojure..."
            mkdir -p "$(dirname "$TEST_ENV_DST")"
            cp "$TEST_ENV_SRC" "$TEST_ENV_DST"
        fi
        ;;
    *)
        ;;
esac

echo ""

echo "=== Done. $PACKAGE assembled under $OUT_DIR ==="
