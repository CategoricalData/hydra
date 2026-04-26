#!/usr/bin/env bash
# Layer 2 assembler: produce a complete Haskell distribution for one package.
#
# Usage:
#   assemble-distribution.sh <pkg> [--dist-root <dir>] [--json-root <dir>]
#
# Produces <dist-root>/<pkg>/ (default: ../../dist/haskell/<pkg>/) by:
#   1. Calling Layer 1 transform-json-to-haskell.sh for main modules
#   2. Calling Layer 1 transform-json-to-haskell.sh for test modules (if the
#      package has any)
#   3. Applying package-specific post-processing (kernel: TestGraph patch)
#
# Assemblers do NOT run tests. Testing is Layer 2.5's job
# (test-distribution.sh), invoked separately. See
# feature_290_packaging-plan.md, "Sync system redesign / Layer 2".
#
# Note: the Haskell head has no per-package build file to generate — the
# monolithic packages/hydra-haskell/hydra.cabal references every dist subdir
# via its source-dirs list. Future work: per-package .cabal files.

set -euo pipefail

if [ $# -lt 1 ]; then
    echo "Usage: $0 <package> [--dist-root <dir>] [--json-root <dir>]" >&2
    echo "" >&2
    echo "Packages: hydra-kernel, hydra-haskell, hydra-java, hydra-python," >&2
    echo "          hydra-scala, hydra-lisp, hydra-pg, hydra-rdf," >&2
    echo "          hydra-coq, hydra-javascript, hydra-ext" >&2
    exit 1
fi

PACKAGE="$1"
shift

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_HASKELL_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_HASKELL_DIR/../.." && pwd )"

DIST_ROOT="$HYDRA_ROOT_DIR/dist/haskell"

while [ $# -gt 0 ]; do
    case "$1" in
        --dist-root) DIST_ROOT="$2"; shift 2 ;;
        --json-root) shift 2 ;;  # forwarded to Layer 1 via --dist-json-root
        *) shift ;;
    esac
done

OUT_DIR="$DIST_ROOT/$PACKAGE"
OUT_MAIN="$OUT_DIR/src/main/haskell"
OUT_TEST="$OUT_DIR/src/test/haskell"
DIST_JSON_ROOT="$HYDRA_ROOT_DIR/dist/json"
INPUT_DIGEST_MAIN="$DIST_JSON_ROOT/$PACKAGE/src/main/digest.json"
INPUT_DIGEST_TEST="$DIST_JSON_ROOT/$PACKAGE/src/test/digest.json"
OUTPUT_DIGEST_MAIN="$OUT_DIR/src/main/digest.json"
OUTPUT_DIGEST_TEST="$OUT_DIR/src/test/digest.json"
TEST_JSON_DIR="$DIST_JSON_ROOT/$PACKAGE/src/test/json"

echo "=== Assembling Haskell distribution: $PACKAGE ==="
echo "  Output: $OUT_DIR"
echo ""

# --synthesize-sources generates hand-equivalent Hydra.Sources.Decode.* and
# Hydra.Sources.Encode.* modules from type definitions. The synthesis filter
# picks up kernel type modules and the two hydra-pg type modules
# (hydra.pg.model, hydra.pg.mapping). hydra-pg's assembler passes the flag so
# its own source wrappers land in dist/haskell/hydra-pg/.
SYNTH_FLAG=""
case "$PACKAGE" in
    hydra-kernel|hydra-pg)
        SYNTH_FLAG="--synthesize-sources"
        ;;
esac

# Per-source-set freshness check via digest-check. See
# heads/java/bin/assemble-distribution.sh for the pattern; same shape
# across every target language.
source "$HYDRA_ROOT_DIR/bin/lib/assemble-common.sh"

# Step 1: Main modules.
if assemble_check_fresh "$INPUT_DIGEST_MAIN" "$OUT_MAIN" "$OUTPUT_DIGEST_MAIN"; then
    echo "Step 1: Main modules unchanged; skipping main regeneration."
else
    rm -f "$OUTPUT_DIGEST_MAIN"
    echo "Step 1: Generating main Haskell modules..."
    "$SCRIPT_DIR/transform-json-to-haskell.sh" "$PACKAGE" main \
        --output "$DIST_ROOT" --include-dsls $SYNTH_FLAG
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
        echo "Step 2: Generating test Haskell modules..."
        "$SCRIPT_DIR/transform-json-to-haskell.sh" "$PACKAGE" test \
            --output "$DIST_ROOT"
        assemble_refresh_digest "$INPUT_DIGEST_TEST" "$OUT_TEST" "$OUTPUT_DIGEST_TEST"
    fi
fi

# Step 3: Package-specific post-processing. None today — the generator emits
# Hydra.Test.TestEnv references directly; see sync-haskell.sh step 5 and
# docs/recipes/maintenance.md "Known accepted patches".

echo "=== Done. $PACKAGE assembled under $OUT_DIR ==="
