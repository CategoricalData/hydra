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
INPUT_DIGEST="$HYDRA_ROOT_DIR/dist/json/$PACKAGE/digest.json"
OUTPUT_DIGEST="$OUT_DIR/digest.json"

echo "=== Assembling Haskell distribution: $PACKAGE ==="
echo "  Output: $OUT_DIR"
echo ""

# Freshness check: skip the slow path when nothing has changed.
if [ -f "$INPUT_DIGEST" ] && [ -f "$OUTPUT_DIGEST" ]; then
    if (cd "$HYDRA_HASKELL_DIR" && \
        stack exec digest-check -- fresh \
            --inputs "$INPUT_DIGEST" \
            --output-dir "$OUT_DIR" \
            --output-digest "$OUTPUT_DIGEST" 2>/dev/null); then
        echo "  Cache hit; skipping work."
        echo "=== Done. $PACKAGE assembled under $OUT_DIR (cache hit) ==="
        exit 0
    fi
fi

# Cache miss: invalidate the per-target digest so Stage 7's per-module
# freshness filter inside bootstrap-from-json can't trust stale records
# (output files may be missing/modified, so DSL-hash-only freshness is
# unreliable).
rm -f "$OUTPUT_DIGEST"

# Step 1: Main modules via Layer 1 transform. Routing is unconditional:
# every module lands under <DIST_ROOT>/<owning-pkg>/ based on its namespace,
# including transitive dependencies for this package. The output dir argument
# is the parent of the per-package dirs.
#
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

echo "Step 1: Generating main Haskell modules..."
"$SCRIPT_DIR/transform-json-to-haskell.sh" "$PACKAGE" main \
    --output "$DIST_ROOT" --include-dsls $SYNTH_FLAG

# Step 2: Test modules (if the package has any).
# Only hydra-kernel has tests today; the transform exits 0 cleanly if the
# test source set is empty, so we invoke unconditionally.
echo ""
echo "Step 2: Generating test Haskell modules..."
"$SCRIPT_DIR/transform-json-to-haskell.sh" "$PACKAGE" test \
    --output "$DIST_ROOT"

# Step 3: Package-specific post-processing.
case "$PACKAGE" in
    hydra-kernel)
        # TestGraph.hs patch: replace empty graph/context with TestEnv versions.
        # Hydra.Test.TestEnv is a hand-written bridge module checked in under
        # dist/haskell/hydra-kernel/src/test/haskell/Hydra/Test/TestEnv.hs;
        # bootstrap-from-json doesn't target it (not in testModules), so it
        # survives regeneration.
        TESTGRAPH="$OUT_DIR/src/test/haskell/Hydra/Test/TestGraph.hs"
        if [ -f "$TESTGRAPH" ]; then
            echo ""
            echo "Step 3: Patching TestGraph.hs..."
            # Portable in-place sed: use a .bak suffix then remove it.
            # (macOS sed -i requires an explicit suffix; GNU accepts empty one.)
            sed -i.bak 's/import qualified Hydra.Lexical as Lexical$/import qualified Hydra.Lexical as Lexical\nimport qualified Hydra.Test.TestEnv as TestEnv/' "$TESTGRAPH"
            sed -i.bak 's/testGraph = Lexical.emptyGraph/testGraph = TestEnv.testGraph testTypes/' "$TESTGRAPH"
            sed -i.bak 's/testContext = Lexical.emptyContext/testContext = TestEnv.testContext/' "$TESTGRAPH"
            rm -f "$TESTGRAPH.bak"
        fi
        ;;
    *)
        # No per-package post-processing for other packages today.
        ;;
esac

echo ""

# Refresh the per-target digest so future fresh-checks short-circuit.
if [ -f "$INPUT_DIGEST" ]; then
    (cd "$HYDRA_HASKELL_DIR" && \
     stack exec digest-check -- refresh \
        --inputs "$INPUT_DIGEST" \
        --output-dir "$OUT_DIR" \
        --output-digest "$OUTPUT_DIGEST")
fi

echo "=== Done. $PACKAGE assembled under $OUT_DIR ==="
