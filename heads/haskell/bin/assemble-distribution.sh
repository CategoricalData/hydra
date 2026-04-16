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
    echo "          hydra-scala, hydra-lisp, hydra-pg, hydra-rdf" >&2
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

echo "=== Assembling Haskell distribution: $PACKAGE ==="
echo "  Output: $OUT_DIR"
echo ""

# Step 1: Main modules via Layer 1 transform. Uses --package-split under the
# hood so that dependencies generate to their own package dirs too; the scope
# filter selects just this package for generation.
#
# --synthesize-sources generates hand-equivalent Hydra.Sources.Decode.* and
# Hydra.Sources.Encode.* modules from kernel types. It only applies to
# hydra-kernel today (the synthesis filter excludes coder-package types,
# and non-kernel domain packages don't have synth coverage yet).
SYNTH_FLAG=""
if [ "$PACKAGE" = "hydra-kernel" ]; then
    SYNTH_FLAG="--synthesize-sources"
fi

echo "Step 1: Generating main Haskell modules..."
"$SCRIPT_DIR/transform-json-to-haskell.sh" "$PACKAGE" main \
    --output "$DIST_ROOT" --package-split --include-dsls $SYNTH_FLAG

# Step 2: Test modules (if the package has any).
# Only hydra-kernel has tests today; the transform exits 0 cleanly if the
# test source set is empty, so we invoke unconditionally.
echo ""
echo "Step 2: Generating test Haskell modules..."
"$SCRIPT_DIR/transform-json-to-haskell.sh" "$PACKAGE" test \
    --output "$DIST_ROOT" --package-split

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
echo "=== Done. $PACKAGE assembled under $OUT_DIR ==="
