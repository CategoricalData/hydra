#!/usr/bin/env bash
# Layer 2 batch assembler: produce Java distributions for every package
# in a single bootstrap-from-json invocation. Much faster than calling
# assemble-distribution.sh once per package because the JSON universe is
# loaded only once.
#
# Usage:
#   assemble-all.sh [--dist-root <dir>]
#
# Produces <dist-root>/<pkg>/ for every package in one shot. Applies the
# same per-package post-processing sed patches as
# assemble-distribution.sh (TestGraph, Lisp Coder).

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_JAVA_HEAD="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_JAVA_HEAD/../.." && pwd )"

DIST_ROOT="$HYDRA_ROOT_DIR/dist/java"

while [ $# -gt 0 ]; do
    case "$1" in
        --dist-root) DIST_ROOT="$2"; shift 2 ;;
        *) shift ;;
    esac
done

echo "=== Assembling Java distributions (batch mode, all packages) ==="
echo "  Output root: $DIST_ROOT"
echo ""

HASKELL_BIN="$HYDRA_ROOT_DIR/heads/haskell/bin"

# Invalidate per-target digests so Stage 7 per-module freshness filter
# cannot trust records against potentially-missing output files.
rm -f "$DIST_ROOT"/*/digest.json

echo "Step 1: Generating main Java modules for every package..."
cd "$HYDRA_ROOT_DIR/heads/haskell"
stack build hydra:exe:bootstrap-from-json >/dev/null 2>&1
stack exec bootstrap-from-json -- \
    --target java \
    --all-packages \
    --include-coders --include-dsls \
    --output "$DIST_ROOT"

echo ""
echo "Step 2: Generating test Java modules..."
stack exec bootstrap-from-json -- \
    --target java \
    --all-packages \
    --include-coders --include-dsls --include-tests \
    --output "$DIST_ROOT"

cd "$HYDRA_ROOT_DIR"

# Per-package post-processing: hydra-kernel TestGraph, hydra-lisp Coder.
TESTGRAPH="$DIST_ROOT/hydra-kernel/src/test/java/hydra/test/TestGraph.java"
if [ -f "$TESTGRAPH" ]; then
    echo ""
    echo "Step 3a: Patching hydra-kernel TestGraph.java..."
    sed -i.bak 's/return hydra.Lexical.emptyGraph();/return hydra.test.TestEnv.testGraph();/' "$TESTGRAPH"
    sed -i.bak 's/return hydra.Lexical.emptyContext();/return hydra.test.TestEnv.testContext();/' "$TESTGRAPH"
    rm -f "$TESTGRAPH.bak"
fi

LISPCODER="$DIST_ROOT/hydra-lisp/src/main/java/hydra/lisp/Coder.java"
if [ -f "$LISPCODER" ]; then
    echo "Step 3b: Patching hydra-lisp Coder.java (PartialVisitor type inference)..."
    sed -i.bak 's/Either<hydra.lisp.syntax.TopLevelFormWithComments, hydra.lisp.syntax.TopLevelFormWithComments> otherwise/Either<T2, hydra.lisp.syntax.TopLevelFormWithComments> otherwise/' "$LISPCODER"
    sed -i.bak 's/Either<hydra.lisp.syntax.TopLevelFormWithComments, hydra.lisp.syntax.TopLevelFormWithComments> visit/Either<T2, hydra.lisp.syntax.TopLevelFormWithComments> visit/' "$LISPCODER"
    rm -f "$LISPCODER.bak"
fi

# Refresh per-target digests for fresh-check cache.
for pkg_dir in "$DIST_ROOT"/*/; do
    pkg=$(basename "$pkg_dir")
    input_digest="$HYDRA_ROOT_DIR/dist/json/$pkg/digest.json"
    output_digest="$pkg_dir/digest.json"
    if [ -f "$input_digest" ]; then
        (cd "$HYDRA_ROOT_DIR/heads/haskell" && \
         stack exec digest-check -- refresh \
            --inputs "$input_digest" \
            --output-dir "$pkg_dir" \
            --output-digest "$output_digest")
    fi
done

echo ""
echo "=== Done. Batch Java assembly complete under $DIST_ROOT ==="
