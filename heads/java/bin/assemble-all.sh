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

# Warm-cache short-circuit: skip BEFORE any stack invocation or digest wipe.
source "$HYDRA_ROOT_DIR/bin/lib/batch-cache.sh"
if batch_cache_fresh "$DIST_ROOT" "$HYDRA_ROOT_DIR/dist/json"; then
    echo "  Cache hit: every per-package digest fresh; skipping batch."
    echo "=== Done (cache hit). ==="
    exit 0
fi

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

# Per-package post-processing: hydra-lisp Coder.
# (TestGraph.java post-generation patch has been eliminated: the DSL now
# emits TestEnv refs directly. See task #25 in feature_290_packaging plan.)
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
    # Strip trailing slash so the digest path is stable.
    pkg_dir_trim="${pkg_dir%/}"
    output_digest="$pkg_dir_trim/digest.json"
    if [ -f "$input_digest" ]; then
        (cd "$HYDRA_ROOT_DIR/heads/haskell" && \
         stack exec digest-check -- refresh \
            --inputs "$input_digest" \
            --output-dir "$pkg_dir_trim" \
            --output-digest "$output_digest")
    fi
done

echo ""
echo "=== Done. Batch Java assembly complete under $DIST_ROOT ==="
