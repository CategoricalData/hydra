#!/usr/bin/env bash
# Layer 2 batch assembler: produce scala distributions for every
# package in a single bootstrap-from-json invocation. Much faster than
# calling assemble-distribution.sh once per package because the JSON
# universe is loaded only once.
set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HEAD_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HEAD_DIR/../.." && pwd )"

DIST_ROOT="$HYDRA_ROOT_DIR/dist/scala"

while [ $# -gt 0 ]; do
    case "$1" in
        --dist-root) DIST_ROOT="$2"; shift 2 ;;
        *) shift ;;
    esac
done

echo "=== Assembling scala distributions (batch mode, all packages) ==="
echo "  Output root: $DIST_ROOT"
echo ""

# Warm-cache short-circuit: skip BEFORE any stack invocation.
source "$HYDRA_ROOT_DIR/bin/lib/batch-cache.sh"
if batch_cache_fresh "$DIST_ROOT" "$HYDRA_ROOT_DIR/dist/json"; then
    echo "  Cache hit: every per-package digest fresh; skipping batch."
    echo "=== Done (cache hit). ==="
    exit 0
fi

cd "$HYDRA_ROOT_DIR/heads/haskell"
stack build hydra:exe:bootstrap-from-json >/dev/null 2>&1

# Invalidate per-target digests so Stage 7 can't trust stale records.
rm -f "$DIST_ROOT"/*/digest.json

echo "Step 1: Generating main scala modules for every package..."
stack exec bootstrap-from-json -- \
    --target scala \
    --all-packages \
    --include-coders --include-dsls \
    --output "$DIST_ROOT"

echo ""
echo "Step 2: Generating test scala modules..."
stack exec bootstrap-from-json -- \
    --target scala \
    --all-packages \
    --include-coders --include-dsls --include-tests \
    --output "$DIST_ROOT"

cd "$HYDRA_ROOT_DIR"

# Per-package post-processing: hydra-kernel testGraph.scala patch.
TESTGRAPH="$DIST_ROOT/hydra-kernel/src/test/scala/hydra/test/testGraph.scala"
if [ -f "$TESTGRAPH" ]; then
    echo ""
    echo "Step 3: Patching hydra-kernel testGraph.scala..."
    sed -i.bak 's/hydra\.lexical\.emptyGraph/hydra.TestSuiteRunner.buildTestGraph()/g' "$TESTGRAPH"
    rm -f "$TESTGRAPH.bak"
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
echo "=== Done. Batch scala assembly complete under $DIST_ROOT ==="
