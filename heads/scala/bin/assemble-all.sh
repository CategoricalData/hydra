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
source "$HYDRA_ROOT_DIR/bin/lib/common.sh"
source "$HYDRA_ROOT_DIR/bin/lib/batch-cache.sh"
if batch_cache_fresh "$DIST_ROOT" "$HYDRA_ROOT_DIR/dist/json"; then
    echo "  Cache hit: every per-package digest fresh; skipping batch."
    echo "=== Done (cache hit). ==="
    exit 0
fi

cd "$HYDRA_ROOT_DIR/heads/haskell"
stack build hydra:exe:bootstrap-from-json >/dev/null 2>&1

# Invalidate per-target digests so Stage 7 can't trust stale records.
rm -f "$DIST_ROOT"/*/src/main/digest.json "$DIST_ROOT"/*/src/test/digest.json

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

# (testGraph.scala emptyGraph-to-buildTestGraph patch eliminated: the
# DSL emits hydra.test.testEnv.testGraph(testTypes) directly, and the
# hand-written heads/scala testEnv.scala resolves the call to
# TestSuiteRunner.buildTestGraph. Mirrors heads/scala/bin/assemble-distribution.sh.)

# Refresh per-source-set digests for fresh-check cache.
for pkg_dir in "$DIST_ROOT"/*/; do
    pkg=$(basename "$pkg_dir")
    pkg_dir_trim="${pkg_dir%/}"
    for set_name in main test; do
        input_digest="$HYDRA_ROOT_DIR/dist/json/$pkg/src/$set_name/digest.json"
        out_set_dir="$pkg_dir_trim/src/$set_name/scala"
        out_digest="$pkg_dir_trim/src/$set_name/digest.json"
        if [ -f "$input_digest" ] && [ -d "$out_set_dir" ]; then
            (cd "$HYDRA_ROOT_DIR/heads/haskell" && \
             stack exec digest-check -- refresh \
                --inputs "$input_digest" \
                --output-dir "$out_set_dir" \
                --output-digest "$out_digest")
        fi
    done
done

echo ""
echo "=== Done. Batch scala assembly complete under $DIST_ROOT ==="
