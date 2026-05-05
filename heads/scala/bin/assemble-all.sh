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
source "$HYDRA_ROOT_DIR/bin/lib/assemble-common.sh"
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

# Refresh per-source-set digests for fresh-check cache. Driven by the
# batch emit set, not by walking dist: every package the batch generator
# emitted must have a main source set on disk, so a missing
# dist/scala/<pkg>/src/main/scala/ is an error rather than something to
# silently skip. Test source set is optional, gated on input test
# digest presence.
BATCH_PACKAGES=$(batch_emit_packages)
for pkg in $BATCH_PACKAGES; do
    pkg_dir="$DIST_ROOT/$pkg"
    # Main set: required.
    input_digest="$HYDRA_ROOT_DIR/dist/json/$pkg/src/main/digest.json"
    out_set_dir="$pkg_dir/src/main/scala"
    out_digest="$pkg_dir/src/main/digest.json"
    if [ ! -f "$input_digest" ]; then
        echo "ERROR: missing input digest for $pkg main: $input_digest" >&2
        exit 1
    fi
    if [ ! -d "$out_set_dir" ]; then
        echo "ERROR: missing generated output for $pkg main: $out_set_dir" >&2
        exit 1
    fi
    (cd "$HYDRA_ROOT_DIR/heads/haskell" && \
     stack exec digest-check -- refresh \
        --inputs "$input_digest" \
        --output-dir "$out_set_dir" \
        --output-digest "$out_digest")
    # Test set: optional, gated on input test digest presence.
    test_input_digest="$HYDRA_ROOT_DIR/dist/json/$pkg/src/test/digest.json"
    if [ -f "$test_input_digest" ]; then
        test_out_set_dir="$pkg_dir/src/test/scala"
        test_out_digest="$pkg_dir/src/test/digest.json"
        if [ ! -d "$test_out_set_dir" ]; then
            echo "ERROR: missing generated test output for $pkg: $test_out_set_dir" >&2
            exit 1
        fi
        (cd "$HYDRA_ROOT_DIR/heads/haskell" && \
         stack exec digest-check -- refresh \
            --inputs "$test_input_digest" \
            --output-dir "$test_out_set_dir" \
            --output-digest "$test_out_digest")
    fi
done

echo ""
echo "=== Done. Batch scala assembly complete under $DIST_ROOT ==="
