#!/usr/bin/env bash
# Layer 2 batch assembler: produce Java distributions for every package
# in a single bootstrap-from-json invocation. Much faster than calling
# assemble-distribution.sh once per package because the JSON universe is
# loaded only once.
#
# Usage:
#   assemble-all.sh [--dist-root <dir>]
#
# Produces <dist-root>/<pkg>/ for every package in one shot. Per-package
# post-processing matches assemble-distribution.sh.

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
source "$HYDRA_ROOT_DIR/bin/lib/common.sh"
source "$HYDRA_ROOT_DIR/bin/lib/batch-cache.sh"
if batch_cache_fresh "$DIST_ROOT" "$HYDRA_ROOT_DIR/dist/json"; then
    echo "  Cache hit: every per-package digest fresh; skipping batch."
    echo "=== Done (cache hit). ==="
    exit 0
fi

# Invalidate per-target digests so Stage 7 per-module freshness filter
# cannot trust records against potentially-missing output files.
rm -f "$DIST_ROOT"/*/src/main/digest.json "$DIST_ROOT"/*/src/test/digest.json

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

# Step 3: Per-package post-processing. Mirrors assemble-distribution.sh
# Step 3 — hydra-kernel only today. Must run BEFORE the digest refresh
# below: copy-kernel-runtime.sh writes into dist/java/hydra-kernel/src/main/java/,
# which the digest tracks; running it after refresh would leave those
# hand-written files unrecorded and trigger a needless re-run on the
# next 'fresh' check.
echo ""
echo "Step 3: Copying hand-written Java runtime into hydra-kernel dist..."
"$SCRIPT_DIR/copy-kernel-runtime.sh" --dist-root "$DIST_ROOT"

# Step 4: Generate per-package build.gradle + settings.gradle. Mirrors
# assemble-distribution.sh Step 4. The build files live at
# dist/java/<pkg>/{build.gradle,settings.gradle} — outside the
# src/<set>/java tree the digest tracks — so ordering with digest
# refresh is moot, but we run before it for consistency with Step 3.
echo ""
echo "Step 4: Generating per-package build.gradle for every package..."
for pkg_dir in "$DIST_ROOT"/*/; do
    pkg=$(basename "$pkg_dir")
    pkg_dir_trim="${pkg_dir%/}"
    if [ -f "$HYDRA_ROOT_DIR/packages/$pkg/package.json" ]; then
        HYDRA_ROOT_DIR="$HYDRA_ROOT_DIR" "$HYDRA_ROOT_DIR/bin/lib/generate-java-package-build.py" \
            "$pkg" --out-dir "$pkg_dir_trim"
    fi
done

# Refresh per-source-set digests for fresh-check cache. Each package
# gets up to two digests: src/main/digest.json (always) and
# src/test/digest.json (when test sources exist).
for pkg_dir in "$DIST_ROOT"/*/; do
    pkg=$(basename "$pkg_dir")
    pkg_dir_trim="${pkg_dir%/}"
    for set_name in main test; do
        input_digest="$HYDRA_ROOT_DIR/dist/json/$pkg/src/$set_name/digest.json"
        out_set_dir="$pkg_dir_trim/src/$set_name/java"
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
echo "=== Done. Batch Java assembly complete under $DIST_ROOT ==="
