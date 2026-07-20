#!/usr/bin/env bash
# Layer 2 assembler: produce a complete Go distribution for one package.
#
# Usage:
#   assemble-distribution.sh <pkg> [--dist-root <dir>]
#
# Produces <dist-root>/<pkg>/ (default: ../../dist/go/<pkg>/).
#
# The Go head is a "head bud" right now: this script regenerates main and
# test sources via the Haskell driver but does not yet emit per-package
# build files (no go.mod scaffolding here — the heads/go/go.mod covers
# the hand-written runtime; generated trees compile against it through a
# top-level workspace once that's wired).

set -euo pipefail

if [ $# -lt 1 ]; then
    echo "Usage: $0 <package> [--dist-root <dir>]" >&2
    exit 1
fi

PACKAGE="$1"
shift

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_GO_HEAD="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_GO_HEAD/../.." && pwd )"

DIST_ROOT="$HYDRA_ROOT_DIR/dist/go"

while [ $# -gt 0 ]; do
    case "$1" in
        --dist-root) DIST_ROOT="$2"; shift 2 ;;
        *) shift ;;
    esac
done

OUT_DIR="$DIST_ROOT/$PACKAGE"
OUT_MAIN="$OUT_DIR/src/main/go"
OUT_TEST="$OUT_DIR/src/test/go"
DIST_JSON_ROOT="$HYDRA_ROOT_DIR/dist/json"
INPUT_DIGEST_MAIN="$DIST_JSON_ROOT/$PACKAGE/build/main/digest.json"
INPUT_DIGEST_TEST="$DIST_JSON_ROOT/$PACKAGE/build/test/digest.json"
OUTPUT_DIGEST_MAIN="$OUT_DIR/build/main/digest.json"
OUTPUT_DIGEST_TEST="$OUT_DIR/build/test/digest.json"
TEST_JSON_DIR="$DIST_JSON_ROOT/$PACKAGE/src/test/json"

echo "=== Assembling Go distribution: $PACKAGE ==="
echo "  Output: $OUT_DIR"
echo ""

HASKELL_BIN="$HYDRA_ROOT_DIR/heads/haskell/bin"

source "$HYDRA_ROOT_DIR/bin/lib/assemble-common.sh"

# Per-target generator stamp; see assemble-common.sh and #347.
export_generation_env go

# Step 1: Main modules.
if assemble_check_fresh "$INPUT_DIGEST_MAIN" "$OUT_MAIN" "$OUTPUT_DIGEST_MAIN"; then
    echo "Step 1: Main modules unchanged; skipping main regeneration."
else
    rm -f "$OUTPUT_DIGEST_MAIN"
    echo "Step 1: Generating main Go modules..."
    "$HASKELL_BIN/transform-json-to-go.sh" "$PACKAGE" main \
        --output "$DIST_ROOT" --include-dsls \
        --prune-stale
    assemble_refresh_digest "$INPUT_DIGEST_MAIN" "$OUT_MAIN" "$OUTPUT_DIGEST_MAIN"
fi

# Step 2: Test modules (if present).
echo ""
if [ ! -d "$TEST_JSON_DIR" ]; then
    echo "Step 2: No test sources for $PACKAGE; skipping."
else
    if assemble_check_fresh "$INPUT_DIGEST_TEST" "$OUT_TEST" "$OUTPUT_DIGEST_TEST"; then
        echo "Step 2: Test modules unchanged; skipping test regeneration."
    else
        rm -f "$OUTPUT_DIGEST_TEST"
        echo "Step 2: Generating test Go modules..."
        "$HASKELL_BIN/transform-json-to-go.sh" "$PACKAGE" test \
            --output "$DIST_ROOT" \
            --prune-stale
        assemble_refresh_digest "$INPUT_DIGEST_TEST" "$OUT_TEST" "$OUTPUT_DIGEST_TEST"
    fi
fi

# Step 3 (hydra-kernel only): copy the hand-written runtime from the overlay
# tree into dist/. Runs AFTER the transform's --prune-stale pass so the prune
# can't remove the overlay files. The generated kernel imports these via the
# dist-local module path hydra.dev/hydra/lib/... (#434).
if [ "$PACKAGE" = "hydra-kernel" ]; then
    echo ""
    echo "Step 3: Copying hand-written Go runtime into hydra-kernel dist..."
    "$SCRIPT_DIR/copy-kernel-runtime.sh" --dist-root "$DIST_ROOT"
fi

# Step 4: emit the Go module files. The coder emits imports rooted at
# hydra.dev (Coder.hs goModulePath), so each source set must declare that
# module path — a mismatch makes every intra-kernel import unresolvable.
# Generated here, never hand-written into dist/.
GO_MODULE_PATH="hydra.dev"
GO_VERSION="1.22"

emit_go_mod() {
    local dir="$1" module="$2"
    [ -d "$dir" ] || return 0
    cat > "$dir/go.mod" <<EOF
// Note: this is an automatically generated file. Do not edit.
module $module

go $GO_VERSION
EOF
}

echo ""
echo "Step 4: Writing Go module files..."
emit_go_mod "$OUT_DIR/src/main/go" "$GO_MODULE_PATH"
if [ -d "$OUT_DIR/src/test/go" ]; then
    # The test source set is a separate module that consumes the main one.
    emit_go_mod "$OUT_DIR/src/test/go" "$GO_MODULE_PATH/test"
    cat >> "$OUT_DIR/src/test/go/go.mod" <<EOF

require $GO_MODULE_PATH v0.0.0
replace $GO_MODULE_PATH => ../../main/go
EOF
fi
echo "  Wrote go.mod for main${TEST_MOD_NOTE:-} (module $GO_MODULE_PATH)"

echo ""
echo "=== Done. $PACKAGE assembled under $OUT_DIR ==="
