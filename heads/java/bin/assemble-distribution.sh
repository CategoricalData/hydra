#!/usr/bin/env bash
# Layer 2 assembler: produce a complete Java distribution for one package.
#
# Usage:
#   assemble-distribution.sh <pkg> [--dist-root <dir>]
#
# Produces <dist-root>/<pkg>/ (default: ../../dist/java/<pkg>/) by:
#   1. Calling Layer 1 transform-json-to-java.sh for main modules
#   2. Calling Layer 1 transform-json-to-java.sh for test modules
#   3. Applying package-specific post-processing:
#      - hydra-kernel: copy hand-written Java runtime (util, lib, dsl, json,
#        tools) into the dist tree so the published Maven artifact is
#        self-contained.
#   4. Generating a per-package build.gradle + settings.gradle so each
#      dist/java/<pkg>/ is a standalone, publishable Gradle build.
#
# Assemblers do NOT run tests; see test-distribution.sh (Layer 2.5).

set -euo pipefail

if [ $# -lt 1 ]; then
    echo "Usage: $0 <package> [--dist-root <dir>]" >&2
    exit 1
fi

PACKAGE="$1"
shift

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

OUT_DIR="$DIST_ROOT/$PACKAGE"
DIST_JSON_ROOT="$HYDRA_ROOT_DIR/dist/json"
INPUT_DIGEST_MAIN="$DIST_JSON_ROOT/$PACKAGE/build/main/digest.json"
INPUT_DIGEST_TEST="$DIST_JSON_ROOT/$PACKAGE/build/test/digest.json"
OUTPUT_DIGEST_MAIN="$OUT_DIR/build/main/digest.json"
OUTPUT_DIGEST_TEST="$OUT_DIR/build/test/digest.json"
OUT_MAIN_DIR="$OUT_DIR/src/main/java"
OUT_TEST_DIR="$OUT_DIR/src/test/java"
TEST_JSON_DIR="$DIST_JSON_ROOT/$PACKAGE/src/test/json"

echo "=== Assembling Java distribution: $PACKAGE ==="
echo "  Output: $OUT_DIR"
echo ""

HASKELL_BIN="$HYDRA_ROOT_DIR/heads/haskell/bin"

# Per-source-set freshness check via digest-check. The per-source-set
# digest pair (input at dist/json/<pkg>/src/<set>/digest.json, output at
# dist/<lang>/<pkg>/src/<set>/digest.json) is the single source of
# truth for "does this set need to regen?". Replaces the older split of
# per-package digest.json + .{main,test}-input-hash.txt dotfiles.
source "$HYDRA_ROOT_DIR/bin/lib/common.sh"
source "$HYDRA_ROOT_DIR/bin/lib/assemble-common.sh"

# Per-target generator stamp: invalidates output digests when the
# Java-emit transform's sources change without the per-package JSON
# inputs themselves changing. See assemble-common.sh and #347.
export HYDRA_GENERATOR_STAMP=$(compute_generator_stamp java)

# Step 0 (hydra-kernel only): drop hand-written runtime files BEFORE
# generation, recording them in a keep-paths manifest so #357 prune in
# bootstrap-from-json --prune-stale below won't remove them. Was Step 3
# in the pre-#357 layout; moving it up preserves the published Maven
# artifact's runtime support while letting the prune step delete stale
# generated classes from removed modules.
KEEP_MANIFEST="$(mktemp -t hydra-keep-paths-java.XXXXXX)"
trap 'rm -f "$KEEP_MANIFEST"' EXIT
case "$PACKAGE" in
    hydra-kernel)
        echo "Step 0: Copying hand-written Java runtime into hydra-kernel dist..."
        "$SCRIPT_DIR/copy-kernel-runtime.sh" --dist-root "$DIST_ROOT" --manifest "$KEEP_MANIFEST"
        echo ""
        ;;
esac

# Step 1: Main modules.
if assemble_check_fresh "$INPUT_DIGEST_MAIN" "$OUT_MAIN_DIR" "$OUTPUT_DIGEST_MAIN"; then
    echo "Step 1: Main modules unchanged; skipping main regeneration."
else
    rm -f "$OUTPUT_DIGEST_MAIN"
    echo "Step 1: Generating main Java modules..."
    "$HASKELL_BIN/transform-json-to-java.sh" "$PACKAGE" main \
        --output "$DIST_ROOT" --include-dsls \
        --prune-stale --keep-paths-from "$KEEP_MANIFEST"
    assemble_refresh_digest "$INPUT_DIGEST_MAIN" "$OUT_MAIN_DIR" "$OUTPUT_DIGEST_MAIN"
fi

# Step 2: Test modules. Any package can have a test source set (just
# `dist/json/<pkg>/src/test/json/`); the per-source-set digest mechanism
# is uniform — adding a test dir for any package automatically wires it
# into the build.
echo ""
if [ ! -d "$TEST_JSON_DIR" ]; then
    echo "Step 2: No test sources for $PACKAGE; skipping."
else
    if assemble_check_fresh "$INPUT_DIGEST_TEST" "$OUT_TEST_DIR" "$OUTPUT_DIGEST_TEST"; then
        echo "Step 2: Test modules unchanged; skipping test regeneration."
    else
        rm -f "$OUTPUT_DIGEST_TEST"
        echo "Step 2: Generating test Java modules..."
        "$HASKELL_BIN/transform-json-to-java.sh" "$PACKAGE" test \
            --output "$DIST_ROOT" \
            --prune-stale --keep-paths-from "$KEEP_MANIFEST"
        assemble_refresh_digest "$INPUT_DIGEST_TEST" "$OUT_TEST_DIR" "$OUTPUT_DIGEST_TEST"
    fi
fi

# Step 4: Generate per-package build.gradle + settings.gradle so each
# dist/java/<pkg>/ is a standalone publishable Gradle build.
echo ""
echo "Step 4: Generating per-package build.gradle..."
HYDRA_ROOT_DIR="$HYDRA_ROOT_DIR" "$HYDRA_ROOT_DIR/bin/lib/generate-java-package-build.py" \
    "$PACKAGE" --out-dir "$OUT_DIR"

echo ""

echo "=== Done. $PACKAGE assembled under $OUT_DIR ==="
