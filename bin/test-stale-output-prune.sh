#!/usr/bin/env bash
# Regression test for #357 generator-side stale-output pruning.
#
# Drops sentinel files into a per-package dist tree, invokes
# bootstrap-from-json --prune-stale --package <pkg>, and asserts:
#   1. The sentinel that mimics a stale generated file is REMOVED.
#   2. For targets with hand-written runtime (Java/Python), an existing
#      runtime file passed via --keep-paths-from is PRESERVED.
#
# Calls bootstrap-from-json directly (not assemble-distribution.sh) so the
# test stays focused on the prune mechanism rather than incidentally
# exercising digest-check, gradle stubs, etc.
#
# Usage:
#   bin/test-stale-output-prune.sh [haskell|java|python|scala|all]

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"

TARGETS=("${1:-all}")
if [ "${TARGETS[0]}" = "all" ]; then
    TARGETS=(haskell java python scala)
fi

PASS=0
FAIL=0

run_one_target() {
    local target="$1"
    local pkg="hydra-kernel"
    local lang_ext ext_for_sentinel

    case "$target" in
        haskell) lang_ext="haskell"; ext_for_sentinel="hs" ;;
        java)    lang_ext="java";    ext_for_sentinel="java" ;;
        python)  lang_ext="python";  ext_for_sentinel="py" ;;
        scala)   lang_ext="scala";   ext_for_sentinel="scala" ;;
        *)       echo "unknown target: $target" >&2; return 1 ;;
    esac

    local dist_root="$HYDRA_ROOT_DIR/dist/$target"
    local main_dir="$dist_root/$pkg/src/main/$lang_ext"

    if [ ! -d "$main_dir" ]; then
        echo "[$target] SKIP: $main_dir missing (run sync first)"
        return 0
    fi

    echo "[$target] Setup: dropping sentinel into $main_dir"

    # Sentinel #1: simulates a stale generated file from a deleted source
    # module. Content is the minimal valid form for the language so that
    # any preceding stack/gradle/pytest pickup doesn't fail before prune.
    local stale_dir="$main_dir/HydraTestStaleSentinel"
    mkdir -p "$stale_dir"
    local stale_sentinel="$stale_dir/StaleFile.$ext_for_sentinel"
    case "$target" in
        haskell)
            cat > "$stale_sentinel" <<'EOF'
-- #357 test sentinel - should be pruned
module HydraTestStaleSentinel.StaleFile where
EOF
            ;;
        java)
            cat > "$stale_sentinel" <<'EOF'
// #357 test sentinel - should be pruned
package hydra.testStaleSentinel;
public final class StaleFile {}
EOF
            ;;
        python)
            cat > "$stale_sentinel" <<'EOF'
# #357 test sentinel - should be pruned
EOF
            ;;
        scala)
            cat > "$stale_sentinel" <<'EOF'
// #357 test sentinel - should be pruned
package hydra.testStaleSentinel
object StaleFile
EOF
            ;;
    esac

    # Sentinel #2 (Java/Python only): pick an existing hand-written
    # runtime file and verify it's preserved when its path is in the
    # keep-paths manifest. If the runtime file isn't on disk for some
    # reason, skip that half of the assertion.
    local runtime_check=""
    local keep_manifest=""
    case "$target" in
        java)
            runtime_check="$main_dir/hydra/dsl/Terms.java"
            ;;
        python)
            runtime_check="$main_dir/hydra/dsl/terms.py"
            ;;
    esac

    if [ -n "$runtime_check" ] && [ -f "$runtime_check" ]; then
        keep_manifest="$(mktemp -t test-prune-keep.XXXXXX)"
        local rel="${runtime_check#$main_dir/}"
        printf "%s\t%s\n" "$main_dir" "$rel" > "$keep_manifest"
    fi

    # Pick load flags + output target. Mirrors transform-json-to-target.sh
    # for the hydra-kernel case (no extra coder includes needed).
    local extra_flags=""
    if [ -n "$keep_manifest" ]; then
        extra_flags="--keep-paths-from $keep_manifest"
    fi
    local synth_flag=""
    if [ "$target" = "haskell" ]; then
        synth_flag="--synthesize-sources"
    fi

    echo "[$target] Running bootstrap-from-json --prune-stale --package $pkg"
    local log="/tmp/test-prune-$target.log"
    if ! ( cd "$HYDRA_ROOT_DIR/heads/haskell" && \
           stack exec bootstrap-from-json -- \
               --target "$target" \
               --package "$pkg" \
               --include-dsls $synth_flag \
               --prune-stale $extra_flags \
               --output "$dist_root" ) > "$log" 2>&1; then
        echo "[$target] FAIL: bootstrap-from-json exit nonzero (log: $log)"
        tail -20 "$log"
        FAIL=$((FAIL + 1))
        [ -n "$keep_manifest" ] && rm -f "$keep_manifest"
        return
    fi

    # Sentinel #1 must be gone.
    if [ -f "$stale_sentinel" ]; then
        echo "[$target] FAIL: stale sentinel still present at $stale_sentinel"
        rm -rf "$stale_dir"
        FAIL=$((FAIL + 1))
        [ -n "$keep_manifest" ] && rm -f "$keep_manifest"
        return
    fi

    # The stale_dir should also be gone (empty-dir cleanup).
    if [ -d "$stale_dir" ]; then
        echo "[$target] WARN: empty dir $stale_dir survived prune"
    fi

    # Sentinel #2 must still be present.
    if [ -n "$runtime_check" ] && [ -n "$keep_manifest" ] && [ ! -f "$runtime_check" ]; then
        echo "[$target] FAIL: kept-manifest file $runtime_check was pruned"
        FAIL=$((FAIL + 1))
        rm -f "$keep_manifest"
        return
    fi

    [ -n "$keep_manifest" ] && rm -f "$keep_manifest"
    echo "[$target] PASS"
    PASS=$((PASS + 1))
}

for t in "${TARGETS[@]}"; do
    run_one_target "$t" || true
done

echo ""
echo "=== test-stale-output-prune.sh: $PASS pass, $FAIL fail ==="

if [ "$FAIL" -gt 0 ]; then
    exit 1
fi
