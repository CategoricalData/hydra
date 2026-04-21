#!/usr/bin/env bash
# Test-cache helpers for Layer 2.5 testers.
#
# A target's test run is idempotent given:
#   * every generated source under dist/<lang>/*/src/{main,test}/*
#   * every hand-written runner/helper under heads/<lang>/src/test/*
#   * the test runner script itself (this file's caller)
#
# If the hash of that set is unchanged since the last successful run,
# skipping the test invocation is safe. Each test-distribution.sh stamps
# dist/<lang>/test-cache.json after success and checks it on entry.

set -euo pipefail

_THIS="${BASH_SOURCE[0]}"
# Invoked by test-distribution.sh via `source`, so caller's BASH_SOURCE[1]
# gives us the path we're protecting.

# Usage: test_cache_check <lang> <dist-root> <heads-test-dir> <runner-script>
#   <lang>           e.g. "java", "python", "haskell"
#   <dist-root>      e.g. "$HYDRA_ROOT_DIR/dist/java"
#   <heads-test-dir> e.g. "$HYDRA_ROOT_DIR/heads/java/src/test"
#   <runner-script>  e.g. "${BASH_SOURCE[0]}" from the caller
#
# Returns 0 (cache hit) if the current hash matches the recorded one.
# Returns 1 (cache miss / no cache) otherwise.
test_cache_check() {
    local lang="$1"
    local dist_root="$2"
    local heads_test_dir="$3"
    local runner_script="$4"
    local cache_file="$dist_root/test-cache.json"

    if [ ! -f "$cache_file" ]; then
        return 1
    fi

    local current_hash
    current_hash=$(_compute_test_hash "$dist_root" "$heads_test_dir" "$runner_script")
    local recorded_hash
    recorded_hash=$(python3 -c "
import json, sys
try:
    print(json.load(open('$cache_file'))['hash'])
except Exception:
    sys.exit(0)
" 2>/dev/null || echo "")

    if [ -n "$recorded_hash" ] && [ "$current_hash" = "$recorded_hash" ]; then
        return 0
    fi
    return 1
}

# Usage: test_cache_record <lang> <dist-root> <heads-test-dir> <runner-script>
#   Same args as test_cache_check. Called after a successful test run.
test_cache_record() {
    local lang="$1"
    local dist_root="$2"
    local heads_test_dir="$3"
    local runner_script="$4"
    local cache_file="$dist_root/test-cache.json"

    local h
    h=$(_compute_test_hash "$dist_root" "$heads_test_dir" "$runner_script")
    mkdir -p "$dist_root"
    python3 -c "
import json
with open('$cache_file', 'w') as f:
    json.dump({'hash': '$h'}, f)
    f.write('\n')
"
}

# Hash the union of:
#   - every *.<lang-ext> file under dist-root
#   - every file under heads-test-dir
#   - the runner script itself
_compute_test_hash() {
    local dist_root="$1"
    local heads_test_dir="$2"
    local runner_script="$3"
    # Collect all relevant files, sort for determinism, hash each + list.
    {
        if [ -d "$dist_root" ]; then
            find "$dist_root" -type f \
                \( -name "*.hs" -o -name "*.java" -o -name "*.py" \
                   -o -name "*.scala" -o -name "*.clj" -o -name "*.scm" \
                   -o -name "*.lisp" -o -name "*.el" \) \
                -not -name "test-cache.json" 2>/dev/null
        fi
        if [ -d "$heads_test_dir" ]; then
            find "$heads_test_dir" -type f 2>/dev/null
        fi
        echo "$runner_script"
    } | LC_ALL=C sort | xargs shasum -a 256 2>/dev/null | shasum -a 256 | awk '{print $1}'
}
