# Shared utilities for Hydra shell scripts.
#
# Source this file from other scripts:
#   SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
#   REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"  # or .. / ../.. etc.
#   source "$REPO_ROOT/bin/lib/common.sh"
#
# This file is intended to be sourced, not executed.

# ---------------------------------------------------------------------------
# Banner widths
#
# Three levels, each a consistent width (use a single hline per message):
#   LEVEL 1 (orchestrator):  48 wide, "=" (e.g. sync-all, prepare-release, bootstrap)
#   LEVEL 2 (script):        44 wide, "-" (e.g. sync-haskell, sync-java headers)
#   LEVEL 3 (step):          no banner, "Step N/M: ..." line only
# ---------------------------------------------------------------------------

BANNER_LEVEL1_WIDTH=48
BANNER_LEVEL2_WIDTH=44

# Print the given character repeated N times.
_repeat_char() {
    local char="$1"
    local count="$2"
    printf '%.0s'"$char" $(seq 1 "$count")
    printf '\n'
}

# Level-1 banner (orchestrator): "=" x 48, one line above the title.
# Usage: banner1 "Hydra full sync"
banner1() {
    _repeat_char "=" "$BANNER_LEVEL1_WIDTH"
    echo "$1"
}

# Level-2 banner (script): "-" x 44, one line above the title.
# Usage: banner2 "Synchronizing Hydra-Haskell"
banner2() {
    _repeat_char "-" "$BANNER_LEVEL2_WIDTH"
    echo "$1"
}

# Step line (no banner): "Step N/M: <title>"
# Usage: step 2 8 "Building executable"
step() {
    local n="$1"
    local total="$2"
    local title="$3"
    echo ""
    echo "Step $n/$total: $title"
}

# Print a completion banner at level 1. Typically the final line a script emits.
# Usage: banner1_done "Full sync complete!"
banner1_done() {
    echo ""
    banner1 "$1"
}

# Print a completion banner at level 2.
# Usage: banner2_done "Hydra-Haskell sync complete!"
banner2_done() {
    echo ""
    banner2 "$1"
}

# ---------------------------------------------------------------------------
# Logging
# ---------------------------------------------------------------------------

# Write an error message to stderr and exit with the given code (default 1).
# Usage: die "something broke" [exit_code]
die() {
    echo "ERROR: $1" >&2
    exit "${2:-1}"
}

# Write a warning message to stderr.
warn() {
    echo "WARNING: $1" >&2
}

# Write an informational message to stderr.
info() {
    echo "$1" >&2
}

# Fail (or warn) if the selected JDK is an x86_64 build running under Rosetta 2
# on an Apple Silicon host. Such JDKs are ~20x slower than a native arm64 JDK
# and produce bogus benchmark timings. Set HYDRA_ALLOW_ROSETTA_JDK=1 to opt in
# (e.g. if the environment genuinely has no native JDK available); in that case
# the helper emits a warning and returns success.
#
# Usage: check_native_jdk          # fatal (exit 1) on Rosetta, unless opted in
#        check_native_jdk --warn   # warn only, never fail
check_native_jdk() {
    local mode="${1:-}"
    [ "$(uname -s)" = "Darwin" ] || return 0
    [ "$(uname -m)" = "arm64" ] || return 0
    local java_cmd="${JAVA_HOME:+$JAVA_HOME/bin/}java"
    command -v "$java_cmd" >/dev/null 2>&1 || return 0
    file "$(command -v "$java_cmd")" | grep -q x86_64 || return 0
    local version
    version="$("$java_cmd" -version 2>&1 | head -1)"
    if [ "$mode" = "--warn" ] || [ "${HYDRA_ALLOW_ROSETTA_JDK:-}" = "1" ]; then
        warn "x86_64 JDK detected on Apple Silicon. This runs under Rosetta 2 and will be ~20x slower than a native arm64 JDK."
        warn "Current JDK: $version"
        return 0
    fi
    echo "ERROR: x86_64 JDK detected on Apple Silicon (runs under Rosetta 2, ~20x slower than a native arm64 JDK)." >&2
    echo "  Current JDK: $version" >&2
    echo "  Point JAVA_HOME at a native arm64 JDK and retry, or set HYDRA_ALLOW_ROSETTA_JDK=1 to proceed anyway." >&2
    exit 1
}

# ---------------------------------------------------------------------------
# Portability helpers
# ---------------------------------------------------------------------------

# In-place sed that works on both BSD (macOS) and GNU (Linux) sed.
# Usage: sed_inplace 's/foo/bar/g' file1 file2 ...
sed_inplace() {
    local expr="$1"
    shift
    if sed --version >/dev/null 2>&1; then
        # GNU sed
        sed -i -e "$expr" "$@"
    else
        # BSD sed (macOS)
        sed -i '' -e "$expr" "$@"
    fi
}

# Run sed in-place across files matching a find pattern.
# Usage: sed_inplace_find <dir> <find-args...> -- <sed-expr>
# Example: sed_inplace_find mydir -name '*.scala' -- 's/foo/bar/g'
sed_inplace_find() {
    local dir="$1"
    shift
    local find_args=()
    while [ $# -gt 0 ] && [ "$1" != "--" ]; do
        find_args+=("$1")
        shift
    done
    shift  # consume --
    local expr="$1"
    find "$dir" "${find_args[@]}" -type f -print0 | while IFS= read -r -d '' f; do
        sed_inplace "$expr" "$f"
    done
}

# Portable millisecond timestamp.
# Prints an integer number of milliseconds since the epoch.
now_ms() {
    if date +%s%3N 2>/dev/null | grep -qv N; then
        # GNU date supports %3N
        date +%s%3N
    elif command -v gdate >/dev/null 2>&1; then
        # macOS with coreutils installed
        gdate +%s%3N
    else
        # Fallback: seconds * 1000 (loses sub-second precision)
        echo "$(($(date +%s) * 1000))"
    fi
}

# ---------------------------------------------------------------------------
# Coarse step caches
#
# Used by sync-haskell.sh to skip whole steps (verify-json-kernel,
# bootstrap-from-json, stack test) when their input set is byte-identical
# to the last successful run. Each step owns a cache file under
# .stack-work/ holding a single hash line; if the recomputed hash matches,
# the step is skipped.
#
# These caches are coarse: any input change rehashes everything and
# triggers a full re-run of the step. Per-input incremental caching is a
# follow-up (see #247-style work).
#
# Each call site computes its own hash via a `find ... | shasum` pipeline
# tailored to the inputs that step depends on (typically a name filter on
# .json or .hs files). The shared helpers below handle the read/compare/
# record bookkeeping; the input set and hash function stay per-step.
#
# Usage:
#   HASH=$(find ... | shasum ... | awk ...)
#   if step_cache_hit "$CACHE_FILE" "$HASH"; then
#       echo "  Inputs unchanged; skipping."
#   else
#       run_the_step
#       step_cache_record "$CACHE_FILE" "$HASH"
#   fi
# ---------------------------------------------------------------------------

# Return 0 if the cache file holds the given hash (cache hit, step can
# be skipped); 1 otherwise (cache miss, step must run).
#
# Usage:
#   if step_cache_hit "$CACHE_FILE" "$HASH"; then ... fi
step_cache_hit() {
    local cache_file="$1"
    local hash="$2"
    [ -n "$hash" ] || return 1
    [ -f "$cache_file" ] || return 1
    [ "$(cat "$cache_file")" = "$hash" ]
}

# Record a hash to the cache file. Call this after the step has succeeded.
#
# Usage:
#   step_cache_record "$CACHE_FILE" "$HASH"
step_cache_record() {
    local cache_file="$1"
    local hash="$2"
    mkdir -p "$(dirname "$cache_file")"
    echo "$hash" > "$cache_file"
}

# ---------------------------------------------------------------------------
# Stack/build helpers
# ---------------------------------------------------------------------------

# Standard GHC runtime flags for code-generation executables.
# Raises heap and allocation-area sizes to handle large module sets.
# -M5G caps the max heap so a runaway pass fails with a visible
# "heap overflow" instead of being SIGKILL'd silently by a 7 GB GH
# runner (three consecutive CI runs in 2026-05 hit silent cancels
# inside update-json-main's kernel validation with no error message).
RTS_FLAGS="+RTS -K256M -A32M -M5G -RTS"
