#!/usr/bin/env bash
# Run benchmark tests for Hydra implementations and display the dashboard.
#
# Usage:
#   bin/run-benchmark-tests.sh                                          # Run default (Haskell + Java + Python)
#   bin/run-benchmark-tests.sh --hosts haskell,java                   # Run specific hosts
#   bin/run-benchmark-tests.sh --hosts all                            # Run all 7 implementations
#   bin/run-benchmark-tests.sh --hosts lisp                           # Run all four Lisp dialects
#   bin/run-benchmark-tests.sh dashboard                                # Just show the dashboard
#
# Hosts:
#   haskell, java, python, clojure, common-lisp, emacs-lisp, scheme
#   lisp  = clojure,common-lisp,emacs-lisp,scheme
#   all   = haskell,java,python,clojure,common-lisp,emacs-lisp,scheme
#
# Options:
#   --hosts H,...   Comma-separated list of hosts (default: haskell,java,python)
#   --repeat N        Run each language N times (default: 1)
#   --tag TAG         Append a human-readable tag to the run directory name

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
RUNS_DIR="$REPO_ROOT/benchmark/runs"

ALL_HOSTS="haskell,java,python,clojure,common-lisp,emacs-lisp,scheme"
LISP_HOSTS="clojure,common-lisp,emacs-lisp,scheme"

# Check for dashboard subcommand first
if [ "${1:-}" = "dashboard" ]; then
    shift
    echo "=== Benchmark Dashboard ==="
    python3 "$REPO_ROOT/bin/benchmark-dashboard.py" --dir "$RUNS_DIR" "$@"
    exit 0
fi

# Parse flags
REPEAT=1
TAG=""
HOSTS="haskell,java,python"
EXTRA_ARGS=()

while [[ $# -gt 0 ]]; do
    case "$1" in
        --repeat)
            REPEAT="$2"
            shift 2
            ;;
        --tag)
            TAG="$2"
            shift 2
            ;;
        --hosts)
            HOSTS="$2"
            shift 2
            ;;
        --help|-h)
            echo "Usage: $0 [OPTIONS]"
            echo "       $0 dashboard [dashboard options...]"
            echo ""
            echo "Run benchmark tests for Hydra implementations."
            echo ""
            echo "Options:"
            echo "  --hosts H,...   Comma-separated list of hosts (default: haskell,java,python)"
            echo "  --repeat N        Run each language N times (default: 1)"
            echo "  --tag TAG         Append a human-readable tag to the run directory name"
            echo "  --help            Show this help message"
            echo ""
            echo "Hosts:"
            echo "  haskell, java, python, clojure, common-lisp, emacs-lisp, scheme"
            echo "  lisp        All four Lisp dialects"
            echo "  all         All 7 implementations"
            exit 0
            ;;
        *)
            EXTRA_ARGS+=("$1")
            shift
            ;;
    esac
done

# Expand special host names
case "$HOSTS" in
    all)  HOSTS="$ALL_HOSTS" ;;
    lisp) HOSTS="$LISP_HOSTS" ;;
esac

# Parse and expand hosts (handle 'lisp' appearing within a comma list)
expanded=""
IFS=',' read -ra RAW_LIST <<< "$HOSTS"
for t in "${RAW_LIST[@]}"; do
    case "$t" in
        all)  expanded="${expanded:+$expanded,}$ALL_HOSTS" ;;
        lisp) expanded="${expanded:+$expanded,}$LISP_HOSTS" ;;
        haskell|java|python|clojure|common-lisp|emacs-lisp|scheme)
            expanded="${expanded:+$expanded,}$t" ;;
        *)
            echo "Error: Unknown host '$t'"
            echo "Valid hosts: haskell, java, python, clojure, common-lisp, emacs-lisp, scheme, lisp, all"
            exit 1
            ;;
    esac
done
IFS=',' read -ra HOST_LIST <<< "$expanded"

RUN_DIR="$RUNS_DIR/run_$(python3 -c 'from datetime import datetime,timezone; t=datetime.now(timezone.utc); print(t.strftime("%Y-%m-%d_%H%M%S") + "_" + f"{t.microsecond//1000:03d}")')"
if [ -n "$TAG" ]; then
    RUN_DIR="${RUN_DIR}_${TAG}"
fi

mkdir -p "$RUN_DIR"

# Warn if running an x86_64 JDK under Rosetta on Apple Silicon (causes ~20x slowdown)
if [ "$(uname -s)" = "Darwin" ] && [ "$(uname -m)" = "arm64" ]; then
    JAVA_CMD="${JAVA_HOME:+$JAVA_HOME/bin/}java"
    if command -v "$JAVA_CMD" > /dev/null 2>&1 && file "$(command -v "$JAVA_CMD")" | grep -q x86_64; then
        echo "WARNING: x86_64 JDK detected on Apple Silicon. This runs under Rosetta 2"
        echo "  and will be ~20x slower than a native arm64 JDK."
        echo "  Current JDK: $("$JAVA_CMD" -version 2>&1 | head -1)"
        echo ""
    fi
fi

run_lang() {
    local lang="$1"
    local i="$2"
    local suffix
    if [ "$REPEAT" -eq 1 ]; then
        suffix=""
    else
        suffix="_${i}"
    fi
    local outfile="$RUN_DIR/${lang}${suffix}.json"

    case "$lang" in
        haskell)
            echo "=== Running Haskell benchmark tests (run $i/$REPEAT) ==="
            cd "$REPO_ROOT/heads/haskell"
            HYDRA_BENCHMARK_OUTPUT="$outfile" \
                stack test
            ;;
        python)
            echo "=== Running Python benchmark tests (run $i/$REPEAT) ==="
            cd "$REPO_ROOT/packages/hydra-python"
            local py="${REPO_ROOT}/packages/hydra-python/.venv/bin/python"
            if [ ! -x "$py" ]; then
                py="python3"
            fi
            HYDRA_BENCHMARK_OUTPUT="$outfile" \
                "$py" -m pytest src/test/python/test_suite_runner.py
            ;;
        java)
            echo "=== Running Java benchmark tests (run $i/$REPEAT) ==="
            cd "$REPO_ROOT"
            HYDRA_BENCHMARK_OUTPUT="$outfile" \
                ./gradlew :hydra-java:test --rerun
            ;;
        clojure)
            echo "=== Running Clojure benchmark tests (run $i/$REPEAT) ==="
            cd "$REPO_ROOT/packages/hydra-lisp/hydra-clojure"
            HYDRA_BENCHMARK_OUTPUT="$outfile" \
                bash run-tests.sh
            ;;
        common-lisp)
            echo "=== Running Common Lisp benchmark tests (run $i/$REPEAT) ==="
            cd "$REPO_ROOT/packages/hydra-lisp/hydra-common-lisp"
            HYDRA_BENCHMARK_OUTPUT="$outfile" \
                bash run-tests.sh
            ;;
        emacs-lisp)
            echo "=== Running Emacs Lisp benchmark tests (run $i/$REPEAT) ==="
            cd "$REPO_ROOT/packages/hydra-lisp/hydra-emacs-lisp"
            HYDRA_BENCHMARK_OUTPUT="$outfile" \
                bash run-tests.sh
            ;;
        scheme)
            echo "=== Running Scheme benchmark tests (run $i/$REPEAT) ==="
            cd "$REPO_ROOT/packages/hydra-lisp/hydra-scheme"
            HYDRA_BENCHMARK_OUTPUT="$outfile" \
                bash run-tests.sh
            ;;
    esac
    echo "  -> $outfile"

    # Inject metadata into benchmark JSON if not already present (Lisp test runners don't include it)
    if [ -f "$outfile" ] && ! grep -q '"metadata"' "$outfile" 2>/dev/null; then
        local ts branch commit msg
        ts=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
        branch=$(cd "$REPO_ROOT" && git rev-parse --abbrev-ref HEAD 2>/dev/null || echo "unknown")
        commit=$(cd "$REPO_ROOT" && git rev-parse --short HEAD 2>/dev/null || echo "unknown")
        msg=$(cd "$REPO_ROOT" && git log -1 --format=%s 2>/dev/null || echo "")
        python3 -c "
import json, sys
with open('$outfile') as f: d = json.load(f)
d['metadata'] = {'timestamp': '$ts', 'language': '$lang', 'branch': '$branch', 'commit': '$commit', 'commitMessage': '''$msg'''}
# Rewrite with metadata first
out = {'metadata': d['metadata'], 'groups': d['groups'], 'summary': d['summary']}
with open('$outfile', 'w') as f: json.dump(out, f, indent=2)
"
    fi
    echo
}

run_all_repeats() {
    local lang="$1"
    for ((i=1; i<=REPEAT; i++)); do
        run_lang "$lang" "$i"
    done
}

echo "=== Benchmark Tests (hosts: ${HOST_LIST[*]}) ==="
echo ""

for host in "${HOST_LIST[@]}"; do
    run_all_repeats "$host"
done

echo "=== Benchmark Dashboard ==="
python3 "$REPO_ROOT/bin/benchmark-dashboard.py" --dir "$RUNS_DIR" "${EXTRA_ARGS[@]+"${EXTRA_ARGS[@]}"}"
