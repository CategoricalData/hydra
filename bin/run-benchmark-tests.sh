#!/usr/bin/env bash
# Run benchmark tests for all implementations and display the dashboard.
#
# Usage:
#   bin/run-benchmark-tests.sh              # Run all (Haskell + Python + Java)
#   bin/run-benchmark-tests.sh haskell      # Run Haskell only
#   bin/run-benchmark-tests.sh python       # Run Python only
#   bin/run-benchmark-tests.sh java         # Run Java only
#   bin/run-benchmark-tests.sh dashboard    # Just show the dashboard
#
# Options:
#   --repeat N    Run each language N times (default: 1)

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
RUNS_DIR="$REPO_ROOT/benchmark/runs"

# Parse --repeat flag
REPEAT=1
ARGS=()
while [[ $# -gt 0 ]]; do
    case "$1" in
        --repeat)
            REPEAT="$2"
            shift 2
            ;;
        *)
            ARGS+=("$1")
            shift
            ;;
    esac
done
set -- "${ARGS[@]+"${ARGS[@]}"}"

RUN_DIR="$RUNS_DIR/run_$(python3 -c 'from datetime import datetime,timezone; t=datetime.now(timezone.utc); print(t.strftime("%Y-%m-%d_%H%M%S") + "_" + f"{t.microsecond//1000:03d}")')"

mkdir -p "$RUN_DIR"

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
            cd "$REPO_ROOT/hydra-haskell"
            HYDRA_BENCHMARK_OUTPUT="$outfile" \
                stack test
            ;;
        python)
            echo "=== Running Python benchmark tests (run $i/$REPEAT) ==="
            cd "$REPO_ROOT/hydra-python"
            HYDRA_BENCHMARK_OUTPUT="$outfile" \
                .venv/bin/python -m pytest src/test/python/test_suite_runner.py
            ;;
        java)
            echo "=== Running Java benchmark tests (run $i/$REPEAT) ==="
            cd "$REPO_ROOT"
            HYDRA_BENCHMARK_OUTPUT="$outfile" \
                ./gradlew :hydra-java:test --rerun
            ;;
    esac
    echo "  -> $outfile"
    echo
}

run_all_repeats() {
    local lang="$1"
    for ((i=1; i<=REPEAT; i++)); do
        run_lang "$lang" "$i"
    done
}

run_dashboard() {
    echo "=== Benchmark Dashboard ==="
    python3 "$REPO_ROOT/bin/benchmark-dashboard.py" --dir "$RUNS_DIR" "$@"
}

target="${1:-all}"
shift || true

case "$target" in
    haskell)
        run_all_repeats haskell
        run_dashboard "$@"
        ;;
    python)
        run_all_repeats python
        run_dashboard "$@"
        ;;
    java)
        run_all_repeats java
        run_dashboard "$@"
        ;;
    dashboard)
        run_dashboard "$@"
        ;;
    all)
        run_all_repeats haskell
        run_all_repeats python
        run_all_repeats java
        run_dashboard "$@"
        ;;
    *)
        echo "Usage: $0 [all|haskell|python|java|dashboard] [--repeat N] [dashboard options...]"
        exit 1
        ;;
esac
