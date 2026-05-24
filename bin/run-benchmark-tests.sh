#!/usr/bin/env bash
# Run benchmark tests for Hydra implementations and display the dashboard.
#
# Usage:
#   bin/run-benchmark-tests.sh                          # Default (haskell, java, python)
#   bin/run-benchmark-tests.sh --hosts haskell,java     # Specific hosts
#   bin/run-benchmark-tests.sh --hosts all              # All 7 implementations
#   bin/run-benchmark-tests.sh --hosts lisp             # All four Lisp dialects
#   bin/run-benchmark-tests.sh dashboard [opts]         # Just show the dashboard
#                                                       # (forwards opts to bin/benchmark-dashboard.py)
#   bin/run-benchmark-tests.sh --help
#
# Options:
#   --hosts H,...   Comma-separated list of hosts (default: haskell,java,python)
#   --repeat N      Run each language N times (default: 1)
#   --tag TAG       Append a human-readable tag to the run directory name
#   --help          Show this help.
#
# Hosts:
#   haskell, java, python, clojure, common-lisp, emacs-lisp, scheme
#   lisp  = clojure,common-lisp,emacs-lisp,scheme
#   all   = haskell,java,python,clojure,common-lisp,emacs-lisp,scheme

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
RUNS_DIR="$REPO_ROOT/benchmark/runs"

source "$REPO_ROOT/bin/lib/common.sh"

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

while [ $# -gt 0 ]; do
    case "$1" in
        --repeat) REPEAT="$2"; shift 2 ;;
        --repeat=*) REPEAT="${1#--repeat=}"; shift ;;
        --tag) TAG="$2"; shift 2 ;;
        --tag=*) TAG="${1#--tag=}"; shift ;;
        --hosts) HOSTS="$2"; shift 2 ;;
        --hosts=*) HOSTS="${1#--hosts=}"; shift ;;
        --help|-h)
            sed -n '2,/^$/p' "$0" | sed 's/^# \{0,1\}//'
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
        haskell|java|python|typescript|clojure|common-lisp|emacs-lisp|scheme)
            expanded="${expanded:+$expanded,}$t" ;;
        *)
            echo "Error: Unknown host '$t'"
            echo "Valid hosts: haskell, java, python, typescript, clojure, common-lisp, emacs-lisp, scheme, lisp, all"
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

check_native_jdk

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
            cd "$REPO_ROOT/heads/python"
            local py="${REPO_ROOT}/heads/python/.venv/bin/python"
            if [ ! -x "$py" ]; then
                py="python3"
            fi
            HYDRA_BENCHMARK_OUTPUT="$outfile" \
                "$py" -m pytest src/test/python/test_suite_runner.py
            ;;
        java)
            echo "=== Running Java benchmark tests (run $i/$REPEAT) ==="
            cd "$REPO_ROOT/heads/java"
            HYDRA_BENCHMARK_OUTPUT="$outfile" \
                ./gradlew :hydra-java:test --rerun
            ;;
        typescript)
            echo "=== Running TypeScript benchmark tests (run $i/$REPEAT) ==="
            cd "$REPO_ROOT/heads/typescript"
            local ts_json
            ts_json=$(mktemp).json
            local ts_start ts_end
            ts_start=$(python3 -c 'import time; print(int(time.monotonic() * 1000))')
            set +e
            HYDRA_JSON_DIR="$REPO_ROOT/dist/json/hydra-kernel/src/main/json" \
                npx vitest run --reporter=json --outputFile="$ts_json"
            local ts_exit=$?
            set -e
            ts_end=$(python3 -c 'import time; print(int(time.monotonic() * 1000))')
            local ts_elapsed=$((ts_end - ts_start))
            # Convert vitest JSON to Hydra benchmark JSON (groups omitted; summary suffices).
            local p f s
            if [ -f "$ts_json" ]; then
                p=$(python3 -c "import json; d=json.load(open('$ts_json')); print(d.get('numPassedTests', 0))" 2>/dev/null || echo 0)
                f=$(python3 -c "import json; d=json.load(open('$ts_json')); print(d.get('numFailedTests', 0))" 2>/dev/null || echo 0)
                s=$(python3 -c "import json; d=json.load(open('$ts_json')); print(d.get('numPendingTests', 0))" 2>/dev/null || echo 0)
            else
                p=0; f=0; s=0
            fi
            cat > "$outfile" <<TSEOF
{
  "groups": [],
  "summary": {
    "totalPassed": $p,
    "totalFailed": $f,
    "totalSkipped": $s,
    "totalTimeMs": $ts_elapsed
  }
}
TSEOF
            rm -f "$ts_json"
            ;;
        clojure)
            echo "=== Running Clojure benchmark tests (run $i/$REPEAT) ==="
            HYDRA_BENCHMARK_OUTPUT="$outfile" \
                bash "$REPO_ROOT/packages/hydra-lisp/bin/run-tests.sh" clojure
            ;;
        common-lisp)
            echo "=== Running Common Lisp benchmark tests (run $i/$REPEAT) ==="
            HYDRA_BENCHMARK_OUTPUT="$outfile" \
                bash "$REPO_ROOT/packages/hydra-lisp/bin/run-tests.sh" common-lisp
            ;;
        emacs-lisp)
            echo "=== Running Emacs Lisp benchmark tests (run $i/$REPEAT) ==="
            HYDRA_BENCHMARK_OUTPUT="$outfile" \
                bash "$REPO_ROOT/packages/hydra-lisp/bin/run-tests.sh" emacs-lisp
            ;;
        scheme)
            echo "=== Running Scheme benchmark tests (run $i/$REPEAT) ==="
            HYDRA_BENCHMARK_OUTPUT="$outfile" \
                bash "$REPO_ROOT/packages/hydra-lisp/bin/run-tests.sh" scheme
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
