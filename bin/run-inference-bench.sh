#!/usr/bin/env bash
# Cross-host inference benchmark — top-level dispatcher.
#
# Runs synthetic workloads (one or more "series") across each requested host,
# saving per-(host,series) results under benchmark/inference-runs/run_<TS>[_<tag>]/
# and printing a comparison dashboard. Parallels bin/run-benchmark-tests.sh
# (kernel-tests benchmark) in CLI shape, run-directory layout, and dashboard
# styling; see also bin/inference-bench-dashboard.py.
#
# Usage:
#   bin/run-inference-bench.sh                          # Default (haskell, java, python; linearChain)
#   bin/run-inference-bench.sh --hosts all              # All four hosts (adds python-pypy)
#   bin/run-inference-bench.sh --series all             # All three series
#   bin/run-inference-bench.sh --tag baseline           # Tag the run directory
#   bin/run-inference-bench.sh dashboard [opts]         # Just show the dashboard
#                                                       # (forwards opts to bin/inference-bench-dashboard.py)
#   bin/run-inference-bench.sh --help
#
# Options:
#   --hosts H,...   Comma-separated list of hosts (default: haskell,java,python)
#                   Special: 'all' expands to haskell,java,python,python-pypy,common-lisp,emacs-lisp
#   --series S,...  Comma-separated series names (default: linearChain)
#                   Special: 'all' expands to linearChain,polymorphicChain,fanOut
#   --sizes N,...   Comma-separated sizes (default: 0,10,25,50,100)
#                   n=0 measures per-call setup; the dashboard subtracts it.
#   --tag TAG       Append a human-readable tag to the run directory name
#   --help          Show this help.
#
# Hosts:
#   haskell       — stack exec bench-inference (uses in-memory mainModules)
#   java          — heads/java/bin/inference-bench.sh (loads kernel JSON)
#   python        — uv run heads/python/bin/inference-bench.py (CPython)
#   python-pypy   — pypy3 heads/python/bin/inference-bench.py (PyPy)
#   common-lisp   — heads/lisp/common-lisp/bin/inference-bench.sh (SBCL)
#   emacs-lisp    — heads/lisp/emacs-lisp/bin/inference-bench.sh
#
# Series (each is a Hydra namespace under hydra.bench.*):
#   linearChain         — depth-N chain of monomorphic walkers
#   polymorphicChain    — depth-N chain with forall-typed signatures
#   fanOut              — branchy DAG (more realistic codegen shape)
#
# Output:
#   - benchmark/inference-runs/run_<TS>[_<tag>]/<host>_<series>.json (one per pair)
#   - Comparison dashboard printed on stdout

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
RUNS_DIR="$REPO_ROOT/benchmark/inference-runs"

source "$REPO_ROOT/bin/lib/common.sh"

ALL_HOSTS="haskell,java,python,python-pypy,common-lisp,emacs-lisp"
DEFAULT_HOSTS="haskell,java,python"
ALL_SERIES="linearChain,polymorphicChain,fanOut"
DEFAULT_SERIES="linearChain"
DEFAULT_SIZES="0,10,25,50,100"

# Dashboard-only subcommand: forward to the dashboard script and exit.
if [ "${1:-}" = "dashboard" ]; then
    shift
    echo "=== Inference-Bench Dashboard ==="
    python3 "$REPO_ROOT/bin/inference-bench-dashboard.py" --dir "$RUNS_DIR" "$@"
    exit 0
fi

# Parse flags.
TAG=""
HOSTS="$DEFAULT_HOSTS"
SERIES="$DEFAULT_SERIES"
SIZES="$DEFAULT_SIZES"
EXTRA_ARGS=()

while [ $# -gt 0 ]; do
    case "$1" in
        --tag) TAG="$2"; shift 2 ;;
        --tag=*) TAG="${1#--tag=}"; shift ;;
        --hosts) HOSTS="$2"; shift 2 ;;
        --hosts=*) HOSTS="${1#--hosts=}"; shift ;;
        --series) SERIES="$2"; shift 2 ;;
        --series=*) SERIES="${1#--series=}"; shift ;;
        --sizes) SIZES="$2"; shift 2 ;;
        --sizes=*) SIZES="${1#--sizes=}"; shift ;;
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

# Expand special host names.
case "$HOSTS" in
    all) HOSTS="$ALL_HOSTS" ;;
esac
case "$SERIES" in
    all) SERIES="$ALL_SERIES" ;;
esac

# Parse hosts (allow 'all' inside a comma list, like run-benchmark-tests.sh).
expanded=""
IFS=',' read -ra RAW_LIST <<< "$HOSTS"
for t in "${RAW_LIST[@]}"; do
    case "$t" in
        all) expanded="${expanded:+$expanded,}$ALL_HOSTS" ;;
        haskell|java|python|python-pypy|common-lisp|emacs-lisp)
            expanded="${expanded:+$expanded,}$t" ;;
        *)
            echo "Error: Unknown host '$t'"
            echo "Valid hosts: haskell, java, python, python-pypy, common-lisp, emacs-lisp, all"
            exit 1
            ;;
    esac
done
IFS=',' read -ra HOST_LIST <<< "$expanded"

# Parse series.
expanded_series=""
IFS=',' read -ra RAW_SERIES <<< "$SERIES"
for s in "${RAW_SERIES[@]}"; do
    case "$s" in
        all) expanded_series="${expanded_series:+$expanded_series,}$ALL_SERIES" ;;
        linearChain|polymorphicChain|fanOut)
            expanded_series="${expanded_series:+$expanded_series,}$s" ;;
        *)
            echo "Error: Unknown series '$s'"
            echo "Valid series: linearChain, polymorphicChain, fanOut, all"
            exit 1
            ;;
    esac
done
IFS=',' read -ra SERIES_LIST <<< "$expanded_series"

# Ensure hydra-bench is regenerated for the requested hosts. The default sync
# (bin/sync.sh) does NOT include the bench package — these workloads are
# deliberately stress-shaped and only relevant for this benchmark — so we
# always invoke bin/sync-bench.sh up front. It's idempotent and fast when
# everything is already current.
echo "" >&2
echo "===== Ensuring hydra-bench is current for: $HOSTS =====" >&2
# Map host names to bench-capable host names (drop python-pypy → python).
bench_hosts_set=()
IFS=',' read -ra _hosts_arr <<< "$HOSTS"
for h in "${_hosts_arr[@]}"; do
    case "$h" in
        python-pypy) bench_hosts_set+=("python") ;;
        *) bench_hosts_set+=("$h") ;;
    esac
done
# Deduplicate.
bench_hosts_dedup=$(printf "%s\n" "${bench_hosts_set[@]}" | awk '!seen[$0]++' | paste -sd, -)
"$REPO_ROOT/bin/sync-bench.sh" --hosts "$bench_hosts_dedup"

# Run directory mirrors bin/run-benchmark-tests.sh layout for symmetry.
RUN_DIR="$RUNS_DIR/run_$(python3 -c 'from datetime import datetime,timezone; t=datetime.now(timezone.utc); print(t.strftime("%Y-%m-%d_%H%M%S") + "_" + f"{t.microsecond//1000:03d}")')"
if [ -n "$TAG" ]; then
    RUN_DIR="${RUN_DIR}_${TAG}"
fi
mkdir -p "$RUN_DIR"

check_native_jdk

# Result files we'll feed to the dashboard.
ALL_RESULT_FILES=()

run_host_series() {
    local host="$1"
    local series="$2"
    local namespace="hydra.bench.${series}"
    local outfile="$RUN_DIR/${host}_${series}.json"
    echo "" >&2
    echo "===== $host / $series =====" >&2
    case "$host" in
        haskell)
            ( cd "$REPO_ROOT/heads/haskell" \
                && stack exec bench-inference -- --sizes "$SIZES" --namespace "$namespace" --out "$outfile" )
            ;;
        java)
            "$REPO_ROOT/heads/java/bin/inference-bench.sh" \
                --sizes "$SIZES" --namespace "$namespace" --out "$outfile"
            ;;
        python)
            ( cd "$REPO_ROOT/heads/python" \
                && uv run python bin/inference-bench.py \
                     --sizes "$SIZES" --namespace "$namespace" --out "$outfile" > /dev/null )
            ;;
        python-pypy)
            ( cd "$REPO_ROOT/heads/python" \
                && pypy3 bin/inference-bench.py \
                     --sizes "$SIZES" --namespace "$namespace" --out "$outfile" \
                     --host-tag python-pypy > /dev/null )
            ;;
        common-lisp)
            "$REPO_ROOT/heads/lisp/common-lisp/bin/inference-bench.sh" \
                --sizes "$SIZES" --namespace "$namespace" --out "$outfile"
            ;;
        emacs-lisp)
            "$REPO_ROOT/heads/lisp/emacs-lisp/bin/inference-bench.sh" \
                --sizes "$SIZES" --namespace "$namespace" --out "$outfile"
            ;;
        *)
            echo "Unknown host: $host" >&2
            return 2
            ;;
    esac

    # Inject metadata into the JSON if not already present.
    if [ -f "$outfile" ] && ! grep -q '"_metadata"' "$outfile" 2>/dev/null; then
        local ts branch commit msg
        ts=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
        branch=$(cd "$REPO_ROOT" && git rev-parse --abbrev-ref HEAD 2>/dev/null || echo "unknown")
        commit=$(cd "$REPO_ROOT" && git rev-parse --short HEAD 2>/dev/null || echo "unknown")
        msg=$(cd "$REPO_ROOT" && git log -1 --format=%s 2>/dev/null || echo "")
        python3 - "$outfile" "$ts" "$host" "$series" "$branch" "$commit" "$msg" <<'PY'
import json, sys
out, ts, host, series, branch, commit, msg = sys.argv[1:]
with open(out) as f: d = json.load(f)
md = {'timestamp': ts, 'host': host, 'series': series,
      'branch': branch, 'commit': commit, 'commitMessage': msg}
# Embed metadata while preserving the array shape: wrap as a dict.
wrapped = {'_metadata': md, 'results': d}
with open(out, 'w') as f: json.dump(wrapped, f, indent=2)
PY
    fi
    ALL_RESULT_FILES+=("$outfile")
}

echo "=== Inference Benchmark (hosts: ${HOST_LIST[*]}; series: ${SERIES_LIST[*]}) ==="
echo "Run dir: $RUN_DIR"
echo ""

for s in "${SERIES_LIST[@]}"; do
    for h in "${HOST_LIST[@]}"; do
        run_host_series "$h" "$s"
    done
done

echo "" >&2
echo "===== Dashboard =====" >&2
python3 "$REPO_ROOT/bin/inference-bench-dashboard.py" "${ALL_RESULT_FILES[@]}" "${EXTRA_ARGS[@]+"${EXTRA_ARGS[@]}"}"
