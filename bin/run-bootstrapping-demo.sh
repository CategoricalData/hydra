#!/usr/bin/env bash
# Run the bootstrapping demo and display the dashboard.
#
# Usage:
#   bin/run-bootstrapping-demo.sh                         # Run the 4 Java/Python paths (generate + test)
#   bin/run-bootstrapping-demo.sh generate [options]      # Generation phase only
#   bin/run-bootstrapping-demo.sh test [options]          # Testing phase only
#   bin/run-bootstrapping-demo.sh dashboard [options]     # Just show the dashboard
#   bin/run-bootstrapping-demo.sh --help                  # Show this help
#
# Options (passed through to bootstrap-all.sh):
#   --tag TAG               Append a human-readable tag to the run directory name
#   --runs DIR              Override runs directory (default: bootstrap/runs)
#   --output DIR            Override output directory for generated code
#   --hosts LANG,...        Run only specified host languages
#   --targets LANG,...      Generate only specified target languages
#   --types-only            Only generate type-defining modules
#   --kernel-only           Only generate kernel modules
#   --repeat N              Run tests N times (test/all modes)
#   --run RUN               Use a specific run (test/dashboard modes)
#   --paths p1,p2,...       Test only specific paths (test mode)
#   --clean                 Force a fresh wipe + recopy of each target dir
#                           (default is incremental; use --clean for cold-start
#                           timing measurements).

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
REPO_ROOT="$( cd "$SCRIPT_DIR/.." && pwd )"
RUNS_DIR="$REPO_ROOT/bootstrap/runs"
DEMO_DIR="$REPO_ROOT/demos/bootstrapping/bin"

if [ "${1:-}" = "--help" ] || [ "${1:-}" = "-h" ]; then
    sed -n '2,/^$/p' "$0" | sed 's/^# \{0,1\}//'
    exit 0
fi

# --- #730 build-slot guard: serialize shared-~/.stack GHC builds fleet-wide. ---
# Hold the slot only for the stack calls this demo makes DIRECTLY: a haskell host or target
# (invoke-haskell-host.sh, test-haskell-target.sh, bootstrap-all.sh's haskell `stack test`). Lisp
# hosts seed their coders via the Java driver (seed-lisp-coders.sh). Everything else reaches GHC only via
# bin/sync.sh, which takes the slot itself unless its #773/#797 freshness-gated fast path applies,
# so the demo must not second-guess that decision here. Defaults mirror bootstrap-all.sh
# (java,python); `all`/`others` scopes are treated as needing the slot.
_demo_needs_slot() {
  local hosts="java,python" targets="java,python" prev="" a
  for a in "$@"; do
    case "$prev" in --hosts) hosts="$a" ;; --targets) targets="$a" ;; esac
    case "$a" in --hosts=*) hosts="${a#--hosts=}" ;; --targets=*) targets="${a#--targets=}" ;; esac
    prev="$a"
  done
  case ",$hosts,$targets," in *,all,*|*,others,*|*,haskell,*) return 0 ;; esac
  return 1
}
if [ -z "${HYDRA_STACK_SLOT_HELD:-}" ] && [ "${1:-}" != "dashboard" ] && _demo_needs_slot "$@"; then
  exec "$REPO_ROOT/bin/with-stack-slot.sh" --label "run-bootstrapping-demo.sh $*" -- "$0" "$@"
fi

# Check for dashboard-only mode
if [ "${1:-}" = "dashboard" ]; then
    shift
    echo "Bootstrapping Dashboard"
    python3 "$REPO_ROOT/bin/bootstrapping-dashboard.py" --dir "$RUNS_DIR" "$@"
    exit 0
fi

# Scope the scratch directory to this worktree so parallel sessions in other
# worktrees don't write into the same /tmp subtree and clobber each other's
# builds. Only injected when the caller didn't pass --output explicitly.
# bootstrap-all.sh expects an optional subcommand (generate|test|display) as
# its first positional argument, so we preserve it at the front and splice
# --output in right after.
has_output_flag=false
for arg in "$@"; do
    case "$arg" in
        --output|--output=*) has_output_flag=true; break ;;
    esac
done

subcommand=""
case "${1:-}" in
    generate|test|display)
        subcommand="$1"
        shift
        ;;
esac

if $has_output_flag; then
    "$DEMO_DIR/bootstrap-all.sh" ${subcommand:+"$subcommand"} "$@"
else
    scope="$(basename "$REPO_ROOT")"
    "$DEMO_DIR/bootstrap-all.sh" ${subcommand:+"$subcommand"} --output "/tmp/hydra-bootstrapping-demo-$scope" "$@"
fi

# --- #769: persist the exercised cells into the tracked cross-generation matrix doc. ---
# The demo's per-cell results (bootstrap/runs/run_*/) are gitignored and otherwise discarded.
# Merge whatever this run just exercised into docs/bootstrap-matrix.md, leaving all other cells
# at their last-known state. Skip on dashboard-only/display modes (no new run dir), and never
# let a doc-update hiccup fail the bootstrap run itself (informational, best-effort).
if [ "$subcommand" != "display" ] && [ -f "$REPO_ROOT/bin/bootstrap-matrix.py" ]; then
    latest_run="$(ls -dt "$RUNS_DIR"/run_* 2>/dev/null | head -1 || true)"
    if [ -n "$latest_run" ] && [ -d "$latest_run" ]; then
        echo ""
        echo "Persisting cross-generation matrix (docs/bootstrap-matrix.md)..."
        python3 "$REPO_ROOT/bin/bootstrap-matrix.py" update --run "$latest_run" \
            --doc "$REPO_ROOT/docs/bootstrap-matrix.md" || \
            echo "  (matrix doc update skipped — non-fatal)"
    fi
fi
