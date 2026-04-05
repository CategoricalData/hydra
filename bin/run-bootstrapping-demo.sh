#!/usr/bin/env bash
set -euo pipefail

# Run the bootstrapping demo and display the dashboard.
#
# Usage:
#   bin/run-bootstrapping-demo.sh                         # Run all 9 paths (generate + test)
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

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
REPO_ROOT="$( cd "$SCRIPT_DIR/.." && pwd )"
RUNS_DIR="$REPO_ROOT/bootstrap/runs"
DEMO_DIR="$REPO_ROOT/hydra-ext/demos/bootstrapping/bin"

if [ "${1:-}" = "--help" ] || [ "${1:-}" = "-h" ]; then
    sed -n '4,21p' "$0" | sed 's/^# //; s/^#//'
    exit 0
fi

# Check for dashboard-only mode
if [ "${1:-}" = "dashboard" ]; then
    shift
    echo "Bootstrapping Dashboard"
    python3 "$REPO_ROOT/bin/bootstrapping-dashboard.py" --dir "$RUNS_DIR" "$@"
    exit 0
fi

# Pass all arguments through to bootstrap-all.sh
"$DEMO_DIR/bootstrap-all.sh" "$@"
