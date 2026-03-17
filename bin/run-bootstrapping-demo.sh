#!/usr/bin/env bash
# Run the bootstrapping demo and display the dashboard.
#
# Usage:
#   bin/run-bootstrapping-demo.sh                         # Run all 9 paths (generate + test)
#   bin/run-bootstrapping-demo.sh generate [options]      # Generation phase only
#   bin/run-bootstrapping-demo.sh test [options]          # Testing phase only
#   bin/run-bootstrapping-demo.sh dashboard [options]     # Just show the dashboard
#
# Options (passed through to bootstrap-all.sh):
#   --tag TAG               Append a human-readable tag to the run directory name
#   --runs DIR              Override runs directory (default: bootstrap/runs)
#   --output DIR            Override output directory for generated code (default: /tmp/hydra-bootstrapping-demo)
#   --hosts=LANG,...        Run only specified host languages
#   --targets=LANG,...      Generate only specified target languages
#   --types-only            Only generate type-defining modules
#   --kernel-only           Only generate kernel modules
#   --repeat N              Run tests N times (test/all modes)
#   --run RUN               Use a specific run (test/dashboard modes)
#   --paths p1,p2,...       Test only specific paths (test mode)

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
RUNS_DIR="$REPO_ROOT/bootstrap/runs"
DEMO_DIR="$REPO_ROOT/hydra-ext/demos/bootstrapping/bin"

# Check for dashboard-only mode
if [ "${1:-}" = "dashboard" ]; then
    shift
    echo "=== Bootstrapping Dashboard ==="
    python3 "$REPO_ROOT/bin/bootstrapping-dashboard.py" --dir "$RUNS_DIR" "$@"
    exit 0
fi

# Pass all arguments through to bootstrap-all.sh
"$DEMO_DIR/bootstrap-all.sh" "$@"
