#!/bin/bash
#
# Shared utilities for translingual demo orchestrator scripts.
# Source this file from demo run.sh scripts:
#   source "$(dirname "$0")/../../bin/common.sh"

# Create a timestamped run directory: <base>/runs/run_YYYY-MM-DD_HHMMSS_mmm[_tag]
# Usage: create_run_dir <base_dir> [tag]
create_run_dir() {
    local base_dir="$1"
    local tag="${2:-}"
    local ts
    ts=$(python3 -c 'from datetime import datetime,timezone; t=datetime.now(timezone.utc); print(t.strftime("%Y-%m-%d_%H%M%S") + "_" + f"{t.microsecond//1000:03d}")')
    local dirname="run_${ts}"
    if [ -n "$tag" ]; then
        dirname="${dirname}_${tag}"
    fi
    local run_dir="$base_dir/runs/$dirname"
    mkdir -p "$run_dir"
    echo "$run_dir"
}
