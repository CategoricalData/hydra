# Shared utilities for translingual demo orchestrator scripts.
#
# Source this file from demo run.sh scripts:
#   SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
#   source "$SCRIPT_DIR/../../bin/common.sh"
#
# Sources the project-level bin/lib/common.sh and adds demo-specific helpers.

# Locate the repo root and source the project-level common library.
_DEMO_COMMON_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
_DEMO_REPO_ROOT="$(cd "$_DEMO_COMMON_DIR/../../.." && pwd)"
source "$_DEMO_REPO_ROOT/bin/lib/common.sh"

# ---------------------------------------------------------------------------
# ANSI colors (standard palette)
# ---------------------------------------------------------------------------
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m'

# ---------------------------------------------------------------------------
# Demo section header — printed between phases of a demo
# Usage: demo_header "Building"
# ---------------------------------------------------------------------------
demo_header() {
    echo ""
    echo -e "${BOLD}${CYAN}=== $1 ===${NC}"
}

# ---------------------------------------------------------------------------
# Format a run status as colored text.
# Usage: format_status "ok" | format_status "error" | format_status "skipped"
# ---------------------------------------------------------------------------
format_status() {
    case "$1" in
        ok)      printf "${GREEN}OK${NC}";;
        error)   printf "${RED}FAIL${NC}";;
        skipped) printf "${YELLOW}SKIP${NC}";;
        *)       printf "%s" "$1";;
    esac
}

# ---------------------------------------------------------------------------
# Extract HYDRA_TIME_MS=<value> from a stderr file.
# Prints "—" if not found.
# Usage: extract_time path/to/stderr.txt
# ---------------------------------------------------------------------------
extract_time() {
    local file="$1"
    if [ -f "$file" ]; then
        grep 'HYDRA_TIME_MS=' "$file" 2>/dev/null | sed 's/.*HYDRA_TIME_MS=//' | head -1 || echo "—"
    else
        echo "—"
    fi
}

# ---------------------------------------------------------------------------
# Build Java classpath from Gradle build output plus cached dependencies.
# Each additional argument is the basename pattern of a required jar (e.g.
# 'commons-text' matches commons-text-*.jar).
# Usage: build_java_classpath commons-text commons-csv commons-lang3
#   -> echoes the classpath string
# ---------------------------------------------------------------------------
build_java_classpath() {
    local cp="$_DEMO_REPO_ROOT/hydra-ext/build/classes/java/main:$_DEMO_REPO_ROOT/hydra-java/build/classes/java/main"
    local name
    for name in "$@"; do
        local jar
        jar="$(find "$HOME/.gradle/caches" -name "${name}-*.jar" -not -name '*sources*' 2>/dev/null | head -1)"
        if [ -n "$jar" ]; then
            cp="$cp:$jar"
        fi
    done
    echo "$cp"
}

# ---------------------------------------------------------------------------
# Create a timestamped run directory: <base>/runs/run_YYYY-MM-DD_HHMMSS_mmm[_tag]
# Usage: create_run_dir <base_dir> [tag]
# ---------------------------------------------------------------------------
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
