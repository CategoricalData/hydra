#!/bin/bash
#
# Orchestrator script for the ValidatePG translingual demo.
#
# Generates example data (schema + graphs) using the Java DSL, then runs
# validation in each host language, capturing output and timing information.
# Compares results across hosts.
#
# Usage: ./run.sh [OPTIONS]
#
#   --hosts LANG,...     Run only specified hosts (default: haskell,java,python)
#   --tag TAG            Append a tag to the run directory name
#   --data-dir <path>   Use existing data directory instead of generating fresh data.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
HYDRA_EXT_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"
REPO_ROOT="$(cd "$HYDRA_EXT_ROOT/.." && pwd)"

source "$SCRIPT_DIR/../../bin/common.sh"

HOSTS="haskell,java,python"
TAG=""
DATA_DIR=""

while [ $# -gt 0 ]; do
  case "$1" in
    --hosts) HOSTS="$2"; shift 2 ;;
    --hosts=*) HOSTS="${1#--hosts=}"; shift ;;
    --tag) TAG="$2"; shift 2 ;;
    --tag=*) TAG="${1#--tag=}"; shift ;;
    --data-dir) DATA_DIR="$2"; shift 2 ;;
    *)
      echo "Unknown argument: $1" >&2
      echo "Usage: $0 [--hosts LANG,...] [--tag TAG] [--data-dir <path>]" >&2
      exit 1
      ;;
  esac
done

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m'

header() { echo ""; echo -e "${BOLD}${CYAN}=== $1 ===${NC}"; }

extract_time() {
  grep 'HYDRA_TIME_MS=' "$1" 2>/dev/null | sed 's/.*HYDRA_TIME_MS=//' || echo "?"
}

IFS=',' read -ra ENABLED_HOSTS <<< "$HOSTS"

# Create run directory
RUN_DIR=$(create_run_dir /tmp/hydra-validatepg "$TAG")

# ============================================================================
# Build
# ============================================================================

header "Building Java"
cd "$REPO_ROOT"
./gradlew :hydra-ext:compileJava :hydra-java:compileJava --quiet 2>&1

JAVA_CP="$REPO_ROOT/hydra-ext/build/classes/java/main:$REPO_ROOT/hydra-java/build/classes/java/main"

# ============================================================================
# Generate data
# ============================================================================

if [ -z "$DATA_DIR" ]; then
  DATA_DIR="$RUN_DIR/data"
  header "Generating data to $DATA_DIR"
  mkdir -p "$DATA_DIR"
  java -cp "$JAVA_CP" hydra.demos.validatepg.GenerateData "$DATA_DIR"
else
  header "Using existing data from $DATA_DIR"
fi

echo ""
echo "Data files:"
for f in "$DATA_DIR"/*.json; do [ -f "$f" ] && echo "  $f"; done

# ============================================================================
# Run hosts
# ============================================================================

run_host() {
  local host="$1"
  mkdir -p "$RUN_DIR/$host"
  echo "skipped" > "$RUN_DIR/$host/_status"
  echo "—" > "$RUN_DIR/$host/_time"

  header "Running $host driver"

  case "$host" in
    java)
      if java -cp "$JAVA_CP" hydra.demos.validatepg.ValidateDemo "$DATA_DIR" \
        > "$RUN_DIR/$host/results.txt" 2> "$RUN_DIR/$host/stderr.txt"; then
        echo "ok" > "$RUN_DIR/$host/_status"
      else
        echo "error" > "$RUN_DIR/$host/_status"
      fi
      ;;
    haskell)
      local driver="$HYDRA_EXT_ROOT/src/main/haskell/Hydra/Ext/Demos/ValidatePg/Demo.hs"
      if [ ! -f "$driver" ]; then
        echo -e "  ${YELLOW}SKIPPED${NC} (driver not found)"; return
      fi
      cd "$HYDRA_EXT_ROOT"
      if echo ":load $driver
:set args $DATA_DIR
main
:quit" | stack ghci hydra-ext:lib --no-load --ghci-options='-v0' 2> "$RUN_DIR/$host/stderr.txt" \
        | grep -v '^Ok' | grep -v '^Loaded' | grep -v '^\*' | grep -v '^$' \
        > "$RUN_DIR/$host/results.txt"; then
        echo "ok" > "$RUN_DIR/$host/_status"
      else
        echo "error" > "$RUN_DIR/$host/_status"
      fi
      ;;
    python)
      local driver="$HYDRA_EXT_ROOT/src/main/python/hydra/demos/validatepg/demo.py"
      if [ ! -f "$driver" ]; then
        echo -e "  ${YELLOW}SKIPPED${NC} (driver not found)"; return
      fi
      cd "$HYDRA_EXT_ROOT"
      if python3 "$driver" "$DATA_DIR" \
        > "$RUN_DIR/$host/results.txt" 2> "$RUN_DIR/$host/stderr.txt"; then
        echo "ok" > "$RUN_DIR/$host/_status"
      else
        echo "error" > "$RUN_DIR/$host/_status"
      fi
      ;;
  esac

  if [ -f "$RUN_DIR/$host/stderr.txt" ]; then
    extract_time "$RUN_DIR/$host/stderr.txt" > "$RUN_DIR/$host/_time"
  fi

  if [ -f "$RUN_DIR/$host/results.txt" ]; then
    echo "  Output:"
    sed 's/^/    /' "$RUN_DIR/$host/results.txt"
  fi
}

for host in "${ENABLED_HOSTS[@]}"; do
  run_host "$host"
done

# ============================================================================
# Compare
# ============================================================================

header "Comparison"

MATCH=true
NUM_HOSTS=${#ENABLED_HOSTS[@]}

i=0
while [ $i -lt $NUM_HOSTS ]; do
  j=$((i + 1))
  while [ $j -lt $NUM_HOSTS ]; do
    h1="${ENABLED_HOSTS[$i]}"
    h2="${ENABLED_HOSTS[$j]}"
    f1="$RUN_DIR/$h1/results.txt"
    f2="$RUN_DIR/$h2/results.txt"

    if [ -s "$f1" ] && [ -s "$f2" ]; then
      if diff -q "$f1" "$f2" > /dev/null 2>&1; then
        echo -e "  ${GREEN}$h1 == $h2${NC}"
      else
        echo -e "  ${RED}$h1 != $h2${NC}"
        diff "$f1" "$f2" | sed 's/^/    /' || true
        MATCH=false
      fi
    fi
    j=$((j + 1))
  done
  i=$((i + 1))
done

# ============================================================================
# Summary
# ============================================================================

header "Summary"

format_status() {
  case "$1" in
    ok)      printf "${GREEN}OK${NC}";;
    error)   printf "${RED}FAIL${NC}";;
    skipped) printf "${YELLOW}SKIP${NC}";;
  esac
}

printf "  %-12s %-14s %-12s\n" "Host" "Status" "Time (ms)"
printf "  %-12s %-14s %-12s\n" "----" "------" "---------"

for host in "${ENABLED_HOSTS[@]}"; do
  status=$(cat "$RUN_DIR/$host/_status" 2>/dev/null || echo "skipped")
  time_ms=$(cat "$RUN_DIR/$host/_time" 2>/dev/null || echo "—")
  printf "  %-12s " "$host"
  format_status "$status"
  printf "       %-12s\n" "$time_ms"
done

echo ""
if [ "$MATCH" = true ]; then
  echo -e "${GREEN}All outputs match.${NC}"
else
  echo -e "${RED}Output mismatch detected.${NC}"
fi

echo ""
echo "Run directory: $RUN_DIR"
