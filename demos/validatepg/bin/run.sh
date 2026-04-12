#!/usr/bin/env bash
set -euo pipefail
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
#   --data-dir <path>    Use existing data directory instead of generating fresh data
#   --help               Show this help message

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"
HYDRA_EXT_ROOT="$REPO_ROOT/packages/hydra-ext"

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
    --help|-h)
      sed -n '4,15p' "$0" | sed 's/^# //; s/^#//'
      exit 0
      ;;
    *)
      die "Unknown argument: $1 (try --help)"
      ;;
  esac
done

IFS=',' read -ra ENABLED_HOSTS <<< "$HOSTS"

# Create run directory
RUN_DIR=$(create_run_dir /tmp/hydra-validatepg "$TAG")

# ============================================================================
# Build
# ============================================================================

demo_header "Building Java"
cd "$REPO_ROOT"
./gradlew :hydra-ext:compileJava :hydra-java:compileJava --quiet 2>&1

JAVA_CP="$REPO_ROOT/packages/hydra-ext/build/classes/java/main:$REPO_ROOT/packages/hydra-java/build/classes/java/main"

# ============================================================================
# Generate data
# ============================================================================

if [ -z "$DATA_DIR" ]; then
  DATA_DIR="$RUN_DIR/data"
  demo_header "Generating data to $DATA_DIR"
  mkdir -p "$DATA_DIR"
  java -cp "$JAVA_CP" hydra.demos.validatepg.GenerateData "$DATA_DIR"
else
  demo_header "Using existing data from $DATA_DIR"
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

  demo_header "Running $host driver"

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

demo_header "Comparison"

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

demo_header "Summary"

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
