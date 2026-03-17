#!/bin/bash
#
# Orchestrator script for the GenPG translingual demo.
#
# Runs CSV-to-GraphSON property graph generation in each host language,
# capturing output and timing information. Compares results across hosts.
#
# Usage: ./run.sh [OPTIONS] [sales|health]
#
#   --hosts LANG,...     Run only specified hosts (default: haskell,java,python)
#   --tag TAG            Append a tag to the run directory name

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
HYDRA_EXT_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"
REPO_ROOT="$(cd "$HYDRA_EXT_ROOT/.." && pwd)"

source "$SCRIPT_DIR/../../bin/common.sh"

HOSTS="haskell,java,python"
TAG=""
DATASET=""

while [ $# -gt 0 ]; do
  case "$1" in
    --hosts) HOSTS="$2"; shift 2 ;;
    --hosts=*) HOSTS="${1#--hosts=}"; shift ;;
    --tag) TAG="$2"; shift 2 ;;
    --tag=*) TAG="${1#--tag=}"; shift ;;
    sales|health) DATASET="$1"; shift ;;
    *)
      echo "Unknown argument: $1" >&2
      echo "Usage: $0 [--hosts LANG,...] [--tag TAG] [sales|health]" >&2
      exit 1
      ;;
  esac
done

DATASET="${DATASET:-sales}"

# Capitalize first letter (portable)
DATASET_CAP="$(echo "$DATASET" | awk '{print toupper(substr($0,1,1)) substr($0,2)}')"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m'

header() { echo ""; echo -e "${BOLD}${CYAN}=== $1 ===${NC}"; }

IFS=',' read -ra ENABLED_HOSTS <<< "$HOSTS"

# All three drivers write to the same output path
OUTPUT_JSONL="$HYDRA_EXT_ROOT/demos/genpg/output/$DATASET.jsonl"

# Create run directory
RUN_DIR=$(create_run_dir /tmp/hydra-genpg "$TAG")

# ============================================================================
# Build
# ============================================================================

header "Building"
cd "$REPO_ROOT"
./gradlew :hydra-ext:compileJava --quiet 2>&1

# Build classpath from Gradle build output + cached dependencies
JAVA_DIRS="$REPO_ROOT/hydra-ext/build/classes/java/main:$REPO_ROOT/hydra-java/build/classes/java/main"
JAVA_DEPS="$(find "$HOME/.gradle/caches" -name 'commons-text-*.jar' -not -name '*sources*' 2>/dev/null | head -1)"
JAVA_DEPS="$JAVA_DEPS:$(find "$HOME/.gradle/caches" -name 'commons-csv-*.jar' -not -name '*sources*' 2>/dev/null | head -1)"
JAVA_DEPS="$JAVA_DEPS:$(find "$HOME/.gradle/caches" -name 'commons-lang3-*.jar' -not -name '*sources*' 2>/dev/null | head -1)"
JAVA_CP="$JAVA_DIRS:$JAVA_DEPS"

echo "Dataset: $DATASET"
echo "Input:   $HYDRA_EXT_ROOT/demos/genpg/data/sources/$DATASET/"

# ============================================================================
# Run hosts
# ============================================================================

run_host() {
  local host="$1"
  mkdir -p "$RUN_DIR/$host"
  echo "skipped" > "$RUN_DIR/$host/_status"

  header "Running $host driver"
  cd "$HYDRA_EXT_ROOT"
  rm -f "$OUTPUT_JSONL"

  case "$host" in
    haskell)
      local ghci_cmd="import Hydra.Ext.Demos.GenPG.Demo
import qualified System.CPUTime as T
import qualified System.IO as IO
t0 <- T.getCPUTime
generate${DATASET_CAP}GraphSON
t1 <- T.getCPUTime
let ms = fromIntegral (t1 - t0) / 1e9 :: Double
IO.hPutStrLn IO.stderr (Prelude.concat [\"HYDRA_TIME_MS=\", show ms])
:quit"
      if echo "$ghci_cmd" | stack ghci hydra-ext:lib --ghci-options='-v0' \
        > "$RUN_DIR/$host/stdout.txt" 2> "$RUN_DIR/$host/stderr.txt"; then
        echo "ok" > "$RUN_DIR/$host/_status"
      else
        echo "error" > "$RUN_DIR/$host/_status"
      fi
      ;;
    java)
      if java -cp "$JAVA_CP" hydra.demos.genpg.Demo "$DATASET" \
        > "$RUN_DIR/$host/stdout.txt" 2> "$RUN_DIR/$host/stderr.txt"; then
        echo "ok" > "$RUN_DIR/$host/_status"
      else
        echo "error" > "$RUN_DIR/$host/_status"
      fi
      ;;
    python)
      local driver="$HYDRA_EXT_ROOT/src/main/python/hydra/demos/genpg/demo.py"
      if [ ! -f "$driver" ]; then
        echo -e "  ${YELLOW}SKIPPED${NC} (driver not found)"; return
      fi
      if python3 "$driver" "$DATASET" \
        > "$RUN_DIR/$host/stdout.txt" 2> "$RUN_DIR/$host/stderr.txt"; then
        echo "ok" > "$RUN_DIR/$host/_status"
      else
        echo "error" > "$RUN_DIR/$host/_status"
      fi
      ;;
  esac

  if [ -f "$OUTPUT_JSONL" ]; then
    cp "$OUTPUT_JSONL" "$RUN_DIR/$host/$DATASET.jsonl"
    echo "  Output: $(wc -l < "$RUN_DIR/$host/$DATASET.jsonl" | tr -d ' ') lines"
  else
    echo "  No output file produced"
    echo "error" > "$RUN_DIR/$host/_status"
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
    f1="$RUN_DIR/$h1/$DATASET.jsonl"
    f2="$RUN_DIR/$h2/$DATASET.jsonl"

    if [ -f "$f1" ] && [ -f "$f2" ]; then
      if diff -q "$f1" "$f2" > /dev/null 2>&1; then
        echo -e "  ${GREEN}$h1 == $h2${NC}"
      else
        l1=$(wc -l < "$f1" | tr -d ' ')
        l2=$(wc -l < "$f2" | tr -d ' ')
        echo -e "  ${RED}$h1 != $h2${NC} ($l1 vs $l2 lines)"
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
  time_ms=$(grep 'HYDRA_TIME_MS=' "$RUN_DIR/$host/stderr.txt" 2>/dev/null | sed 's/.*HYDRA_TIME_MS=//' || echo "—")
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
