#!/usr/bin/env bash
set -euo pipefail
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
#   --help               Show this help message

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"
HYDRA_EXT_ROOT="$REPO_ROOT/packages/hydra-ext"

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
    --help|-h)
      sed -n '5,13p' "$0" | sed 's/^# //; s/^#//'
      exit 0
      ;;
    sales|health) DATASET="$1"; shift ;;
    *)
      die "Unknown argument: $1 (try --help)"
      ;;
  esac
done

DATASET="${DATASET:-sales}"

# Capitalize first letter (portable)
DATASET_CAP="$(echo "$DATASET" | awk '{print toupper(substr($0,1,1)) substr($0,2)}')"

IFS=',' read -ra ENABLED_HOSTS <<< "$HOSTS"

# All three drivers write to the same output path
OUTPUT_JSONL="$REPO_ROOT/demos/genpg/output/$DATASET.jsonl"

# Create run directory
RUN_DIR=$(create_run_dir /tmp/hydra-genpg "$TAG")

# ============================================================================
# Build
# ============================================================================

demo_header "Building"
cd "$REPO_ROOT"
./gradlew :hydra-ext:compileJava --quiet 2>&1

JAVA_CP="$(build_java_classpath commons-text commons-csv commons-lang3)"

echo "Dataset: $DATASET"
echo "Input:   $REPO_ROOT/demos/genpg/data/sources/$DATASET/"

# ============================================================================
# Run hosts
# ============================================================================

run_host() {
  local host="$1"
  mkdir -p "$RUN_DIR/$host"
  echo "skipped" > "$RUN_DIR/$host/_status"

  demo_header "Running $host driver"
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

demo_header "Comparison"

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

demo_header "Summary"

printf "  %-12s %-14s %-12s\n" "Host" "Status" "Time (ms)"
printf "  %-12s %-14s %-12s\n" "----" "------" "---------"

for host in "${ENABLED_HOSTS[@]}"; do
  status=$(cat "$RUN_DIR/$host/_status" 2>/dev/null || echo "skipped")
  time_ms=$(extract_time "$RUN_DIR/$host/stderr.txt")
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
