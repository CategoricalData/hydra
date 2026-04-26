#!/usr/bin/env bash
set -euo pipefail
#
# Orchestrator script for the GenPG RDF/SHACL demo.
#
# Converts CSV data to a property graph, then outputs SHACL shapes and RDF
# instance data in N-Triples format. Validates conforming data against shapes
# and verifies that intentionally invalid data is rejected.
#
# Usage: ./run-rdf.sh [OPTIONS] [sales|health]
#
#   --hosts LANG,...     Run only specified hosts (default: haskell,java,python)
#   --tag TAG            Append a tag to the run directory name
#   --skip-validate      Skip pyshacl validation step
#   --help               Show this help message

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"
HYDRA_HASKELL_HEAD="$REPO_ROOT/heads/haskell"

source "$SCRIPT_DIR/../../bin/common.sh"

HOSTS="haskell,java,python"
TAG=""
DATASET=""
SKIP_VALIDATE=false

while [ $# -gt 0 ]; do
  case "$1" in
    --hosts) HOSTS="$2"; shift 2 ;;
    --hosts=*) HOSTS="${1#--hosts=}"; shift ;;
    --tag) TAG="$2"; shift 2 ;;
    --tag=*) TAG="${1#--tag=}"; shift ;;
    --skip-validate) SKIP_VALIDATE=true; shift ;;
    --help|-h)
      sed -n '4,15p' "$0" | sed 's/^# //; s/^#//'
      exit 0
      ;;
    sales|health) DATASET="$1"; shift ;;
    *)
      die "Unknown argument: $1 (try --help)"
      ;;
  esac
done

DATASET="${DATASET:-sales}"
DATASET_CAP="$(echo "$DATASET" | awk '{print toupper(substr($0,1,1)) substr($0,2)}')"

IFS=',' read -ra ENABLED_HOSTS <<< "$HOSTS"

# Output paths
OUTPUT_DIR="$REPO_ROOT/demos/genpg/output/$DATASET"

# Create run directory
RUN_DIR=$(create_run_dir /tmp/hydra-genpg-rdf "$TAG")

# ============================================================================
# Build
# ============================================================================

demo_header "Building"
cd "$REPO_ROOT"
./gradlew :hydra-java:compileJava --quiet 2>&1

JAVA_CP="$(build_java_classpath commons-text commons-csv commons-lang3)"

echo "Dataset: $DATASET"
echo "Input:   $REPO_ROOT/demos/genpg/data/sources/$DATASET/"
echo "Output:  ${DATASET}-shapes.nt, ${DATASET}-data.nt, ${DATASET}-invalid.nt"

# ============================================================================
# Run hosts
# ============================================================================

run_host() {
  local host="$1"
  mkdir -p "$RUN_DIR/$host"
  echo "skipped" > "$RUN_DIR/$host/_status"

  demo_header "Running $host driver"
  cd "$HYDRA_HASKELL_HEAD"

  local out_prefix="$RUN_DIR/$host/$DATASET"

  case "$host" in
    haskell)
      echo "  Loading Haskell modules (this may take a minute)..."
      local ghci_cmd=":l Hydra.Ext.Demos.GenPG.Rdf
import qualified System.CPUTime as T
import qualified System.IO as IO
t0 <- T.getCPUTime
generateRdf \"$REPO_ROOT/demos/genpg/data/sources/$DATASET\" ${DATASET}TableSchemas ${DATASET}Graph ${DATASET}GraphSchema \"$out_prefix\"
t1 <- T.getCPUTime
IO.hPutStrLn IO.stderr (\"HYDRA_TIME_MS=\" Prelude.++ show (fromIntegral (t1 - t0) / 1e9 :: Double))
:quit"
      if echo "$ghci_cmd" | stack ghci --ghci-options='+RTS -K256M -A32M -RTS -v0' hydra:lib \
        > "$RUN_DIR/$host/stdout.txt" 2> "$RUN_DIR/$host/stderr.txt"; then
        echo "ok" > "$RUN_DIR/$host/_status"
      else
        echo "error" > "$RUN_DIR/$host/_status"
      fi
      ;;
    java)
      if java -cp "$JAVA_CP" hydra.demos.genpg.RdfDemo "$DATASET" "$out_prefix" \
        > "$RUN_DIR/$host/stdout.txt" 2> "$RUN_DIR/$host/stderr.txt"; then
        echo "ok" > "$RUN_DIR/$host/_status"
      else
        echo "error" > "$RUN_DIR/$host/_status"
      fi
      ;;
    python)
      local driver="$REPO_ROOT/demos/src/main/python/hydra/demos/genpg/rdf.py"
      if [ ! -f "$driver" ]; then
        echo -e "  ${YELLOW}SKIPPED${NC} (driver not found)"; return
      fi
      if python3 "$driver" "$DATASET" "$out_prefix" \
        > "$RUN_DIR/$host/stdout.txt" 2> "$RUN_DIR/$host/stderr.txt"; then
        echo "ok" > "$RUN_DIR/$host/_status"
      else
        echo "error" > "$RUN_DIR/$host/_status"
      fi
      ;;
  esac

  if [ -f "$out_prefix-shapes.nt" ]; then
    local nshapes=$(wc -l < "$out_prefix-shapes.nt" | tr -d ' ')
    local ndata=$(wc -l < "$out_prefix-data.nt" 2>/dev/null | tr -d ' ')
    echo "  Shapes: $nshapes triples, Data: ${ndata:-0} triples"
  else
    echo "  No output files produced"
    echo "error" > "$RUN_DIR/$host/_status"
  fi

  if [ -f "$RUN_DIR/$host/stdout.txt" ]; then
    sed 's/^/    /' "$RUN_DIR/$host/stdout.txt"
  fi
}

for host in "${ENABLED_HOSTS[@]}"; do
  run_host "$host"
done

# ============================================================================
# Validate with pyshacl
# ============================================================================

if [ "$SKIP_VALIDATE" = false ]; then
  demo_header "SHACL Validation (pyshacl)"

  # Find pyshacl
  PYSHACL=""
  if command -v pyshacl &> /dev/null; then
    PYSHACL="pyshacl"
  elif [ -x "$REPO_ROOT/.venv/bin/pyshacl" ]; then
    PYSHACL="$REPO_ROOT/.venv/bin/pyshacl"
  fi

  if [ -z "$PYSHACL" ]; then
    echo -e "  ${YELLOW}SKIPPED${NC} (pyshacl not found; install with: pip install pyshacl)"
  else
    # Use output from the first successful host
    SHAPES_FILE=""
    for host in "${ENABLED_HOSTS[@]}"; do
      local_prefix="$RUN_DIR/$host/$DATASET"
      if [ -f "${local_prefix}-shapes.nt" ] && [ -f "${local_prefix}-data.nt" ]; then
        SHAPES_FILE="${local_prefix}-shapes.nt"
        DATA_FILE="${local_prefix}-data.nt"
        INVALID_FILE="${local_prefix}-invalid.nt"
        echo "  Using output from: $host"
        break
      fi
    done

    if [ -z "$SHAPES_FILE" ]; then
      echo -e "  ${YELLOW}SKIPPED${NC} (no output files found)"
    else
      # Validate conforming data
      echo -e "  ${BOLD}Conforming data:${NC}"
      if "$PYSHACL" -s "$SHAPES_FILE" -sf nt -df nt "$DATA_FILE" > "$RUN_DIR/validation_conforming.txt" 2>&1; then
        echo -e "    ${GREEN}CONFORMS${NC} (as expected)"
      else
        echo -e "    ${RED}NON-CONFORMANT${NC} (unexpected!)"
        sed 's/^/      /' "$RUN_DIR/validation_conforming.txt" | head -20
      fi

      # Validate non-conforming data
      if [ -f "$INVALID_FILE" ]; then
        echo -e "  ${BOLD}Non-conforming data:${NC}"
        if "$PYSHACL" -s "$SHAPES_FILE" -sf nt -df nt "$INVALID_FILE" > "$RUN_DIR/validation_invalid.txt" 2>&1; then
          echo -e "    ${YELLOW}CONFORMS${NC} (unexpected — violations not detected)"
        else
          echo -e "    ${RED}NON-CONFORMANT${NC} (as expected)"
        fi
        echo "    Violations:"
        grep 'Constraint Violation\|Message:' "$RUN_DIR/validation_invalid.txt" | sed 's/^/      /'
      fi
    fi
  fi
fi

# ============================================================================
# Compare hosts
# ============================================================================

if [ ${#ENABLED_HOSTS[@]} -gt 1 ]; then
  demo_header "Comparison"

  for suffix in shapes data invalid; do
    i=0
    while [ $i -lt ${#ENABLED_HOSTS[@]} ]; do
      j=$((i + 1))
      while [ $j -lt ${#ENABLED_HOSTS[@]} ]; do
        h1="${ENABLED_HOSTS[$i]}"
        h2="${ENABLED_HOSTS[$j]}"
        f1="$RUN_DIR/$h1/$DATASET-${suffix}.nt"
        f2="$RUN_DIR/$h2/$DATASET-${suffix}.nt"
        if [ -f "$f1" ] && [ -f "$f2" ]; then
          if diff -q <(sort "$f1") <(sort "$f2") > /dev/null 2>&1; then
            echo -e "  ${GREEN}$h1 == $h2 ($suffix)${NC}"
          else
            echo -e "  ${RED}$h1 != $h2 ($suffix)${NC}"
          fi
        fi
        j=$((j + 1))
      done
      i=$((i + 1))
    done
  done
fi

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
echo "Run directory: $RUN_DIR"
