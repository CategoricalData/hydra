#!/usr/bin/env bash
set -euo pipefail
#
# Orchestrator script for the SHACL RDF demo.
#
# Generates SHACL shapes from Hydra kernel types and encodes kernel modules
# as conforming RDF data. Then validates the data against the shapes using
# pyshacl.
#
# Usage: ./run.sh [OPTIONS]
#
#   --hosts LANG,...     Run only specified hosts (default: haskell)
#   --tag TAG            Append a tag to the run directory name
#   --skip-validate      Skip pyshacl validation step
#   --help               Show this help message

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"
HYDRA_HASKELL_HEAD="$REPO_ROOT/heads/haskell"

source "$SCRIPT_DIR/../../bin/common.sh"

HOSTS="haskell"
TAG=""
SKIP_VALIDATE=false
JSON_DIR="$REPO_ROOT/dist/json/hydra-kernel/src/main/json"

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
    *)
      die "Unknown argument: $1 (try --help)"
      ;;
  esac
done

IFS=',' read -ra ENABLED_HOSTS <<< "$HOSTS"

# Create run directory
RUN_DIR=$(create_run_dir /tmp/hydra-shacl "$TAG")

# ============================================================================
# Run hosts
# ============================================================================

run_host() {
  local host="$1"
  mkdir -p "$RUN_DIR/$host"
  echo "skipped" > "$RUN_DIR/$host/_status"
  echo "—" > "$RUN_DIR/$host/_time"

  demo_header "Running $host driver"

  local out_dir="$RUN_DIR/$host/output"
  mkdir -p "$out_dir"

  case "$host" in
    haskell)
      local driver="$REPO_ROOT/demos/src/main/haskell/Hydra/Ext/Demos/Shacl/Demo.hs"
      if [ ! -f "$driver" ]; then
        echo -e "  ${YELLOW}SKIPPED${NC} (driver not found)"; return
      fi
      cd "$HYDRA_HASKELL_HEAD"
      if echo ":load $driver
:set args $JSON_DIR $out_dir
main
:quit" | stack ghci hydra:lib --no-load --ghci-options='-v0 +RTS -K256M -A32M -RTS' 2> "$RUN_DIR/$host/stderr.txt" \
        | grep -v '^Ok' | grep -v '^Loaded' | grep -v '^\*' | grep -v '^$' \
        > "$RUN_DIR/$host/results.txt"; then
        echo "ok" > "$RUN_DIR/$host/_status"
      else
        echo "error" > "$RUN_DIR/$host/_status"
      fi
      ;;
    *)
      echo -e "  ${YELLOW}SKIPPED${NC} (not yet implemented)"
      return
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
# Validate with pyshacl
# ============================================================================

if [ "$SKIP_VALIDATE" = false ]; then
  demo_header "SHACL Validation (pyshacl)"

  # Find the first host with output
  SHAPES_FILE=""
  DATA_FILE=""
  for host in "${ENABLED_HOSTS[@]}"; do
    local_out="$RUN_DIR/$host/output"
    if [ -f "$local_out/shapes.nt" ] && [ -f "$local_out/data.nt" ]; then
      SHAPES_FILE="$local_out/shapes.nt"
      DATA_FILE="$local_out/data.nt"
      break
    fi
  done

  if [ -z "$SHAPES_FILE" ]; then
    echo -e "  ${YELLOW}SKIPPED${NC} (no output files found)"
  else
    # Find pyshacl: check PATH, then check for a venv
    PYSHACL=""
    if command -v pyshacl &> /dev/null; then
      PYSHACL="pyshacl"
    elif [ -x "$REPO_ROOT/.venv/bin/pyshacl" ]; then
      PYSHACL="$REPO_ROOT/.venv/bin/pyshacl"
    fi

    if [ -z "$PYSHACL" ]; then
      echo -e "  ${YELLOW}SKIPPED${NC} (pyshacl not found; install with: pip install pyshacl)"
    else
      # Validate conforming data (expect: conforms)
      echo -e "  ${BOLD}Conforming data:${NC}"
      echo "    Shapes: $SHAPES_FILE"
      echo "    Data:   $DATA_FILE"
      if "$PYSHACL" -s "$SHAPES_FILE" -sf nt -df nt "$DATA_FILE" > "$RUN_DIR/validation_conforming.txt" 2>&1; then
        echo -e "    ${GREEN}CONFORMS${NC} (as expected)"
      else
        echo -e "    ${RED}NON-CONFORMANT${NC} (unexpected! exit code $?)"
      fi
      echo ""

      # Validate non-conforming data (expect: non-conformant)
      local_out="$RUN_DIR/${ENABLED_HOSTS[0]}/output"
      INVALID_FILE="$local_out/invalid.nt"
      if [ -f "$INVALID_FILE" ]; then
        echo -e "  ${BOLD}Non-conforming data:${NC}"
        echo "    Shapes: $SHAPES_FILE"
        echo "    Data:   $INVALID_FILE"
        if "$PYSHACL" -s "$SHAPES_FILE" -sf nt -df nt "$INVALID_FILE" > "$RUN_DIR/validation_invalid.txt" 2>&1; then
          echo -e "    ${YELLOW}CONFORMS${NC} (unexpected — violations not detected)"
        else
          echo -e "    ${RED}NON-CONFORMANT${NC} (as expected)"
        fi
        echo ""
        echo "    Validation report:"
        sed 's/^/      /' "$RUN_DIR/validation_invalid.txt"
      fi
    fi
  fi
fi

# ============================================================================
# Compare hosts (if multiple)
# ============================================================================

if [ ${#ENABLED_HOSTS[@]} -gt 1 ]; then
  demo_header "Comparison"

  MATCH=true
  NUM_HOSTS=${#ENABLED_HOSTS[@]}

  i=0
  while [ $i -lt $NUM_HOSTS ]; do
    j=$((i + 1))
    while [ $j -lt $NUM_HOSTS ]; do
      h1="${ENABLED_HOSTS[$i]}"
      h2="${ENABLED_HOSTS[$j]}"
      f1="$RUN_DIR/$h1/output/shapes.nt"
      f2="$RUN_DIR/$h2/output/shapes.nt"

      if [ -s "$f1" ] && [ -s "$f2" ]; then
        if diff -q "$f1" "$f2" > /dev/null 2>&1; then
          echo -e "  ${GREEN}$h1 == $h2 (shapes)${NC}"
        else
          echo -e "  ${RED}$h1 != $h2 (shapes)${NC}"
          MATCH=false
        fi
      fi

      f1="$RUN_DIR/$h1/output/data.nt"
      f2="$RUN_DIR/$h2/output/data.nt"

      if [ -s "$f1" ] && [ -s "$f2" ]; then
        if diff -q "$f1" "$f2" > /dev/null 2>&1; then
          echo -e "  ${GREEN}$h1 == $h2 (data)${NC}"
        else
          echo -e "  ${RED}$h1 != $h2 (data)${NC}"
          MATCH=false
        fi
      fi

      j=$((j + 1))
    done
    i=$((i + 1))
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
  time_ms=$(cat "$RUN_DIR/$host/_time" 2>/dev/null || echo "—")
  printf "  %-12s " "$host"
  format_status "$status"
  printf "       %-12s\n" "$time_ms"
done

echo ""
echo "Run directory: $RUN_DIR"
