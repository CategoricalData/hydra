#!/usr/bin/env bash
set -euo pipefail
#
# Orchestrator script for the automatic differentiation demo.
#
# Differentiates mathematical functions symbolically, evaluates derivatives
# numerically, and verifies correctness via gradient checking.
#
# Usage: ./run.sh [OPTIONS]
#
#   --tag TAG    Append a tag to the run directory name
#   --help       Show this help message

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
HYDRA_EXT_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"
REPO_ROOT="$(cd "$HYDRA_EXT_ROOT/.." && pwd)"

source "$SCRIPT_DIR/../../bin/common.sh"

TAG=""

while [ $# -gt 0 ]; do
  case "$1" in
    --tag) TAG="$2"; shift 2 ;;
    --tag=*) TAG="${1#--tag=}"; shift ;;
    --help|-h)
      sed -n '4,13p' "$0" | sed 's/^# //; s/^#//'
      exit 0
      ;;
    *)
      die "Unknown argument: $1 (try --help)"
      ;;
  esac
done

# Create run directory
RUN_DIR=$(create_run_dir /tmp/hydra-grad "$TAG")

demo_header "Automatic Differentiation Demo"

OUT_DIR="$RUN_DIR/output"
mkdir -p "$OUT_DIR"

DRIVER="$HYDRA_EXT_ROOT/src/main/haskell/Hydra/Ext/Demos/Grad/Demo.hs"

if [ ! -f "$DRIVER" ]; then
  echo -e "  ${RED}ERROR${NC}: Driver not found at $DRIVER"
  exit 1
fi

cd "$HYDRA_EXT_ROOT"
echo ":load $DRIVER
:set args $OUT_DIR
main
:quit" | stack ghci hydra-ext:lib --no-load --ghci-options='-v0 +RTS -K256M -A32M -RTS' 2> "$RUN_DIR/stderr.txt" \
  | grep -v '^Ok' | grep -v '^Loaded' | grep -v '^\*' | grep -v '^$' \
  | tee "$RUN_DIR/results.txt"

# ============================================================================
# Summary
# ============================================================================

demo_header "Run complete"
echo "  Output directory: $OUT_DIR"
echo "  Full results:     $RUN_DIR/results.txt"

if [ -f "$RUN_DIR/stderr.txt" ]; then
  TIME=$(extract_time "$RUN_DIR/stderr.txt")
  if [ -n "$TIME" ]; then
    echo "  Time: ${TIME} ms"
  fi
fi
