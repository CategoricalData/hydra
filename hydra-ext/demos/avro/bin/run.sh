#!/bin/bash
#
# Runner for the Avro bidirectional coder demo.
#
# Demonstrates the full bidirectional Avro pipeline:
#   1. Forward:    Avro schema + JSON data -> Hydra -> RDF (N-Triples)
#   2. Reverse:    Hydra types -> Avro schema + JSON data
#   3. Round-trip: Avro schema -> Hydra -> Avro schema (structural comparison)
#   4. Codec:      Schema string encode/decode round-trip
#
# Usage: ./run.sh [--demo N] [--tag TAG]
#
#   --demo N     Run only demo N (1-4, or "all" for all demos)
#   --tag TAG    Append a tag to the run directory name

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
HYDRA_EXT_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"
REPO_ROOT="$(cd "$HYDRA_EXT_ROOT/.." && pwd)"

source "$SCRIPT_DIR/../../bin/common.sh"

DEMO="all"
TAG=""

while [ $# -gt 0 ]; do
  case "$1" in
    --demo) DEMO="$2"; shift 2 ;;
    --demo=*) DEMO="${1#--demo=}"; shift ;;
    --tag) TAG="$2"; shift 2 ;;
    --tag=*) TAG="${1#--tag=}"; shift ;;
    *)
      echo "Unknown argument: $1" >&2
      echo "Usage: $0 [--demo N] [--tag TAG]" >&2
      exit 1
      ;;
  esac
done

# Colors
BOLD='\033[1m'
CYAN='\033[0;36m'
GREEN='\033[0;32m'
NC='\033[0m'

header() { echo ""; echo -e "${BOLD}${CYAN}=== $1 ===${NC}"; }

RUN_DIR=$(create_run_dir /tmp/hydra-avro-demo "$TAG")

header "Avro Bidirectional Coder Demo"
echo "Run directory: $RUN_DIR"

cd "$HYDRA_EXT_ROOT"

case "$DEMO" in
  1) GHCI_FN="runForwardDemo" ;;
  2) GHCI_FN="runReverseDemo" ;;
  3) GHCI_FN="runRoundTripDemo" ;;
  4) GHCI_FN="runSchemaCodecDemo" ;;
  5) GHCI_FN="runPropertyGraphDemo" ;;
  all) GHCI_FN="runAllDemos" ;;
  *)
    echo "Invalid demo number: $DEMO (expected 1-4 or 'all')" >&2
    exit 1
    ;;
esac

GHCI_CMD="import Hydra.Ext.Demos.AvroBicoder
import qualified System.CPUTime as T
t0 <- T.getCPUTime
$GHCI_FN
t1 <- T.getCPUTime
let ms = fromIntegral (t1 - t0) / 1e9 :: Double
putStrLn (\"\\nElapsed: \" ++ show (round ms :: Int) ++ \" ms\")
:quit"

header "Running demo: $DEMO"

if echo "$GHCI_CMD" | stack ghci hydra-ext:lib --ghci-options='-v0' \
  > "$RUN_DIR/stdout.txt" 2> "$RUN_DIR/stderr.txt"; then
  cat "$RUN_DIR/stdout.txt"
  echo -e "\n${GREEN}Demo completed successfully.${NC}"
else
  echo "Demo failed. stderr:"
  cat "$RUN_DIR/stderr.txt"
  exit 1
fi
