#!/bin/bash
# Run Scala tests on generated code.
#
# Usage: ./test-scala-target.sh <output-dir>

set -e

OUTPUT_DIR="$1"
if [ -z "$OUTPUT_DIR" ]; then
    echo "Usage: $0 <output-dir>"
    exit 1
fi

echo "Running Scala tests..."
cd "$OUTPUT_DIR"

START_MS=$(python3 -c 'import time; print(int(time.monotonic() * 1000))')
SBT_LOG=$(mktemp)
set +e
sbt test 2>&1 | tee "$SBT_LOG"
EXIT_CODE=${PIPESTATUS[0]}
set -e
END_MS=$(python3 -c 'import time; print(int(time.monotonic() * 1000))')
ELAPSED_MS=$((END_MS - START_MS))

# Write benchmark JSON if requested
if [ -n "${HYDRA_BENCHMARK_OUTPUT:-}" ]; then
    # Parse pass/fail counts from sbt output. ScalaTest's summary line looks like:
    #   [info] Tests: succeeded 2565, failed 0, canceled 0, ignored 0, pending 0
    SUMMARY=$(grep -E '^\[info\] Tests: succeeded [0-9]+' "$SBT_LOG" | tail -1)
    PASSED=$(echo "$SUMMARY" | sed -nE 's/.*succeeded ([0-9]+).*/\1/p')
    FAILED=$(echo "$SUMMARY" | sed -nE 's/.*failed ([0-9]+).*/\1/p')
    SKIPPED=$(echo "$SUMMARY" | sed -nE 's/.*ignored ([0-9]+).*/\1/p')
    PASSED=${PASSED:-0}
    FAILED=${FAILED:-0}
    SKIPPED=${SKIPPED:-0}
    cat > "$HYDRA_BENCHMARK_OUTPUT" <<ENDJSON
{
  "groups": [],
  "summary": {
    "totalPassed": $PASSED,
    "totalFailed": $FAILED,
    "totalSkipped": $SKIPPED,
    "totalTimeMs": $ELAPSED_MS
  }
}
ENDJSON
    echo "Benchmark written to: $HYDRA_BENCHMARK_OUTPUT"
fi

rm -f "$SBT_LOG"
exit $EXIT_CODE
