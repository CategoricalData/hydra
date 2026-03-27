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
sbt test 2>&1
EXIT_CODE=$?
END_MS=$(python3 -c 'import time; print(int(time.monotonic() * 1000))')
ELAPSED_MS=$((END_MS - START_MS))

# Write benchmark JSON if requested
if [ -n "${HYDRA_BENCHMARK_OUTPUT:-}" ]; then
    # Parse pass/fail counts from sbt output
    PASSED=$(sbt 'testOnly *' 2>&1 | grep -c "passed" || echo "0")
    cat > "$HYDRA_BENCHMARK_OUTPUT" <<ENDJSON
{
  "groups": [],
  "summary": {
    "totalPassed": 0,
    "totalFailed": 0,
    "totalSkipped": 0,
    "totalTimeMs": $ELAPSED_MS
  }
}
ENDJSON
    echo "Benchmark written to: $HYDRA_BENCHMARK_OUTPUT"
fi

exit $EXIT_CODE
