#!/bin/bash
set -eo pipefail

# Run Hydra Emacs Lisp test suite.
#
# Prerequisites:
#   - Emacs 28+ must be installed
#   - Run from the hydra-emacs-lisp directory
#
# Environment:
#   HYDRA_BENCHMARK_OUTPUT  If set, writes benchmark JSON to this path

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPT_DIR"

START_SEC=$(python3 -c 'import time; print(time.monotonic())')

# Run tests, capturing output while displaying it
EXIT_CODE=0
OUTPUT=$(emacs --batch --load run-tests.el 2>&1 | tee /dev/stderr) || EXIT_CODE=$?

END_SEC=$(python3 -c 'import time; print(time.monotonic())')

# Parse results from output (format: "Results: N passed, N failed, N skipped")
PASSED=$(echo "$OUTPUT" | sed -n 's/.*Results: *\([0-9]*\) passed.*/\1/p' | tail -1)
FAILED=$(echo "$OUTPUT" | sed -n 's/.*Results:.*passed, *\([0-9]*\) failed.*/\1/p' | tail -1)
SKIPPED=$(echo "$OUTPUT" | sed -n 's/.*Results:.*failed, *\([0-9]*\) skipped.*/\1/p' | tail -1)
PASSED=${PASSED:-0}
FAILED=${FAILED:-0}
SKIPPED=${SKIPPED:-0}

# Write benchmark JSON if requested
if [ -n "${HYDRA_BENCHMARK_OUTPUT:-}" ]; then
    ELAPSED_MS=$(python3 -c "print(round(($END_SEC - $START_SEC) * 1000, 1))")
    cat > "$HYDRA_BENCHMARK_OUTPUT" <<EOF
{
  "groups": [],
  "summary": {
    "totalPassed": ${PASSED},
    "totalFailed": ${FAILED},
    "totalSkipped": ${SKIPPED},
    "totalTimeMs": ${ELAPSED_MS}
  }
}
EOF
    echo "Benchmark output: $HYDRA_BENCHMARK_OUTPUT"
fi

exit $EXIT_CODE
