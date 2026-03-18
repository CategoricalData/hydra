#!/bin/bash
set -eo pipefail

# Run Hydra Scheme test suite.
#
# Prerequisites:
#   - chibi-scheme or guile must be installed
#   - Run from the hydra-scheme directory
#
# Environment:
#   HYDRA_BENCHMARK_OUTPUT  If set, writes benchmark JSON to this path

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPT_DIR"

# Detect Scheme implementation
if command -v guile > /dev/null 2>&1; then
    SCHEME_CMD="guile -L src/gen-main/scheme -L src/gen-test/scheme -L src/main/scheme -s run-tests.scm"
elif command -v chibi-scheme > /dev/null 2>&1; then
    SCHEME_CMD="chibi-scheme -I src/gen-main/scheme -I src/gen-test/scheme -I src/main/scheme run-tests.scm"
else
    echo "Error: No Scheme implementation found. Install guile or chibi-scheme."
    exit 1
fi

START_SEC=$(python3 -c 'import time; print(time.monotonic())')

# Run tests, capturing stdout while letting stderr (Guile compilation messages) pass through
EXIT_CODE=0
OUTPUT=$($SCHEME_CMD 2>/dev/null | tee /dev/stderr) || EXIT_CODE=$?

END_SEC=$(python3 -c 'import time; print(time.monotonic())')

# Parse results from output (format: "N passed, N failed, N skipped")
PASSED=$(echo "$OUTPUT" | sed -n 's/^\([0-9]*\) passed,.*/\1/p' | tail -1)
FAILED=$(echo "$OUTPUT" | sed -n 's/.*passed, *\([0-9]*\) failed.*/\1/p' | tail -1)
SKIPPED=$(echo "$OUTPUT" | sed -n 's/.*failed, *\([0-9]*\) skipped.*/\1/p' | tail -1)
PASSED=${PASSED:-0}
FAILED=${FAILED:-0}
SKIPPED=${SKIPPED:-0}

# If no results were parsed but exit code is non-zero, report failure
if [ "$PASSED" = "0" ] && [ "$FAILED" = "0" ] && [ "$EXIT_CODE" -ne 0 ]; then
    FAILED=1
fi

# Benchmark JSON is written by the Scheme test runner when HYDRA_BENCHMARK_OUTPUT is set

exit $EXIT_CODE
