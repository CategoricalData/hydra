#!/bin/bash
set -eo pipefail

# Run Hydra Clojure test suite.
#
# Prerequisites:
#   - Clojure CLI (clojure) must be installed
#   - Run from the hydra-clojure directory
#
# Environment:
#   HYDRA_BENCHMARK_OUTPUT  If set, writes benchmark JSON to this path

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPT_DIR"

START_SEC=$(python3 -c 'import time; print(time.monotonic())')

# Run tests, capturing stdout while letting stderr (namespace warnings) pass through
EXIT_CODE=0
OUTPUT=$(clojure -M -m run-tests 2>&1 | tee /dev/stderr) || EXIT_CODE=$?

END_SEC=$(python3 -c 'import time; print(time.monotonic())')

# Parse results from output (format: "Pass: N\nFail: N\nSkip: N")
PASSED=$(echo "$OUTPUT" | sed -n 's/^Pass: *\([0-9]*\).*/\1/p' | tail -1)
FAILED=$(echo "$OUTPUT" | sed -n 's/^Fail: *\([0-9]*\).*/\1/p' | tail -1)
SKIPPED=$(echo "$OUTPUT" | sed -n 's/^Skip: *\([0-9]*\).*/\1/p' | tail -1)
PASSED=${PASSED:-0}
FAILED=${FAILED:-0}
SKIPPED=${SKIPPED:-0}

# Benchmark JSON is written by the Clojure test runner when HYDRA_BENCHMARK_OUTPUT is set

exit $EXIT_CODE
