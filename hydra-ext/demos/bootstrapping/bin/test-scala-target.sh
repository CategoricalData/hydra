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

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../../.." && pwd )"

echo "Running Scala tests..."
cd "$OUTPUT_DIR"
sbt test 2>&1
