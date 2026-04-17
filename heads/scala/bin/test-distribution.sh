#!/usr/bin/env bash
# Layer 2.5 tester: run Scala tests against an already-assembled distribution.

set -euo pipefail

if [ $# -lt 1 ]; then
    echo "Usage: $0 <package>" >&2
    exit 1
fi

PACKAGE="$1"

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT_DIR="$( cd "$SCRIPT_DIR/../../.." && pwd )"
HYDRA_SCALA_PROJ="$HYDRA_ROOT_DIR/packages/hydra-scala"

echo "=== Testing Scala distribution: $PACKAGE ==="
echo "  (Note: sbt project is monolithic today — running the full test suite)"
echo ""

cd "$HYDRA_SCALA_PROJ"
sbt test
