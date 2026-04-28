#!/usr/bin/env bash
# Layer 2.5 tester: run Scala tests against an already-assembled distribution.

set -euo pipefail

if [ $# -lt 1 ]; then
    echo "Usage: $0 <package>" >&2
    exit 1
fi

PACKAGE="$1"

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
THIS_SCRIPT="$SCRIPT_DIR/$(basename "${BASH_SOURCE[0]}")"
HYDRA_ROOT_DIR="$( cd "$SCRIPT_DIR/../../.." && pwd )"
HYDRA_SCALA_PROJ="$HYDRA_ROOT_DIR/packages/hydra-scala"

echo "=== Testing Scala distribution: $PACKAGE ==="
echo "  (Note: sbt project is monolithic today — running the full test suite)"
echo ""

source "$HYDRA_ROOT_DIR/bin/lib/test-cache.sh"
if test_cache_check scala "$HYDRA_ROOT_DIR/dist/scala" "$HYDRA_ROOT_DIR/heads/scala/src/test" "$THIS_SCRIPT"; then
    echo "  Cache hit: no changes since last successful Scala test run; skipping."
    echo "=== Done (cache hit). ==="
    exit 0
fi

cd "$HYDRA_SCALA_PROJ"
sbt test

test_cache_record scala "$HYDRA_ROOT_DIR/dist/scala" "$HYDRA_ROOT_DIR/heads/scala/src/test" "$THIS_SCRIPT"
