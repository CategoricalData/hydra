#!/usr/bin/env bash
# Layer 2.5 tester: run Java tests against an already-assembled distribution.
#
# Usage:
#   test-distribution.sh <pkg>
#
# Invokes Gradle against the hydra-java project. Today the Gradle project
# is monolithic (it references dist/java/hydra-kernel/ and dist/java/hydra-ext/
# as source sets), so per-package test scoping is limited.

set -euo pipefail

if [ $# -lt 1 ]; then
    echo "Usage: $0 <package>" >&2
    exit 1
fi

PACKAGE="$1"

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT_DIR="$( cd "$SCRIPT_DIR/../../.." && pwd )"

echo "=== Testing Java distribution: $PACKAGE ==="
echo "  (Note: gradle project is monolithic today — running the full test suite)"
echo ""

# Warm-cache short-circuit: if every Java source under dist/java/ and
# under heads/java/src/test/ is byte-identical to the last successful
# run's record, skip the whole gradle pipeline.
source "$HYDRA_ROOT_DIR/bin/lib/test-cache.sh"
if test_cache_check java "$HYDRA_ROOT_DIR/dist/java" "$HYDRA_ROOT_DIR/heads/java/src/test" "${BASH_SOURCE[0]}"; then
    echo "  Cache hit: no changes since last successful Java test run; skipping."
    echo "=== Done (cache hit). ==="
    exit 0
fi

cd "$HYDRA_ROOT_DIR"
./gradlew :hydra-java:compileJava
./gradlew :hydra-java:compileTestJava
./gradlew :hydra-java:test

test_cache_record java "$HYDRA_ROOT_DIR/dist/java" "$HYDRA_ROOT_DIR/heads/java/src/test" "${BASH_SOURCE[0]}"
