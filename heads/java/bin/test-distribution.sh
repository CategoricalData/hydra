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

cd "$HYDRA_ROOT_DIR"
./gradlew :hydra-java:compileJava
./gradlew :hydra-java:compileTestJava
./gradlew :hydra-java:test
