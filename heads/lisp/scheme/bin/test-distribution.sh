#!/usr/bin/env bash
# Layer 2.5 tester: run Scheme tests against an already-assembled distribution.
set -euo pipefail
if [ $# -lt 1 ]; then
    echo "Usage: $0 <package>" >&2
    exit 1
fi
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
THIS_SCRIPT="$SCRIPT_DIR/$(basename "${BASH_SOURCE[0]}")"
HYDRA_SCHEME_HEAD="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "${HYDRA_SCHEME_HEAD}/../../.." && pwd )"
echo "=== Testing Scheme distribution: $1 ==="

source "$HYDRA_ROOT_DIR/bin/lib/test-cache.sh"
if test_cache_check scheme "$HYDRA_ROOT_DIR/dist/scheme" "${HYDRA_SCHEME_HEAD}/src/test" "$THIS_SCRIPT"; then
    echo "  Cache hit: no changes since last successful scheme test run; skipping."
    echo "=== Done (cache hit). ==="
    exit 0
fi
cd "$HYDRA_SCHEME_HEAD"
bash run-tests.sh

test_cache_record scheme "$HYDRA_ROOT_DIR/dist/scheme" "${HYDRA_SCHEME_HEAD}/src/test" "$THIS_SCRIPT"
