#!/usr/bin/env bash
# Layer 2.5 tester: run Clojure tests against an already-assembled distribution.
set -euo pipefail
if [ $# -lt 1 ]; then
    echo "Usage: $0 <package>" >&2
    exit 1
fi
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
THIS_SCRIPT="$SCRIPT_DIR/$(basename "${BASH_SOURCE[0]}")"
HYDRA_CLOJURE_HEAD="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "${HYDRA_CLOJURE_HEAD}/../../.." && pwd )"
echo "=== Testing Clojure distribution: $1 ==="

source "$HYDRA_ROOT_DIR/bin/lib/test-cache.sh"
if test_cache_check clojure "$HYDRA_ROOT_DIR/dist/clojure" "${HYDRA_CLOJURE_HEAD}/src/test" "$THIS_SCRIPT"; then
    echo "  Cache hit: no changes since last successful clojure test run; skipping."
    echo "=== Done (cache hit). ==="
    exit 0
fi
cd "$HYDRA_CLOJURE_HEAD"
bash run-tests.sh

test_cache_record clojure "$HYDRA_ROOT_DIR/dist/clojure" "${HYDRA_CLOJURE_HEAD}/src/test" "$THIS_SCRIPT"
