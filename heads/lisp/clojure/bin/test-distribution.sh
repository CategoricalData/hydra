#!/usr/bin/env bash
# Layer 2.5 tester: run Clojure tests against an already-assembled distribution.
set -euo pipefail
if [ $# -lt 1 ]; then
    echo "Usage: $0 <package>" >&2
    exit 1
fi
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_CLOJURE_HEAD="$( cd "$SCRIPT_DIR/.." && pwd )"
echo "=== Testing Clojure distribution: $1 ==="
cd "$HYDRA_CLOJURE_HEAD"
bash run-tests.sh
