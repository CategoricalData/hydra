#!/usr/bin/env bash
# Layer 2.5 tester: run Emacs Lisp tests against an already-assembled distribution.
set -euo pipefail
if [ $# -lt 1 ]; then
    echo "Usage: $0 <package>" >&2
    exit 1
fi
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_EL_HEAD="$( cd "$SCRIPT_DIR/.." && pwd )"
echo "=== Testing Emacs Lisp distribution: $1 ==="
cd "$HYDRA_EL_HEAD"
bash run-tests.sh
