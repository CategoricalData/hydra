#!/usr/bin/env bash
# Layer 2.5 tester: run Emacs Lisp tests against an already-assembled distribution.
set -euo pipefail
if [ $# -lt 1 ]; then
    echo "Usage: $0 <package>" >&2
    exit 1
fi
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_EL_HEAD="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "${HYDRA_EL_HEAD}/../../.." && pwd )"
echo "=== Testing Emacs Lisp distribution: $1 ==="

source "$HYDRA_ROOT_DIR/bin/lib/test-cache.sh"
if test_cache_check emacs-lisp "$HYDRA_ROOT_DIR/dist/emacs-lisp" "${HYDRA_EL_HEAD}/src/test" "${BASH_SOURCE[0]}"; then
    echo "  Cache hit: no changes since last successful emacs-lisp test run; skipping."
    echo "=== Done (cache hit). ==="
    exit 0
fi
cd "$HYDRA_EL_HEAD"
bash run-tests.sh

test_cache_record emacs-lisp "$HYDRA_ROOT_DIR/dist/emacs-lisp" "${HYDRA_EL_HEAD}/src/test" "${BASH_SOURCE[0]}"
