#!/usr/bin/env bash
# Layer 2.5 tester: run Lisp tests against an already-assembled distribution.
#
# Usage:
#   test-distribution.sh <pkg> <dialect>
#
# Status: STUB. For now, delegates to the existing sync-lisp.sh --dialects
# <dialect> workflow (which both assembles and tests). Refactoring per-
# dialect test runners into this script is a tracked followup.

set -euo pipefail

if [ $# -lt 2 ]; then
    echo "Usage: $0 <package> <dialect>" >&2
    exit 1
fi

PACKAGE="$1"
DIALECT="$2"

echo "=== Testing Lisp distribution: $PACKAGE / $DIALECT ==="
echo "  (STUB: per-dialect test runners not yet ported from sync-lisp.sh.)"
echo "  For now, run: heads/haskell/bin/sync-lisp.sh --dialects $DIALECT"
echo ""
exit 1
