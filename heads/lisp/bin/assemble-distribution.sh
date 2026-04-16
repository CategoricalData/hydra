#!/usr/bin/env bash
# Layer 2 assembler: produce a complete Lisp distribution for one package and
# one dialect.
#
# Usage:
#   assemble-distribution.sh <pkg> <dialect> [--dist-root <dir>]
#
# Dialects: clojure, scheme, common-lisp, emacs-lisp.
#
# Status: PARTIAL. This script today only runs the Layer 1 transform and
# emits the generated source; it does NOT apply the extensive per-dialect
# post-processing that sync-lisp.sh performs (Scheme runtime library copy,
# stub modules, per-dialect testGraph patches, etc.). For full Lisp
# assembly, continue to use heads/haskell/bin/sync-lisp.sh.
#
# Refactoring the Lisp post-processing into this assembler is a tracked
# followup; see feature_290_packaging-plan.md session 7 status.

set -euo pipefail

if [ $# -lt 2 ]; then
    echo "Usage: $0 <package> <dialect> [--dist-root <dir>]" >&2
    echo "" >&2
    echo "Dialects: clojure, scheme, common-lisp, emacs-lisp" >&2
    exit 1
fi

PACKAGE="$1"
DIALECT="$2"
shift 2

case "$DIALECT" in
    clojure|scheme|common-lisp|emacs-lisp) ;;
    *)
        echo "Error: unknown Lisp dialect: $DIALECT" >&2
        exit 1
        ;;
esac

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_LISP_HEAD="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_LISP_HEAD/../.." && pwd )"

DIST_ROOT="$HYDRA_ROOT_DIR/dist/$DIALECT"

while [ $# -gt 0 ]; do
    case "$1" in
        --dist-root) DIST_ROOT="$2"; shift 2 ;;
        *) shift ;;
    esac
done

OUT_DIR="$DIST_ROOT/$PACKAGE"

echo "=== Assembling Lisp distribution: $PACKAGE / $DIALECT ==="
echo "  Output: $OUT_DIR"
echo "  (PARTIAL: post-processing not yet ported from sync-lisp.sh)"
echo ""

HASKELL_BIN="$HYDRA_ROOT_DIR/heads/haskell/bin"

echo "Step 1: Generating main Lisp modules..."
"$HASKELL_BIN/transform-json-to-lisp.sh" "$PACKAGE" "$DIALECT" main \
    --output "$OUT_DIR/src/main"

echo ""
echo "Step 2: Generating test Lisp modules..."
"$HASKELL_BIN/transform-json-to-lisp.sh" "$PACKAGE" "$DIALECT" test \
    --output "$OUT_DIR/src/test"

echo ""
echo "=== Done. $PACKAGE / $DIALECT assembled under $OUT_DIR ==="
echo "    (Note: post-processing patches not applied — see sync-lisp.sh)"
