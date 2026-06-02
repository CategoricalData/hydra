#!/usr/bin/env bash
# Layer 2 assembler: produce a complete Common Lisp distribution for one package.
#
# Usage:
#   assemble-distribution.sh <pkg> [--dist-root <dir>]
#
# Produces <dist-root>/<pkg>/ (default: ../../../dist/common-lisp/<pkg>/).
#
# Per-dialect bits only; the shared steps live in heads/lisp/bin/common.sh.
#
# Common Lisp does NOT need the test_env.lisp copy that the other three
# dialects perform: run-tests.lisp loads test_env.lisp directly from
# heads/lisp/common-lisp/ before loading test_graph.lisp from dist.
# So LISP_TEST_ENV is left empty.

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

LISP_DIALECT="common-lisp"
LISP_PRETTY_NAME="Common Lisp"
LISP_HEAD_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
LISP_TEST_ENV=""

source "$SCRIPT_DIR/../../bin/common.sh"

lisp_assemble_main "$@"

# Step 4 (Common Lisp only): regenerate the alist-based struct compatibility
# layer from the freshly generated kernel structs.
#
# struct-compat.lisp provides positional (make-<struct> v1 v2 ...) constructors
# whose argument-to-field mapping is a hardcoded keyword list in field-declaration
# order. The Lisp coder emits record construction positionally, so any kernel
# record field reorder (e.g. #402's Module/Package identity-first reorder) silently
# desyncs this file and routes values into the wrong fields — see #407. gen-compat.sh
# reads dist/common-lisp/hydra-kernel/.../hydra/*.lisp, so it must run after the
# kernel has been assembled. Regenerating it here makes it impossible to go stale.
if [ "${PACKAGE:-}" = "hydra-kernel" ]; then
    echo ""
    echo "Step 4: Regenerating struct-compat.lisp from generated kernel structs..."
    bash "$LISP_HEAD_DIR/src/main/common-lisp/hydra/gen-compat.sh"
fi
