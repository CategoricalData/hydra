#!/usr/bin/env bash
# Layer 2 assembler: produce a complete Scheme distribution for one package.
#
# Usage:
#   assemble-distribution.sh <pkg> [--dist-root <dir>]
#
# Produces <dist-root>/<pkg>/ (default: ../../../dist/scheme/<pkg>/).
#
# Per-dialect bits only; the shared steps live in heads/lisp/bin/common.sh.
# Scheme also needs to copy runtime libraries and write empty
# define-library stub modules for hydra-kernel; that's done after the
# common steps via scheme_post_kernel_extras.

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

LISP_DIALECT="scheme"
LISP_PRETTY_NAME="Scheme"
LISP_HEAD_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
LISP_TEST_ENV="test_env.scm"

source "$SCRIPT_DIR/../../bin/common.sh"

lisp_assemble_main "$@"

if [ "$PACKAGE" = "hydra-kernel" ]; then
    scheme_post_kernel_extras "$OUT_DIR"
fi
