#!/usr/bin/env bash
# Layer 2 assembler: produce a complete Emacs Lisp distribution for one package.
#
# Usage:
#   assemble-distribution.sh <pkg> [--dist-root <dir>]
#
# Produces <dist-root>/<pkg>/ (default: ../../../dist/emacs-lisp/<pkg>/).
#
# Per-dialect bits only; the shared steps live in heads/lisp/bin/common.sh.

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

LISP_DIALECT="emacs-lisp"
LISP_PRETTY_NAME="Emacs Lisp"
LISP_HEAD_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
LISP_TEST_ENV="test_env.el"

source "$SCRIPT_DIR/../../bin/common.sh"

lisp_assemble_main "$@"
