#!/usr/bin/env bash
# Layer 2 assembler: produce a complete Clojure distribution for one package.
#
# Usage:
#   assemble-distribution.sh <pkg> [--dist-root <dir>]
#
# Produces <dist-root>/<pkg>/ (default: ../../../dist/clojure/<pkg>/).
#
# Per-dialect bits only; the shared steps live in heads/lisp/bin/common.sh.

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

LISP_DIALECT="clojure"
LISP_PRETTY_NAME="Clojure"
LISP_HEAD_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
LISP_TEST_ENV="testEnv.clj"

source "$SCRIPT_DIR/../../bin/common.sh"

lisp_assemble_main "$@"
