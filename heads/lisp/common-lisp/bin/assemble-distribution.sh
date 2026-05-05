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
