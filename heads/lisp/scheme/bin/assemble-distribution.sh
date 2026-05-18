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

# #357: for hydra-kernel, build a keep-paths manifest BEFORE lisp_assemble_main
# so that bootstrap-from-json --prune-stale won't delete the runtime libs +
# stubs that scheme_post_kernel_extras drops in after the common steps. The
# manifest enumerates paths only; the actual copy still happens in the
# post-extras call. lisp_assemble_main forwards LISP_KEEP_MANIFEST to
# bootstrap-from-json's --keep-paths-from when set.
LISP_KEEP_MANIFEST=""
if [ "${1-}" = "hydra-kernel" ]; then
    LISP_KEEP_MANIFEST="$(mktemp -t hydra-keep-paths-scheme.XXXXXX)"
    trap 'rm -f "$LISP_KEEP_MANIFEST"' EXIT
    scheme_keep_paths "$LISP_KEEP_MANIFEST" "$@"
fi

lisp_assemble_main "$@"

if [ "$PACKAGE" = "hydra-kernel" ]; then
    scheme_post_kernel_extras "$OUT_DIR"
fi
