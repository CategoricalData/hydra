#!/usr/bin/env bash
# Cross-host inference benchmark — Common Lisp wrapper.
#
# Thin wrapper around heads/lisp/common-lisp/bin/inference-bench.lisp that
# fills in --json-dir from the repo layout and forwards remaining args.
#
# Usage:
#   heads/lisp/common-lisp/bin/inference-bench.sh \
#       --sizes 0,10,25,50 --namespace hydra.bench.linearChain --out path.json
#
# Invoked by bin/run-inference-bench.sh; can also be run standalone.

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
# heads/lisp/common-lisp/bin/ -> repo root is ../../../..
REPO_ROOT="$( cd "$SCRIPT_DIR/../../../.." && pwd )"
JSON_DIR="$REPO_ROOT/dist/json/hydra-kernel/src/main/json"

exec sbcl --noinform --no-sysinit --no-userinit \
    --disable-debugger \
    --script "$SCRIPT_DIR/inference-bench.lisp" \
    -- --json-dir "$JSON_DIR" "$@"
