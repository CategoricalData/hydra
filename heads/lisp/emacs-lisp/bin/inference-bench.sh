#!/usr/bin/env bash
# Cross-host inference benchmark — Emacs Lisp wrapper.
#
# Thin wrapper around heads/lisp/emacs-lisp/bin/inference-bench.el that
# fills in --json-dir / --bench-json-dir from the repo layout and
# forwards remaining args.
#
# Usage:
#   heads/lisp/emacs-lisp/bin/inference-bench.sh \
#       --sizes 0,10,25,50 --namespace hydra.bench.linearChain --out path.json
#
# Invoked by bin/run-inference-bench.sh; can also be run standalone.

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
# heads/lisp/emacs-lisp/bin/ -> repo root is ../../../..
REPO_ROOT="$( cd "$SCRIPT_DIR/../../../.." && pwd )"
JSON_DIR="$REPO_ROOT/dist/json/hydra-kernel/src/main/json"
BENCH_JSON_DIR="$REPO_ROOT/dist/json/hydra-bench/src/main/json"

EMACS="${EMACS:-emacs}"

exec "$EMACS" --batch --no-init-file \
    --script "$SCRIPT_DIR/inference-bench.el" \
    -- --json-dir "$JSON_DIR" --bench-json-dir "$BENCH_JSON_DIR" "$@"
