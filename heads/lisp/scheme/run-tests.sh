#!/usr/bin/env bash
# Shim: delegate to the shared Lisp test runner.
exec "$(dirname "$0")/../../../packages/hydra-lisp/bin/run-tests.sh" scheme "$@"
