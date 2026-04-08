#!/usr/bin/env bash
# Shim: delegate to the shared Lisp test runner.
exec "$(dirname "$0")/../bin/run-tests.sh" common-lisp "$@"
