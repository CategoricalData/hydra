#!/bin/bash
# Build and test an Emacs Lisp bootstrap target directory.
#
# Usage: ./test-emacs-lisp-target.sh <output-dir>

set -e

OUTPUT_DIR="$1"
if [ -z "$OUTPUT_DIR" ]; then
    echo "Usage: $0 <output-dir>"
    exit 1
fi

# Copy hand-written test_env.el into the dist tree. The DSL emits
# (require 'hydra.test.testEnv) and references
# hydra_test_test_env_test_{context,graph} directly; this file provides them.
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../.." && pwd )"
TEST_ENV_SRC="$HYDRA_ROOT/heads/lisp/emacs-lisp/src/test/emacs-lisp/hydra/test/test_env.el"
TEST_ENV_DST="$OUTPUT_DIR/src/test/emacs-lisp/hydra/test/test_env.el"
if [ -f "$TEST_ENV_SRC" ]; then
    mkdir -p "$(dirname "$TEST_ENV_DST")"
    cp "$TEST_ENV_SRC" "$TEST_ENV_DST"
fi

echo "Running Emacs Lisp tests..."
cd "$OUTPUT_DIR"
HYDRA_BENCHMARK_OUTPUT="${HYDRA_BENCHMARK_OUTPUT:-}" emacs --batch --load run-tests.el 2>&1
