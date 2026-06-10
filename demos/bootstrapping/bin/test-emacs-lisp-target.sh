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

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../.." && pwd )"
HYDRA_ELISP_DIR="$HYDRA_ROOT/heads/lisp/emacs-lisp"

# Defensive re-copy of run-tests.el that setup-emacs-lisp-target.sh laid down.
# In the scala-host sc->emacs-lisp cell (#456, sibling of #444), run-tests.el
# was observed missing at test time, even though the setup script ran. Root
# cause is unidentified; this guarantees the test step has the runner
# regardless. Idempotent. Mirrors test-{clojure,common-lisp,scheme}-target.sh.
if [ ! -f "$OUTPUT_DIR/run-tests.el" ]; then
    cp "$HYDRA_ELISP_DIR/run-tests.el" "$OUTPUT_DIR/"
fi

# Copy hand-written test_env.el into the dist tree. The DSL emits
# (require 'hydra.test.testEnv) and references
# hydra_test_test_env_test_{context,graph} directly; this file provides them.
TEST_ENV_SRC="$HYDRA_ELISP_DIR/src/test/emacs-lisp/hydra/test/test_env.el"
TEST_ENV_DST="$OUTPUT_DIR/src/test/emacs-lisp/hydra/test/test_env.el"
if [ -f "$TEST_ENV_SRC" ]; then
    mkdir -p "$(dirname "$TEST_ENV_DST")"
    cp "$TEST_ENV_SRC" "$TEST_ENV_DST"
fi

echo "Running Emacs Lisp tests..."
cd "$OUTPUT_DIR"
# HYDRA_LISP_DIST_BASE tells the (head's) run-tests.el to find generated
# content under <demo>/src/{main,test}/emacs-lisp/ rather than the
# head-relative <head>/../../../dist/emacs-lisp/hydra-kernel/... fallback,
# which is wrong from /tmp/hydra-bootstrapping-demo-*/haskell-to-emacs-lisp/.
HYDRA_BENCHMARK_OUTPUT="${HYDRA_BENCHMARK_OUTPUT:-}" \
HYDRA_LISP_DIST_BASE="$OUTPUT_DIR" \
    emacs --batch --load run-tests.el 2>&1
