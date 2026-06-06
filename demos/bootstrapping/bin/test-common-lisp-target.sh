#!/bin/bash
# Build and test a Common Lisp bootstrap target directory.
#
# Usage: ./test-common-lisp-target.sh <output-dir>

set -e

OUTPUT_DIR="$1"
if [ -z "$OUTPUT_DIR" ]; then
    echo "Usage: $0 <output-dir>"
    exit 1
fi

# test_graph.lisp post-generation patch removed. The DSL emits
# hydra_test_test_env_test_{context,graph} refs directly, and the
# hand-written heads/lisp/common-lisp/.../test_env.lisp resolves them.

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../.." && pwd )"
HYDRA_CL_DIR="$HYDRA_ROOT/heads/lisp/common-lisp"

# Defensive re-copy of static resources that setup-common-lisp-target.sh laid
# down. In the typescript-host cells (#444), these were observed missing at
# test time, even though the setup script ran. Root cause is unidentified;
# this guarantees the test step has the runner regardless. Idempotent: if
# setup already populated the dir, these cp's are no-ops on content. Mirrors
# setup-common-lisp-target.sh.
if [ ! -f "$OUTPUT_DIR/src/test/common-lisp/run-tests.lisp" ]; then
    mkdir -p "$OUTPUT_DIR/src/test/common-lisp"
    for f in "$HYDRA_CL_DIR/src/test/common-lisp"/*.lisp; do
        [ -f "$f" ] && cp "$f" "$OUTPUT_DIR/src/test/common-lisp/"
    done
fi
if [ -d "$HYDRA_CL_DIR/src/test/common-lisp/hydra" ]; then
    mkdir -p "$OUTPUT_DIR/src/test/common-lisp/hydra"
    cp -r "$HYDRA_CL_DIR/src/test/common-lisp/hydra/." "$OUTPUT_DIR/src/test/common-lisp/hydra/"
fi

echo "Running Common Lisp tests..."
cd "$OUTPUT_DIR"
# HYDRA_LISP_DIST_BASE tells the (head's) run-tests.lisp to find generated
# kernel + test modules under <demo>/src/{main,test}/common-lisp/ rather than
# the head-relative <head>/../../../dist/common-lisp/hydra-kernel/... fallback,
# which is wrong from /tmp/hydra-bootstrapping-demo-*/haskell-to-common-lisp/.
HYDRA_BENCHMARK_OUTPUT="${HYDRA_BENCHMARK_OUTPUT:-}" \
HYDRA_LISP_DIST_BASE="$OUTPUT_DIR" \
    sbcl --noinform --non-interactive --no-userinit --load src/test/common-lisp/run-tests.lisp 2>&1
