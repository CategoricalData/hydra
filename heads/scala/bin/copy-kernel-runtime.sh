#!/usr/bin/env bash
# Overlay the hand-written Scala kernel runtime + test runner onto
# dist/scala/hydra-kernel/ so the Scala distribution is self-contained (#434).
#
# The runtime's canonical home is the top-level overlay tree
# overlay/scala/hydra-kernel/src/{main,test}/scala/ (a sibling of dist/, packages/,
# heads/, bindings/ — see docs/build-system.md). It holds the hand-written
# primitive registry (hydra/lib/Libraries.scala), the native primitive impls
# (hydra/scala/lib/*.scala), and the test runner (hydra/TestSuiteRunner.scala,
# hydra/test/testEnv.scala). The generation drivers (Bootstrap.scala,
# Generation.scala) are NOT runtime and stay under heads/scala/src.
#
# This script is the ONLY reader of overlay/scala/. packages/hydra-scala/build.sbt
# lists the dist copy (dist/scala/hydra-kernel/src/{main,test}/scala) on its
# unmanagedSourceDirectories, so the sbt build consumes the runtime from dist —
# never from overlay/ or heads/. (heads/scala/src/main/scala remains on the path
# for the drivers only, so there is no longer a duplicate-class clash.)
#
# It runs AFTER the JSON->Scala transform's --prune-stale pass (the transform does
# not take a keep-paths manifest here), so the prune can't remove these files.
#
# Usage:
#   copy-kernel-runtime.sh [--dist-root <dir>]

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_SCALA_HEAD="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_SCALA_HEAD/../.." && pwd )"

DIST_ROOT="$HYDRA_ROOT_DIR/dist/scala"

while [ $# -gt 0 ]; do
    case "$1" in
        --dist-root) DIST_ROOT="$2"; shift 2 ;;
        *) shift ;;
    esac
done

OVERLAY_ROOT="$HYDRA_ROOT_DIR/overlay/scala/hydra-kernel/src"
OVERLAY_MAIN="$OVERLAY_ROOT/main/scala"
OVERLAY_TEST="$OVERLAY_ROOT/test/scala"
OUT_MAIN="$DIST_ROOT/hydra-kernel/src/main/scala"
OUT_TEST="$DIST_ROOT/hydra-kernel/src/test/scala"

if [ ! -d "$OVERLAY_MAIN" ]; then
    echo "error: missing overlay dir $OVERLAY_MAIN" >&2
    exit 1
fi

mkdir -p "$OUT_MAIN/hydra"
cp -R "$OVERLAY_MAIN/." "$OUT_MAIN/"
echo "  Overlaid hand-written Scala kernel runtime from overlay/scala/ into $OUT_MAIN/hydra/"

if [ -d "$OVERLAY_TEST" ]; then
    mkdir -p "$OUT_TEST/hydra"
    cp -R "$OVERLAY_TEST/." "$OUT_TEST/"
    echo "  Overlaid hand-written Scala kernel test runtime from overlay/scala/ into $OUT_TEST/hydra/"
fi
