#!/bin/bash
# Scala bootstrapping: loads Hydra modules from JSON and generates code.
#
# Usage: ./invoke-scala-host.sh --target <haskell|java|python|scala> [--include-tests] [--kernel-only] [--output <dir>]

set -eo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../../../.." && pwd )"
HYDRA_SCALA_DIR="$HYDRA_ROOT/packages/hydra-scala"

# Parse arguments to find target (for coder module check)
TARGET=""
EXTRA_ARGS=""
JSON_DIR=""

while [ $# -gt 0 ]; do
    case "$1" in
        --target) TARGET="$2"; EXTRA_ARGS="$EXTRA_ARGS --target $2"; shift ;;
        --output) EXTRA_ARGS="$EXTRA_ARGS --output $2"; shift ;;
        --json-dir) JSON_DIR="$2"; EXTRA_ARGS="$EXTRA_ARGS --json-dir $2"; shift ;;
        *) EXTRA_ARGS="$EXTRA_ARGS $1" ;;
    esac
    shift
done

if [ -z "$TARGET" ]; then
    echo "Usage: $0 --target <lang> --output <dir> [OPTIONS]"
    exit 1
fi

if [ -z "$JSON_DIR" ]; then
    JSON_DIR="$HYDRA_ROOT/packages/hydra-kernel/src/gen-main/json"
    EXTRA_ARGS="$EXTRA_ARGS --json-dir $JSON_DIR"
fi

echo "Scala host: generating $TARGET code..."
echo ""

cd "$HYDRA_SCALA_DIR"
# sbt wraps program println output as [info] lines. We need to let those
# through (not use -warn) and strip the [info] prefix so the output matches
# the log format expected by bootstrap-all.sh's parse_bootstrap_log.
# Filter out sbt's own [info] noise (loading, compiling, etc.) but keep
# program output by only stripping [info] from lines that don't start with
# sbt-specific patterns.
sbt "runMain hydra.bootstrap $EXTRA_ARGS" 2>&1 \
    | grep -v '^\[info\] \(welcome\|loading\|set current\|compiling\|done compiling\|Executing\)' \
    | grep -v '^\[warn\]' \
    | sed 's/^\[info\] //'
