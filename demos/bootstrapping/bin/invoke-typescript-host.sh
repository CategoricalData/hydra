#!/bin/bash
# TypeScript bootstrapping demo: loads Hydra modules from JSON and
# generates code for a target language via the TS-implemented coders in
# dist/typescript/hydra-<target>/.
#
# Usage: ./invoke-typescript-host.sh --target <typescript|python|java|haskell|scala|clojure|scheme|common-lisp|emacs-lisp> [--include-tests] [--kernel-only] [--types-only] [--output <dir>]
#
# When --output is provided, output goes to <dir>/typescript-to-<target>/.

set -eo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../.." && pwd )"

TARGET=""
OUTPUT_BASE=""
PASSTHROUGH_ARGS=()
while [ $# -gt 0 ]; do
    case "$1" in
        --target) TARGET="$2"; shift 2 ;;
        --target=*) TARGET="${1#--target=}"; shift ;;
        --output) OUTPUT_BASE="$2"; shift 2 ;;
        --output=*) OUTPUT_BASE="${1#--output=}"; shift ;;
        *) PASSTHROUGH_ARGS+=("$1"); shift ;;
    esac
done

if [ -z "$TARGET" ]; then
    echo "ERROR: --target is required"
    echo "Usage: $0 --target <lang> [--output <dir>] [--include-tests] [--kernel-only] [--types-only]"
    exit 1
fi

OUTPUT_ARGS=()
if [ -n "$OUTPUT_BASE" ]; then
    OUTPUT_DIR="$OUTPUT_BASE/typescript-to-$TARGET"
    OUTPUT_ARGS=(--output "$OUTPUT_DIR")
fi

# The TS bootstrap driver lives in the dist tree alongside the kernel runtime.
# It needs Node 20+ with tsx for ESM .ts execution.
BOOTSTRAP_TS="$HYDRA_ROOT/dist/typescript/hydra-kernel/src/main/typescript/hydra/bootstrap.ts"
if [ ! -f "$BOOTSTRAP_TS" ]; then
    echo "ERROR: $BOOTSTRAP_TS not found. Run heads/typescript/bin/copy-kernel-runtime.sh first."
    exit 1
fi

cd "$HYDRA_ROOT/heads/typescript"

# Bump the V8 stack so the TS-implemented kernel checker can recurse
# through deeply-nested Hydra terms (notably during TS->Java codegen,
# where the Java coder hits typeOfTerm/typeOfApplication ~11x more
# than Python does). Default V8 --stack-size is 864 KB. V8 segfaults
# if --stack-size exceeds the OS thread-stack limit, so raise ulimit -s
# first. Settings below: 64 MB ulimit (the macOS hard cap on this Mac),
# 56 MB V8 stack. Microbench recursion depth: 9k (default) -> 612k.
# Empirical effect on TS->Java kernel codegen: 33 stack overflows at
# default -> 5 at --stack-size=16384 -> 3 at --stack-size=49152 ->
# (see feature_126_typescript-plan.md for current measurement).
#
# 64MB stack segfaults — V8 thinks it has the space but trips into
# the OS thread-stack guard region. 56MB is the empirical ceiling
# stable on this Mac.
#
# NODE_OPTIONS rejects --stack-size, so we bypass npx and invoke
# tsx's CLI shim directly with `node --stack-size=N tsx-cli.mjs ...`.
ulimit -s 65520

echo "TypeScript host: generating $TARGET code..."
echo ""
node --stack-size=57344 node_modules/tsx/dist/cli.mjs "$BOOTSTRAP_TS" \
    --target "$TARGET" \
    --json-dir "$HYDRA_ROOT/dist/json/hydra-kernel/src/main/json" \
    "${OUTPUT_ARGS[@]}" \
    "${PASSTHROUGH_ARGS[@]}"
