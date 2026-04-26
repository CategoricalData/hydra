#!/bin/bash
# Generate code using the Clojure host implementation.
#
# The Clojure host loads Hydra modules from JSON and generates code for a
# target language using Clojure's generated coder modules.
#
# Prerequisites:
#   - Clojure CLI (clojure) must be installed
#   - The Haskell bootstrap-from-json must be available (for coder module setup)
#
# Usage: ./invoke-clojure-host.sh --target <lang> --output <dir> [OPTIONS]

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../.." && pwd )"
HYDRA_CLOJURE_DIR="$HYDRA_ROOT/heads/lisp/clojure"

# Parse arguments (pass through to Clojure bootstrap)
TARGET=""
OUTPUT_BASE=""
EXTRA_ARGS=""
JSON_DIR=""

while [ $# -gt 0 ]; do
    case "$1" in
        --target) TARGET="$2"; EXTRA_ARGS="$EXTRA_ARGS --target $2"; shift ;;
        --output) OUTPUT_BASE="$2"; EXTRA_ARGS="$EXTRA_ARGS --output $2"; shift ;;
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
    # The Clojure bootstrap still takes a single kernel JSON directory.
    # Full per-package walking (see issue #290 Phase 1c) is not yet implemented.
    JSON_DIR="$HYDRA_ROOT/dist/json/hydra-kernel/src/main/json"
    EXTRA_ARGS="$EXTRA_ARGS --json-dir $JSON_DIR"
fi

OUTPUT_DIR="${OUTPUT_BASE}/clojure-to-${TARGET}"

echo "Clojure host: generating $TARGET code..."
echo "  Target:    $TARGET"
echo "  Output:    $OUTPUT_DIR"
echo ""

# Step 1: Ensure coder modules are available in the Clojure project.
# The Clojure implementation needs the target language's coder modules
# generated into its dist directory. If they don't exist, generate them
# from ext JSON using the Haskell bootstrap-from-json.
CODER_CHECK=""
case "$TARGET" in
    haskell)                            CODER_CHECK="$HYDRA_ROOT/dist/clojure/hydra-kernel/src/main/clojure/hydra/haskell/coder.clj" ;;
    java)                               CODER_CHECK="$HYDRA_ROOT/dist/clojure/hydra-kernel/src/main/clojure/hydra/java/coder.clj" ;;
    python)                             CODER_CHECK="$HYDRA_ROOT/dist/clojure/hydra-kernel/src/main/clojure/hydra/python/coder.clj" ;;
    clojure|scheme|common-lisp|emacs-lisp) CODER_CHECK="$HYDRA_ROOT/dist/clojure/hydra-kernel/src/main/clojure/hydra/lisp/coder.clj" ;;
esac

if [ -n "$CODER_CHECK" ] && [ ! -f "$CODER_CHECK" ]; then
    echo "  Coder modules not found. Generating from per-package JSON..."

    # Build the Haskell bootstrap-from-json if needed
    cd "$HYDRA_ROOT/heads/haskell"
    stack build hydra:exe:bootstrap-from-json 2>&1 | grep -v "^$"

    # --dist-json-root points at the dist/json root; the bootstrap walks
    # per-package subdirectories in dependency order.
    stack exec bootstrap-from-json -- \
        --target clojure \
        --output "$HYDRA_ROOT/dist/clojure/hydra-kernel" \
        --include-coders \
        --dist-json-root "$HYDRA_ROOT/dist/json" 2>&1

    echo "  Coder modules generated."
    echo ""
fi

# Step 2: Run the Clojure bootstrap
cd "$HYDRA_CLOJURE_DIR"
clojure -M -m hydra.bootstrap $EXTRA_ARGS
