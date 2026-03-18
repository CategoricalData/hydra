#!/bin/bash
set -eo pipefail

# Script to regenerate Lisp code for all four dialects from Hydra sources.
#
# This generates Clojure, Common Lisp, Emacs Lisp, and Scheme code
# from the Hydra kernel and test modules using the Lisp coder.
#
# Prerequisites:
#   - Hydra-Ext must be consistent (run sync-ext.sh first)
#   - Run from the hydra-ext directory (or the script will cd there)
#
# Usage:
#   ./bin/sync-lisp.sh                                  # Generate all four dialects
#   ./bin/sync-lisp.sh --dialects clojure,scheme        # Generate specific dialects
#   ./bin/sync-lisp.sh --quick                          # Skip tests
#   ./bin/sync-lisp.sh --help                           # Show this help

QUICK_MODE=false
DIALECTS="clojure,common-lisp,emacs-lisp,scheme"

while [ $# -gt 0 ]; do
    case "$1" in
        --quick)
            QUICK_MODE=true
            ;;
        --dialects)
            DIALECTS="$2"
            shift
            ;;
        --help|-h)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Regenerate Lisp code from Hydra sources."
            echo ""
            echo "Options:"
            echo "  --dialects D,...  Generate only specified dialects (default: all four)"
            echo "                    Valid dialects: clojure, common-lisp, emacs-lisp, scheme"
            echo "  --quick           Skip running tests after generation"
            echo "  --help            Show this help message"
            echo ""
            echo "Steps performed:"
            echo "  1. Build generate-lisp executable"
            echo "  2. Generate Lisp code for selected dialects"
            echo "  3. Run tests for each dialect (unless --quick)"
            exit 0
            ;;
    esac
    shift
done

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_EXT_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_EXT_DIR/.." && pwd )"

cd "$HYDRA_EXT_DIR"

# RTS flags to avoid stack overflow during generation
RTS_FLAGS="+RTS -K256M -A32M -RTS"

# Parse dialect list
IFS=',' read -ra DIALECT_LIST <<< "$DIALECTS"

# Expand 'lisp' and 'all' to all four dialects
expanded=()
for d in "${DIALECT_LIST[@]}"; do
    case "$d" in
        lisp|all) expanded+=(clojure common-lisp emacs-lisp scheme) ;;
        *)        expanded+=("$d") ;;
    esac
done
DIALECT_LIST=("${expanded[@]}")

# Validate dialects
for d in "${DIALECT_LIST[@]}"; do
    case "$d" in
        clojure|common-lisp|emacs-lisp|scheme) ;;
        *)
            echo "Error: Unknown dialect '$d'. Valid dialects: clojure, common-lisp, emacs-lisp, scheme"
            exit 1
            ;;
    esac
done

echo "=========================================="
echo "Synchronizing Lisp (${DIALECT_LIST[*]})"
echo "=========================================="
echo ""

echo "Step 1: Building generate-lisp executable..."
echo ""
stack build hydra-ext:exe:generate-lisp

echo ""
echo "Step 2: Generating Lisp code..."
echo ""
# generate-lisp generates all four dialects; we run it once
# and let the user's --dialects flag control which tests to run
stack exec generate-lisp -- $RTS_FLAGS

dialect_dir() {
    case "$1" in
        clojure)     echo "$HYDRA_ROOT_DIR/hydra-lisp/hydra-clojure" ;;
        common-lisp) echo "$HYDRA_ROOT_DIR/hydra-lisp/hydra-common-lisp" ;;
        emacs-lisp)  echo "$HYDRA_ROOT_DIR/hydra-lisp/hydra-emacs-lisp" ;;
        scheme)      echo "$HYDRA_ROOT_DIR/hydra-lisp/hydra-scheme" ;;
    esac
}

if [ "$QUICK_MODE" = false ]; then
    echo ""
    echo "Step 3: Running tests..."
    echo ""

    for dialect in "${DIALECT_LIST[@]}"; do
        DIR=$(dialect_dir "$dialect")
        echo "  Testing ${dialect}..."
        if [ -f "$DIR/run-tests.sh" ]; then
            cd "$DIR"
            bash run-tests.sh
            cd "$HYDRA_EXT_DIR"
        else
            echo "    Skipped (no run-tests.sh found in $DIR)"
        fi
        echo ""
    done
else
    echo ""
    echo "Step 3: Skipped (--quick mode)"
fi

# Report new files
for dialect in "${DIALECT_LIST[@]}"; do
    CHECK_DIR=$(dialect_dir "$dialect")
    LABEL=$(basename "$CHECK_DIR")
    if [ -d "$CHECK_DIR" ]; then
        NEW_FILES=$(cd "$CHECK_DIR" && git status --porcelain src/gen-main src/gen-test 2>/dev/null | grep "^??" | awk '{print $2}' || true)
        if [ -n "$NEW_FILES" ]; then
            NEW_COUNT=$(echo "$NEW_FILES" | wc -l | tr -d ' ')
            echo "New files in $LABEL ($NEW_COUNT). You may want to run:"
            echo "  cd $CHECK_DIR && git add src/gen-main src/gen-test"
            echo ""
        fi
    fi
done

echo ""
echo "=========================================="
echo "Lisp sync complete!"
echo "=========================================="
