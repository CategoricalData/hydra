#!/bin/bash
set -eo pipefail

# Script to synchronize Hydra-Java with the source of truth in Hydra-Haskell/Hydra-Ext.
#
# This script regenerates all Java artifacts from the Hydra sources:
#   1. Main modules, eval lib, and coder modules (from JSON)
#   2. Kernel test modules (from JSON)
#   3. Generation tests (from Haskell DSL)
#   4. Ext Java modules (PG model, decoders, encoders, etc.) into hydra-ext
#
# Prerequisites:
#   - JSON modules must be up to date (run sync-haskell.sh and sync-ext.sh first)
#   - Run from the hydra-ext directory
#
# Usage:
#   ./bin/sync-java.sh          # Full sync (all steps)
#   ./bin/sync-java.sh --quick  # Skip tests (for faster iteration)
#   ./bin/sync-java.sh --help   # Show this help

QUICK_MODE=false

for arg in "$@"; do
    case $arg in
        --quick)
            QUICK_MODE=true
            shift
            ;;
        --help|-h)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Synchronize Hydra-Java with the source of truth in Hydra-Haskell/Hydra-Ext."
            echo ""
            echo "Options:"
            echo "  --quick    Skip running Java tests after generation"
            echo "  --help     Show this help message"
            echo ""
            echo "Steps performed:"
            echo "  1. Build executable"
            echo "  2. Generate Java main modules and tests from JSON"
            echo "  3. Generate ext Java modules (PG model, decoders, etc.)"
            echo "  4. Build and test Java (unless --quick)"
            echo "  5. Report new files to git add"
            exit 0
            ;;
    esac
done

echo "=========================================="
echo "Synchronizing Hydra-Java"
echo "=========================================="
echo ""

# Warn if running an x86_64 JDK under Rosetta on Apple Silicon (causes ~20x slowdown)
if [ "$(uname -s)" = "Darwin" ] && [ "$(uname -m)" = "arm64" ]; then
    JAVA_CMD="${JAVA_HOME:+$JAVA_HOME/bin/}java"
    if command -v "$JAVA_CMD" > /dev/null 2>&1 && file "$(command -v "$JAVA_CMD")" | grep -q x86_64; then
        echo "WARNING: x86_64 JDK detected on Apple Silicon. This runs under Rosetta 2"
        echo "  and will be ~20x slower than a native arm64 JDK."
        echo "  Current JDK: $("$JAVA_CMD" -version 2>&1 | head -1)"
        echo ""
    fi
fi

# Get the directory where this script is located
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_EXT_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_EXT_DIR/.." && pwd )"
HYDRA_JAVA_DIR="$HYDRA_ROOT_DIR/hydra-java"

cd "$HYDRA_EXT_DIR"

# RTS flags to avoid stack overflow during generation
RTS_FLAGS="+RTS -K256M -A32M -RTS"

echo "Step 1/5: Building executable..."
echo ""
stack build hydra-ext:exe:bootstrap-from-json

echo ""
echo "Step 2/5: Generating Java main modules and tests from JSON..."
echo ""
stack exec bootstrap-from-json -- --target java --include-coders --include-tests --include-gentests $RTS_FLAGS

echo ""
echo "Step 3/5: Generating ext Java demo modules from JSON..."
echo ""
stack exec bootstrap-from-json -- --target java --output . --include-coders --ext-only $RTS_FLAGS

if [ "$QUICK_MODE" = false ]; then
    echo ""
    echo "Step 4/5: Building and testing Java..."
    echo ""

    cd "$HYDRA_ROOT_DIR"

    ./gradlew :hydra-java:compileJava :hydra-ext:compileJava
    ./gradlew :hydra-java:compileTestJava :hydra-ext:compileTestJava
    ./gradlew :hydra-java:test :hydra-ext:test

    cd "$HYDRA_EXT_DIR"
else
    echo ""
    echo "Step 4/5: Skipped (--quick mode)"
fi

echo ""
echo "Step 5/5: Generating ext Java modules from JSON..."
echo ""

HYDRA_EXT_JAVA_DIR="$HYDRA_EXT_DIR"

for CHECK_DIR in "$HYDRA_JAVA_DIR" "$HYDRA_EXT_JAVA_DIR"; do
    cd "$CHECK_DIR"
    LABEL=$(basename "$CHECK_DIR")

    NEW_FILES=$(git status --porcelain src/gen-main/java src/gen-test/java 2>/dev/null | grep "^??" | awk '{print $2}' || true)

    if [ -n "$NEW_FILES" ]; then
        echo "New files in $LABEL. You may want to run:"
        echo ""
        echo "  cd $CHECK_DIR"
        echo "  git add src/gen-main/java src/gen-test/java"
        echo ""
        echo "New files:"
        echo "$NEW_FILES" | head -20
        NEW_COUNT=$(echo "$NEW_FILES" | wc -l | tr -d ' ')
        if [ "$NEW_COUNT" -gt 20 ]; then
            echo "  ... and $((NEW_COUNT - 20)) more"
        fi
        echo ""
    else
        echo "No new files in $LABEL."
    fi
done

echo ""
echo "=========================================="
echo "Sync complete!"
echo "=========================================="
