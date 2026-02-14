#!/bin/bash
# Run all bootstrapping paths: Haskell -> JSON -> {Haskell, Java, Python}
# This demonstrates that Hydra can regenerate itself from a language-independent
# JSON representation, validating 3 bootstrapping paths.
#
# All output goes to /tmp/hydra-bootstrapping-demo with subdirectories:
#   haskell-to-haskell/
#   haskell-to-java/
#   haskell-to-python/

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
OUTPUT_BASE="/tmp/hydra-bootstrapping-demo"

echo "=========================================="
echo "Hydra Bootstrapping Demo"
echo "=========================================="
echo ""
echo "This demo validates 3 bootstrapping paths:"
echo "  1. Haskell -> JSON -> Haskell"
echo "  2. Haskell -> JSON -> Java"
echo "  3. Haskell -> JSON -> Python"
echo ""
echo "All output will be written to: $OUTPUT_BASE"
echo ""

FAILED=()
PASSED=()

echo "=== Path 1: Bootstrap to Haskell ==="
echo ""
if "$SCRIPT_DIR/bootstrap-to-haskell.sh"; then
    echo ""
    echo "Path 1: PASSED"
    PASSED+=("Haskell")
else
    echo ""
    echo "Path 1: FAILED"
    FAILED+=("Haskell")
fi

echo ""
echo "=== Path 2: Bootstrap to Java ==="
echo ""
if "$SCRIPT_DIR/bootstrap-to-java.sh"; then
    echo ""
    echo "Path 2: PASSED"
    PASSED+=("Java")
else
    echo ""
    echo "Path 2: FAILED"
    FAILED+=("Java")
fi

echo ""
echo "=== Path 3: Bootstrap to Python ==="
echo ""
if "$SCRIPT_DIR/bootstrap-to-python.sh"; then
    echo ""
    echo "Path 3: PASSED"
    PASSED+=("Python")
else
    echo ""
    echo "Path 3: FAILED"
    FAILED+=("Python")
fi

echo ""
echo "=========================================="
echo "Summary"
echo "=========================================="
echo ""
echo "Output directory: $OUTPUT_BASE"
echo ""

if [ ${#PASSED[@]} -gt 0 ]; then
    echo "PASSED paths: ${PASSED[*]}"
fi

if [ ${#FAILED[@]} -eq 0 ]; then
    echo ""
    echo "All 3 bootstrapping paths PASSED!"
    echo ""
    echo "Generated output:"
    for d in haskell-to-haskell haskell-to-java haskell-to-python; do
      if [ -d "$OUTPUT_BASE/$d" ]; then
        COUNT=$(find "$OUTPUT_BASE/$d" -type f | wc -l | tr -d ' ')
        echo "  $OUTPUT_BASE/$d/ ($COUNT files)"
      fi
    done
    exit 0
else
    echo "FAILED paths: ${FAILED[*]}"
    exit 1
fi
