#!/bin/bash
set -e

# Script to regenerate Python generation tests from Hydra sources
# These tests verify that Hydra's Python code generator produces correct code.

echo "=========================================="
echo "Updating Python Generation Tests"
echo "=========================================="
echo ""

# Get the directory where this script is located
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_EXT_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"

cd "$HYDRA_EXT_DIR"

echo "Building update-python-generation-tests executable..."
stack build hydra-ext:exe:update-python-generation-tests

if [ $? -ne 0 ]; then
    echo "ERROR: Build failed"
    exit 1
fi

echo ""
echo "Build successful!"
echo ""
echo "Generating Python generation tests..."
echo ""

# Run with RTS flags to avoid stack overflow
# -K256M: Set stack size to 256MB
# -A32M: Set allocation area size to 32MB
stack exec update-python-generation-tests -- +RTS -K256M -A32M -RTS

if [ $? -eq 0 ]; then
    echo ""
    echo "=========================================="
    echo "Python generation tests updated!"
    echo "=========================================="
    echo ""
    echo "To run the tests:"
    echo "  cd ../hydra-python && pytest src/gen-test/python/generation/"
else
    echo ""
    echo "ERROR: Python generation test generation failed"
    exit 1
fi
