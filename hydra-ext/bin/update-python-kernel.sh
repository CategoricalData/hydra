#!/bin/bash
set -e

# Script to regenerate the Python kernel from Hydra sources
# This script builds the hydra-ext-debug executable and runs it with
# proper RTS flags to avoid stack overflow during generation.

echo "=========================================="
echo "Updating Python Kernel"
echo "=========================================="
echo ""

# Get the directory where this script is located
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_EXT_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"

cd "$HYDRA_EXT_DIR"

echo "Building hydra-ext-debug executable..."
stack build hydra-ext:exe:hydra-ext-debug

if [ $? -ne 0 ]; then
    echo "ERROR: Build failed"
    exit 1
fi

echo ""
echo "Build successful!"
echo ""
echo "Generating Python kernel..."
echo "This may take several minutes and use significant memory."
echo ""

# Run with RTS flags to avoid stack overflow
# -K256M: Set stack size to 256MB
# -A32M: Set allocation area size to 32MB
stack exec hydra-ext-debug -- +RTS -K256M -A32M -RTS

if [ $? -eq 0 ]; then
    echo ""
    echo "=========================================="
    echo "Python kernel generation complete!"
    echo "=========================================="
else
    echo ""
    echo "ERROR: Python generation failed"
    exit 1
fi
