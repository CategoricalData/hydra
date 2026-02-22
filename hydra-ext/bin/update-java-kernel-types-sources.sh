#!/bin/bash
set -e

# Script to generate Java kernel TYPE source modules from Hydra kernel type modules.
# These are "source-level" representations of the 22 kernel type modules that contain
# the Module AST as data, providing the type universe needed for JSON module decoding.

echo "=========================================="
echo "Updating Java Kernel Type Sources"
echo "=========================================="
echo ""

# Get the directory where this script is located
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_EXT_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"

cd "$HYDRA_EXT_DIR"

echo "Building update-java-kernel-types-sources executable..."
stack build hydra-ext:exe:update-java-kernel-types-sources

if [ $? -ne 0 ]; then
    echo "ERROR: Build failed"
    exit 1
fi

echo ""
echo "Build successful!"
echo ""
echo "Generating Java kernel type sources..."
echo ""

# Run with RTS flags to avoid stack overflow
# -K256M: Set stack size to 256MB
# -A32M: Set allocation area size to 32MB
stack exec update-java-kernel-types-sources -- +RTS -K256M -A32M -RTS

if [ $? -eq 0 ]; then
    echo ""
    echo "=========================================="
    echo "Java kernel type sources generation complete!"
    echo "=========================================="
else
    echo ""
    echo "ERROR: Java kernel type sources generation failed"
    exit 1
fi
