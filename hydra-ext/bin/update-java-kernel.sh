#!/bin/bash
set -e

# Script to regenerate the Java kernel from Hydra sources
# This script builds the update-java-kernel executable and runs it with
# proper RTS flags to avoid stack overflow during generation.

echo "=========================================="
echo "Updating Java Kernel"
echo "=========================================="
echo ""

# Get the directory where this script is located
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_EXT_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"

cd "$HYDRA_EXT_DIR"

echo "Building update-java-kernel executable..."
stack build hydra-ext:exe:update-java-kernel

if [ $? -ne 0 ]; then
    echo "ERROR: Build failed"
    exit 1
fi

echo ""
echo "Build successful!"
echo ""
echo "Generating Java kernel..."
echo "This may take several minutes and use significant memory."
echo ""

# Run with RTS flags to avoid stack overflow
# -K256M: Set stack size to 256MB
# -A32M: Set allocation area size to 32MB
stack exec update-java-kernel -- +RTS -K256M -A32M -RTS

if [ $? -eq 0 ]; then
    echo ""
    echo "=========================================="
    echo "Java kernel generation complete!"
    echo "=========================================="
else
    echo ""
    echo "ERROR: Java generation failed"
    exit 1
fi
