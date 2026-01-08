#!/bin/bash
set -e

# Script to generate Python kernel source modules from Hydra kernel modules.
# These are "source-level" representations of the kernel modules that contain
# the Module AST as data, rather than application-level Python code.
#
# This is needed for Python tests that need to access kernel module definitions
# as data (similar to how Haskell tests include kernelModules in the test graph).

echo "=========================================="
echo "Updating Python Kernel Sources"
echo "=========================================="
echo ""

# Get the directory where this script is located
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_EXT_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"

cd "$HYDRA_EXT_DIR"

echo "Building update-python-kernel-sources executable..."
stack build hydra-ext:exe:update-python-kernel-sources

if [ $? -ne 0 ]; then
    echo "ERROR: Build failed"
    exit 1
fi

echo ""
echo "Build successful!"
echo ""
echo "Generating Python kernel sources..."
echo "This may take several minutes and use significant memory."
echo ""

# Run with RTS flags to avoid stack overflow
# -K256M: Set stack size to 256MB
# -A32M: Set allocation area size to 32MB
stack exec update-python-kernel-sources -- +RTS -K256M -A32M -RTS

if [ $? -eq 0 ]; then
    echo ""
    echo "=========================================="
    echo "Python kernel sources generation complete!"
    echo "=========================================="
else
    echo ""
    echo "ERROR: Python kernel sources generation failed"
    exit 1
fi
