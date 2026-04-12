#!/usr/bin/env bash
# Wrapper script to verify kernel modules JSON export consistency

set -e

cd "$(dirname "$0")/.."

echo "Building verify-json-kernel executable..."
stack build hydra:verify-json-kernel

echo ""
echo "Running verify-json-kernel..."
stack exec verify-json-kernel
