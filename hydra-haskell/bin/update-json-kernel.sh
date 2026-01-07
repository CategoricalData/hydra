#!/usr/bin/env bash
# Wrapper script to generate kernel modules to JSON

set -e

cd "$(dirname "$0")/.."

echo "Building update-json-kernel executable..."
stack build hydra:update-json-kernel

echo ""
echo "Running update-json-kernel..."
stack exec update-json-kernel
