#!/usr/bin/env bash
# Wrapper script to generate generation tests

set -e

cd "$(dirname "$0")/.."

echo "Building generate-generation-tests executable..."
stack build hydra:update-generation-tests

echo ""
echo "Running generate-generation-tests..."
stack exec update-generation-tests
