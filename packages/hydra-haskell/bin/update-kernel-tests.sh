#!/usr/bin/env bash
# Wrapper script to generate kernel tests

set -e

cd "$(dirname "$0")/.."

echo "Building update-kernel-tests executable..."
stack build hydra:update-kernel-tests

echo ""
echo "Running update-kernel-tests..."
stack exec update-kernel-tests
