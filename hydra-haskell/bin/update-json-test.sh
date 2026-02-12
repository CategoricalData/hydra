#!/usr/bin/env bash
# Wrapper script to generate test modules to JSON

set -e

cd "$(dirname "$0")/.."

echo "Building update-json-test executable..."
stack build hydra:update-json-test

echo ""
echo "Running update-json-test..."
stack exec update-json-test
