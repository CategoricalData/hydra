#!/usr/bin/env bash
# Wrapper script to generate main modules to JSON

set -e

cd "$(dirname "$0")/.."

echo "Building update-json-main executable..."
stack build hydra:update-json-main

echo ""
echo "Running update-json-main..."
stack exec update-json-main -- --output-dir "$(pwd)/../../dist/json/hydra-kernel/src/main/json"
