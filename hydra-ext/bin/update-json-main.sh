#!/usr/bin/env bash
# Wrapper script to generate hydra-ext modules to JSON

set -e

cd "$(dirname "$0")/.."

echo "Building update-json-main executable..."
stack build hydra-ext:update-json-main

echo ""
echo "Running update-json-main..."
stack exec update-json-main
