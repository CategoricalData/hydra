#!/usr/bin/env bash
# Wrapper script to generate every package's main modules to JSON.
# Writes fanned-out output to dist/json/<package>/src/main/json/ based on
# the routing table in Hydra.PackageRouting.

set -e

cd "$(dirname "$0")/.."

echo "Building update-json-main executable..."
stack build hydra:update-json-main

echo ""
echo "Running update-json-main..."
stack exec update-json-main -- --dist-root "$(pwd)/../../dist/json"
