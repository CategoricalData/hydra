#!/usr/bin/env bash
# Wrapper script to generate the JSON manifest (module namespace lists)

set -e

cd "$(dirname "$0")/.."

echo "Building update-json-manifest executable..."
stack build hydra:update-json-manifest

echo ""
echo "Running update-json-manifest..."
stack exec update-json-manifest
