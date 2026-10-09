#!/usr/bin/env bash
# Wrapper script to generate test modules to JSON
#
# NOT DEAD CODE: no automated callers by design; documented manual entry point
# (docs/recipes/json-kernel.md § "Export test modules to JSON"), the test-module
# companion to update-json-main.sh. See docs/build-system.md § "Build & sync scripts". (#714)

set -e

cd "$(dirname "$0")/.."

echo "Building update-json-test executable..."
stack build hydra:update-json-test

echo ""
echo "Running update-json-test..."
stack exec update-json-test -- --output-dir "$(pwd)/../../dist/json/hydra-kernel/src/test/json"
