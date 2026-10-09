#!/usr/bin/env bash
# Wrapper script to verify kernel modules JSON export consistency
#
# NOT DEAD CODE: no automated callers by design. The documented manual "verify JSON
# round-trips" tool, referenced by 8 docs. See docs/build-system.md § "Build & sync
# scripts". (#714)

set -e

cd "$(dirname "$0")/.."

echo "Building verify-json-kernel executable..."
stack build hydra:verify-json-kernel

echo ""
echo "Running verify-json-kernel..."
stack exec verify-json-kernel
