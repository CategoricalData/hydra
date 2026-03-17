#!/bin/bash
# Clean output directory and copy static resources for a Haskell bootstrap target.
# This is host-language-independent: the same static resources are needed
# regardless of which host (Haskell, Java, Python) generated the code.
#
# Usage: ./setup-haskell-target.sh <output-dir>

set -e

OUTPUT_DIR="$1"
if [ -z "$OUTPUT_DIR" ]; then
    echo "Usage: $0 <output-dir>"
    exit 1
fi

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../../.." && pwd )"
HYDRA_HASKELL_DIR="$HYDRA_ROOT/hydra-haskell"
HASKELL_RESOURCES="$SCRIPT_DIR/../resources/haskell"

# Clean and create output directory
echo "Preparing output directory: $OUTPUT_DIR"
rm -rf "$OUTPUT_DIR"
mkdir -p "$OUTPUT_DIR"

# Copy static resources
echo "Copying static resources for Haskell target..."

# Build files
echo "  Copying build files..."
cp "$HASKELL_RESOURCES/stack.yaml" "$OUTPUT_DIR/"
cp "$HASKELL_RESOURCES/package.yaml" "$OUTPUT_DIR/"
cp "$HASKELL_RESOURCES/README.md" "$OUTPUT_DIR/"

# Hand-written source files (primitive libraries, DSL, sources, etc.)
echo "  Copying hand-written source files..."
mkdir -p "$OUTPUT_DIR/src/main/haskell"
cp -r "$HYDRA_HASKELL_DIR/src/main/haskell/Hydra" "$OUTPUT_DIR/src/main/haskell/"

# Copy ext modules from baseline, replacing any generated versions.
# Hand-written Sources modules (e.g. Templates.hs, Annotations.hs) import generated
# modules like Hydra.Sources.Decode.Core and Hydra.Sources.Encode.Core, and
# Staging/Yaml/Coder.hs imports Hydra.Ext.Org.Yaml.Model — all of which live in
# gen-main. Copying the full baseline gen-main ext tree ensures these are available.
echo "  Copying ext modules from baseline..."
HS_GEN="$OUTPUT_DIR/src/gen-main/haskell"
HS_BASELINE="$HYDRA_HASKELL_DIR/src/gen-main/haskell"
if [ -d "$HS_BASELINE/Hydra/Ext" ]; then
    mkdir -p "$HS_GEN/Hydra"
    rm -rf "$HS_GEN/Hydra/Ext"
    cp -r "$HS_BASELINE/Hydra/Ext" "$HS_GEN/Hydra/"
    echo "    Copied Hydra/Ext from baseline"
fi
# Copy Sources/Decode and Sources/Encode (generated DSL source modules imported by
# hand-written Sources modules like Templates.hs and Annotations.hs)
for src_dir in Decode Encode; do
    if [ -d "$HS_BASELINE/Hydra/Sources/$src_dir" ]; then
        mkdir -p "$HS_GEN/Hydra/Sources"
        rm -rf "$HS_GEN/Hydra/Sources/$src_dir"
        cp -r "$HS_BASELINE/Hydra/Sources/$src_dir" "$HS_GEN/Hydra/Sources/"
        echo "    Copied Hydra/Sources/$src_dir from baseline"
    fi
done

# Kernel test suite runner and its dependencies
echo "  Copying kernel test suite runner..."
mkdir -p "$OUTPUT_DIR/src/test/haskell"
cp "$HYDRA_HASKELL_DIR/src/test/haskell/Spec.hs" "$OUTPUT_DIR/src/test/haskell/"
mkdir -p "$OUTPUT_DIR/src/test/haskell/Hydra"
cp "$HYDRA_HASKELL_DIR/src/test/haskell/Hydra/TestSuiteSpec.hs" "$OUTPUT_DIR/src/test/haskell/Hydra/"
cp "$HYDRA_HASKELL_DIR/src/test/haskell/Hydra/TestUtils.hs" "$OUTPUT_DIR/src/test/haskell/Hydra/"
cp "$HYDRA_HASKELL_DIR/src/test/haskell/Hydra/ArbitraryCore.hs" "$OUTPUT_DIR/src/test/haskell/Hydra/"

# License (needed by cabal)
touch "$OUTPUT_DIR/LICENSE"

# Summary
STATIC_COUNT=$(find "$OUTPUT_DIR/src/main" -name "*.hs" 2>/dev/null | wc -l | tr -d ' ')
echo "  Static resources: $STATIC_COUNT files"
echo ""
