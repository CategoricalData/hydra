#!/bin/bash
# Clean output directory and copy static resources for a Haskell bootstrap target.
# This is host-language-independent: the same static resources are needed
# regardless of which host (Haskell, Java, Python) generated the code.
#
# Usage: ./setup-haskell-target.sh [--clean] <output-dir>
#
# By default the prep is incremental: if every input the script reads from is
# byte-identical to the last successful prep recorded under
# <output-dir>/.bootstrap-prep-hash, the script exits early and skips the
# wipe-and-overlay. Pass --clean (or set HYDRA_BOOTSTRAP_CLEAN=1) to force a
# full rm -rf + recopy — needed for cold-start timing measurements in
# benchmark runs.

set -e

CLEAN=false
if [ "${HYDRA_BOOTSTRAP_CLEAN:-}" = "1" ]; then
    CLEAN=true
fi

OUTPUT_DIR=""
while [ $# -gt 0 ]; do
    case "$1" in
        --clean) CLEAN=true; shift ;;
        --no-clean) CLEAN=false; shift ;;
        --) shift; break ;;
        --*) echo "Unknown flag: $1" >&2; exit 1 ;;
        *)  OUTPUT_DIR="$1"; shift ;;
    esac
done

if [ -z "$OUTPUT_DIR" ]; then
    echo "Usage: $0 [--clean] <output-dir>"
    exit 1
fi

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../.." && pwd )"
HYDRA_HASKELL_DIR="$HYDRA_ROOT/packages/hydra-haskell"
HYDRA_KERNEL_DIR="$HYDRA_ROOT/packages/hydra-kernel"
# #546: hydra-build owns Hydra.Sources.Build.Test.* (relocated out of hydra-kernel).
# The kernel's Hydra.Sources.Test.TestSuite still imports them (Option-A stopgap,
# TODO(#547)), so the bootstrap cell must include hydra-build's DSL sources too.
HYDRA_BUILD_DIR="$HYDRA_ROOT/packages/hydra-build"
HYDRA_HASKELL_HEAD_DIR="$HYDRA_ROOT/heads/haskell"
HASKELL_RESOURCES="$SCRIPT_DIR/../resources/haskell"

source "$HYDRA_ROOT/bin/lib/common.sh"

# #670: keep the demo's Hackage dependency list in sync with the head's
# heads/haskell/package.yaml (the hand-maintained root). Runs before the
# cache hash below so drift in the head's deps invalidates the cache too
# (HASKELL_RESOURCES is one of the hashed paths).
python3 "$HYDRA_ROOT/bin/lib/sync-haskell-bootstrap-demo-deps.py"

# Compute a cache hash over every path the overlay sequence below reads from,
# plus this script itself. A change to any of those invalidates the cache
# and triggers a full wipe-and-overlay.
PREP_HASH=$(
    {
        find "$HYDRA_HASKELL_HEAD_DIR/src/main/haskell/Hydra" \
             "$HYDRA_ROOT/overlay/haskell/hydra-kernel/src/main/haskell/Hydra" \
             "$HYDRA_KERNEL_DIR/src/main/haskell/Hydra" \
             "$HYDRA_HASKELL_DIR/src/main/haskell/Hydra" \
             "$HYDRA_BUILD_DIR/src/main/haskell/Hydra" \
             "$HYDRA_ROOT/dist/haskell/hydra-kernel/src/main/haskell/Hydra/Core/Decode" \
             "$HYDRA_ROOT/dist/haskell/hydra-kernel/src/main/haskell/Hydra/Core/Encode" \
             "$HYDRA_ROOT/dist/haskell/hydra-kernel/src/main/haskell/Hydra/Core/Dsl" \
             "$HYDRA_ROOT/dist/haskell/hydra-kernel/src/main/haskell/Hydra/Core/Lib" \
             "$HYDRA_ROOT/dist/haskell/hydra-haskell/src/main/haskell/Hydra/Haskell/Dsl" \
             "$HYDRA_ROOT/dist/haskell/hydra-build/src/main/haskell/Hydra/Build" \
             "$HYDRA_HASKELL_HEAD_DIR/src/test/haskell" \
             "$HASKELL_RESOURCES" \
             -type f 2>/dev/null
        echo "$HYDRA_ROOT/dist/haskell/hydra-kernel/src/main/haskell/Hydra/Core/Dsls.hs"
        echo "$HYDRA_ROOT/dist/haskell/hydra-kernel/src/test/haskell/Hydra/Core/Test/TestEnv.hs"
        echo "${BASH_SOURCE[0]}"
    } | LC_ALL=C sort | xargs shasum -a 256 2>/dev/null | shasum -a 256 | awk '{print $1}'
)
PREP_CACHE="$OUTPUT_DIR/.bootstrap-prep-hash"

if [ "$CLEAN" = false ] && step_cache_hit "$PREP_CACHE" "$PREP_HASH"; then
    echo "Output directory already prepared (cache hit): $OUTPUT_DIR"
    echo "  Pass --clean (or HYDRA_BOOTSTRAP_CLEAN=1) to force a fresh wipe + recopy."
    exit 0
fi

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
# Hand-written Haskell runtime now lives in heads/haskell. Kernel and Haskell-coder
# DSL source modules live under packages/hydra-kernel and packages/hydra-haskell,
# but everything must land in a single src/main/haskell tree in the bootstrap output.
echo "  Copying hand-written source files..."
mkdir -p "$OUTPUT_DIR/src/main/haskell"
# Base: the Haskell head's hand-written runtime (Hydra.Generation,
# Hydra.Haskell.Generation, Hydra.Module.*, Hydra.Tools.*, the exec drivers, ...)
cp -r "$HYDRA_HASKELL_HEAD_DIR/src/main/haskell/Hydra" "$OUTPUT_DIR/src/main/haskell/"
# Overlay the hand-written kernel runtime that was relocated out of the head into
# the top-level overlay/ tree (#418): Hydra.Settings, Hydra.Kernel,
# Hydra.Haskell.Lib.*, Hydra.Core.Dsl.{Terms,Literals,Meta.Common}. The bootstrap
# output is a single flat tree, so these must be stitched in here just as
# sync-haskell.sh overlays them onto dist/haskell/hydra-kernel/.
cp -r "$HYDRA_ROOT/overlay/haskell/hydra-kernel/src/main/haskell/Hydra/." "$OUTPUT_DIR/src/main/haskell/Hydra/"
# Remove ext-related sources not needed for the bootstrap target
rm -f "$OUTPUT_DIR/src/main/haskell/Hydra/ExtGeneration.hs"
rm -f "$OUTPUT_DIR/src/main/haskell/Hydra/Sources/Ext.hs"
# Hydra.TargetFilePaths is only used by the bootstrap-from-json exec
# (which the demo invokes from the parent project, not from this copy),
# and hard-imports Hydra.Java.Coder — absent from this demo's tree since
# only the Haskell coder is copied. Strip it so the demo library builds.
rm -f "$OUTPUT_DIR/src/main/haskell/Hydra/TargetFilePaths.hs"
# Hydra.Coq.GenerateDriver imports the DSL-generated Hydra.Coq.Generate module,
# which lives under dist/haskell/hydra-ext/ and is not part of the kernel
# bootstrap. Drop the whole directory — the Coq target is not reachable
# from the Haskell bootstrap pipeline.
rm -rf "$OUTPUT_DIR/src/main/haskell/Hydra/Coq"
# Overlay kernel DSL sources (Hydra.Sources.Kernel.*, Hydra.Sources.Decode.*, Hydra.Sources.Encode.*, ...)
if [ -d "$HYDRA_KERNEL_DIR/src/main/haskell/Hydra" ]; then
    cp -r "$HYDRA_KERNEL_DIR/src/main/haskell/Hydra/." "$OUTPUT_DIR/src/main/haskell/Hydra/"
fi
# Overlay Haskell-coder DSL sources (Hydra.Sources.Haskell.*, Hydra.Sources.All)
if [ -d "$HYDRA_HASKELL_DIR/src/main/haskell/Hydra" ]; then
    cp -r "$HYDRA_HASKELL_DIR/src/main/haskell/Hydra/." "$OUTPUT_DIR/src/main/haskell/Hydra/"
fi
# Overlay hydra-build DSL sources (Hydra.Sources.Build.Test.{Modules,Reconcile,Routing}).
# #546 relocated these out of hydra-kernel; the kernel Hydra.Sources.Test.TestSuite still
# imports them (Option-A stopgap, TODO(#547)), so without this the cell's TestSuite fails
# with "Could not find module 'Hydra.Sources.Build.Test.Reconcile'".
if [ -d "$HYDRA_BUILD_DIR/src/main/haskell/Hydra" ]; then
    cp -r "$HYDRA_BUILD_DIR/src/main/haskell/Hydra/." "$OUTPUT_DIR/src/main/haskell/Hydra/"
fi

# Copy ext modules from baseline. Hand-written Sources modules import generated
# modules like Hydra.Sources.Decode.Core and Hydra.Sources.Encode.Core which live in dist.
echo "  Copying ext modules from baseline..."
HS_GEN="$OUTPUT_DIR/src/main/haskell"
HS_KERNEL_BASELINE="$HYDRA_ROOT/dist/haskell/hydra-kernel/src/main/haskell"
HS_HASKELL_BASELINE="$HYDRA_ROOT/dist/haskell/hydra-haskell/src/main/haskell"
HS_BUILD_BASELINE="$HYDRA_ROOT/dist/haskell/hydra-build/src/main/haskell"
# #546: the generated test modules Hydra.Core.Test.Build.* import the generated runtime
# Hydra.Build.{Modules,Reconcile,Routing}, which live in the hydra-build baseline (not
# hydra-kernel) and aren't emitted into the cell under --kernel-only. Copy them so the
# generated Test/Build/*.hs resolve (else: "Could not find module 'Hydra.Build.Modules'").
if [ -d "$HS_BUILD_BASELINE/Hydra/Build" ]; then
    mkdir -p "$HS_GEN/Hydra/Build"
    cp -r "$HS_BUILD_BASELINE/Hydra/Build/." "$HS_GEN/Hydra/Build/"
    echo "    Copied Hydra/Build from hydra-build baseline"
fi
# #512: Hydra.DigestFormat (in hydra-haskell's baseline copy above) imports the typed
# digest codec's generated Decode/Encode modules (Hydra.Build.Decode.Format,
# Hydra.Build.Encode.Format), which — like Hydra.Build.* above — live in the hydra-build
# baseline, not hydra-kernel's Core/{Decode,Encode} copied below. Without this, the cell
# fails with "Could not find module 'Hydra.Build.Decode.Format'".
for build_codec_dir in Decode Encode; do
    if [ -d "$HS_BUILD_BASELINE/Hydra/Build/$build_codec_dir" ]; then
        mkdir -p "$HS_GEN/Hydra/Build/$build_codec_dir"
        cp -r "$HS_BUILD_BASELINE/Hydra/Build/$build_codec_dir/." "$HS_GEN/Hydra/Build/$build_codec_dir/"
        echo "    Copied Hydra/Build/$build_codec_dir from hydra-build baseline"
    fi
done
# Copy Core/Decode and Core/Encode (generated DSL source modules imported by
# hand-written modules like Templates.hs and Annotations.hs, e.g. Hydra.Core.Decode.Model).
# #729 dropped the Sources segment from these module names (was Hydra.Sources.Decode.*).
for src_dir in Decode Encode; do
    if [ -d "$HS_KERNEL_BASELINE/Hydra/Core/$src_dir" ]; then
        mkdir -p "$HS_GEN/Hydra/Core"
        rm -rf "$HS_GEN/Hydra/Core/$src_dir"
        cp -r "$HS_KERNEL_BASELINE/Hydra/Core/$src_dir" "$HS_GEN/Hydra/Core/"
        echo "    Copied Hydra/Core/$src_dir from hydra-kernel baseline"
    fi
done
# Copy generated DSL modules (Hydra.Core.Dsl.Model, Hydra.Core.Dsl.Graph, etc.) imported by
# hand-written Hydra.Core.Dsl.Meta.* modules. Overlay on top of heads/haskell Dsl
# (not replacing it) so hand-written Dsl files like Hydra.Core.Dsl.Terms are preserved.
if [ -d "$HS_KERNEL_BASELINE/Hydra/Core/Dsl" ]; then
    mkdir -p "$HS_GEN/Hydra/Core/Dsl"
    cp -r "$HS_KERNEL_BASELINE/Hydra/Core/Dsl/." "$HS_GEN/Hydra/Core/Dsl/"
    echo "    Overlaid Hydra/Core/Dsl from hydra-kernel baseline"
fi
# Overlay Haskell-coder Dsl wrappers from hydra-haskell (own Hydra.Haskell.* namespace,
# not nested under Core -- that segment is specific to the hydra-kernel package).
if [ -d "$HS_HASKELL_BASELINE/Hydra/Haskell/Dsl" ]; then
    mkdir -p "$HS_GEN/Hydra/Haskell/Dsl"
    cp -r "$HS_HASKELL_BASELINE/Hydra/Haskell/Dsl/." "$HS_GEN/Hydra/Haskell/Dsl/"
    echo "    Overlaid Hydra/Haskell/Dsl (Haskell coder wrappers) from hydra-haskell baseline"
fi
# Copy generated Hydra.Core.Lib.* modules (Hydra.Core.Lib.Eithers, Hydra.Core.Lib.Lists, ...).
# Since #473 relocated the Haskell authoring DSL cluster into overlay/, the
# hand-written Hydra.Core.Dsl.{Libraries,Meta.Phantoms,Meta.Lib.*} modules import
# these generated definition modules. They live in the kernel baseline alongside
# Hydra/Core/Dsl, but were never stitched into the single-tree bootstrap output, so
# GHC failed with "Could not find module 'Hydra.Core.Lib.Eithers'". Copy them in,
# parallel to the Hydra/Core/Dsl overlay above.
if [ -d "$HS_KERNEL_BASELINE/Hydra/Core/Lib" ]; then
    mkdir -p "$HS_GEN/Hydra/Core"
    cp -r "$HS_KERNEL_BASELINE/Hydra/Core/Lib" "$HS_GEN/Hydra/Core/"
    echo "    Copied Hydra/Core/Lib from hydra-kernel baseline"
fi
# Copy Hydra.Core.Dsls (DSL source generator module). It is generated separately from
# mainModules due to stack overflow issues, so it won't be produced by the bootstrap
# code generator. It is imported by hand-written Generation.hs.
if [ -f "$HS_KERNEL_BASELINE/Hydra/Core/Dsls.hs" ]; then
    mkdir -p "$HS_GEN/Hydra/Core"
    cp "$HS_KERNEL_BASELINE/Hydra/Core/Dsls.hs" "$HS_GEN/Hydra/Core/"
    echo "    Copied Hydra/Core/Dsls.hs from hydra-kernel baseline"
fi

# Kernel test suite runner and its dependencies (now in heads/haskell)
echo "  Copying kernel test suite runner..."
mkdir -p "$OUTPUT_DIR/src/test/haskell"
cp "$HYDRA_HASKELL_HEAD_DIR/src/test/haskell/Spec.hs" "$OUTPUT_DIR/src/test/haskell/"
mkdir -p "$OUTPUT_DIR/src/test/haskell/Hydra"
cp "$HYDRA_HASKELL_HEAD_DIR/src/test/haskell/Hydra/TestSuiteSpec.hs" "$OUTPUT_DIR/src/test/haskell/Hydra/"
cp "$HYDRA_HASKELL_HEAD_DIR/src/test/haskell/Hydra/TestUtils.hs" "$OUTPUT_DIR/src/test/haskell/Hydra/"
cp "$HYDRA_HASKELL_HEAD_DIR/src/test/haskell/Hydra/ArbitraryCore.hs" "$OUTPUT_DIR/src/test/haskell/Hydra/"

# TestEnv: provides the real test graph with primitives and kernel term bindings.
# Must be copied to src/test (where the generated TestGraph.hs imports it).
mkdir -p "$OUTPUT_DIR/src/test/haskell/Hydra/Core/Test"
cp "$HYDRA_ROOT/dist/haskell/hydra-kernel/src/test/haskell/Hydra/Core/Test/TestEnv.hs" "$OUTPUT_DIR/src/test/haskell/Hydra/Core/Test/"

# License (needed by cabal)
touch "$OUTPUT_DIR/LICENSE"

# Summary
STATIC_COUNT=$(find "$OUTPUT_DIR/src/main" -name "*.hs" 2>/dev/null | wc -l | tr -d ' ')
echo "  Static resources: $STATIC_COUNT files"
echo ""

# Record the input hash so the next invocation can short-circuit when nothing
# under the source paths above has changed.
step_cache_record "$PREP_CACHE" "$PREP_HASH"
