#!/usr/bin/env bash
# Overlay the hand-written Haskell distribution-package source from the top-level
# overlay/haskell/ tree onto dist/haskell/ (#418). This is the Haskell analog of
# Java/Python's copy-kernel-runtime.sh.
#
# The overlay trees are copies, not patches: they add package source under dist/,
# they do not edit any generated file. Canonical homes are the uncompiled trees
# under overlay/haskell/ (a sibling of dist/, packages/, heads/).
#
# WHY this is a standalone step run BEFORE any `stack build`: the head compiles
# dist/haskell/hydra-kernel/ (a source-dir in package.yaml) but does NOT compile
# overlay/haskell/hydra-kernel/. The dist Lib/ location is gitignored and empty on
# a cold tree, so the overlaid modules (Hydra.Haskell.Lib.*, the umbrella Hydra.hs,
# Hydra.Dsl.*) must be copied into dist/haskell/ BEFORE the executables are built —
# otherwise GHC can't find them. bin/sync.sh's Phase 0 builds the execs first, so
# it invokes this script first. sync-haskell.sh also calls it (idempotent cp -R).
set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT_DIR="$( cd "$SCRIPT_DIR/../../.." && pwd )"

echo "  Overlaying hand-written distribution-package source onto dist/haskell/..."

# hydra-kernel runtime: MERGE onto the generated kernel dist (it shares the
# Hydra.Dsl.* namespace with generated modules, so merge — never wipe — the dir).
KERNEL_RUNTIME_SRC="$HYDRA_ROOT_DIR/overlay/haskell/hydra-kernel/src/main/haskell"
KERNEL_DST="$HYDRA_ROOT_DIR/dist/haskell/hydra-kernel/src/main/haskell"
mkdir -p "$KERNEL_DST"
cp -R "$KERNEL_RUNTIME_SRC"/. "$KERNEL_DST/"
echo "    hydra-kernel: overlaid $(find "$KERNEL_RUNTIME_SRC" -name '*.hs' | wc -l | tr -d ' ') hand-written runtime module(s)"

# hydra umbrella: its own dedicated package dir (no generated content to merge).
UMBRELLA_SRC="$HYDRA_ROOT_DIR/overlay/haskell/hydra/src/main/haskell"
UMBRELLA_DST="$HYDRA_ROOT_DIR/dist/haskell/hydra/src/main/haskell"
mkdir -p "$UMBRELLA_DST"
rm -rf "$UMBRELLA_DST"/*
cp -R "$UMBRELLA_SRC"/. "$UMBRELLA_DST/"
echo "    hydra: copied $(find "$UMBRELLA_DST" -name '*.hs' | wc -l | tr -d ' ') module(s)"
