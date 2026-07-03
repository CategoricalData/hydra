#!/usr/bin/env bash
# Overlay the hand-written Go kernel runtime onto dist/go/hydra-kernel/ so the
# Go distribution package is self-contained (#434).
#
# The runtime's canonical home is the top-level overlay tree
# overlay/go/hydra-kernel/src/main/go/ (a sibling of dist/, packages/,
# heads/ — see docs/build-system.md). It holds the hand-written hydra/lib/*
# primitive implementations (today only lib/literals is implemented; the rest are
# package stubs) — the Go coder emits the kernel modules (hydra/core, hydra/errors,
# …) but NOT the hydra.lib.* primitive impls, exactly as for Java/Python. The
# generated Go imports the runtime via the dist-local module path
# `hydra.dev/hydra/lib/...`, so it must resolve under the dist tree.
#
# This script is the ONLY reader of overlay/go/. The Go head consumes the dist
# copy, never overlay/ or heads/go for shipped runtime.
#
# It runs AFTER the JSON->Go transform's --prune-stale pass (the transform does
# not take a keep-paths manifest here), so the prune can't remove these files.
#
# Usage:
#   copy-kernel-runtime.sh [--dist-root <dir>]

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_GO_HEAD="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_GO_HEAD/../.." && pwd )"

DIST_ROOT="$HYDRA_ROOT_DIR/dist/go"

while [ $# -gt 0 ]; do
    case "$1" in
        --dist-root) DIST_ROOT="$2"; shift 2 ;;
        *) shift ;;
    esac
done

OVERLAY_DIR="$HYDRA_ROOT_DIR/overlay/go/hydra-kernel/src/main/go"
OUT_DIR="$DIST_ROOT/hydra-kernel/src/main/go"

if [ ! -d "$OVERLAY_DIR" ]; then
    echo "error: missing overlay dir $OVERLAY_DIR" >&2
    exit 1
fi

mkdir -p "$OUT_DIR/hydra"

# Merge the overlay tree onto the generated kernel dist. Trailing /. copies
# CONTENTS into the dest, leaving generated siblings untouched.
cp -R "$OVERLAY_DIR/." "$OUT_DIR/"

echo "  Overlaid hand-written Go kernel runtime from overlay/go/ into $OUT_DIR/hydra/"
