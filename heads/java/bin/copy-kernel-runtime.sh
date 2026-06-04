#!/usr/bin/env bash
# Overlay the hand-written Java kernel runtime onto dist/java/hydra-kernel/ so the
# published hydra-kernel Maven artifact is self-contained.
#
# The runtime's canonical home is the top-level overlay tree
# overlay/java/hydra-kernel/src/main/java/ (a sibling of dist/, packages/, heads/,
# bindings/ — see docs/build-system.md). It holds exactly the hand-written classes
# the kernel needs at runtime (hydra/{util,lib,dsl,tools}/, hydra/json/{JsonEncoding,
# JsonDecoding}.java, and the top-level Adapters/Coders) — nothing else. Because the
# overlay tree contains ONLY runtime, this is a dumb full-tree merge: no selective
# file lists, no exclusions (#418). Compare the Haskell analog in sync-haskell.sh.
#
# Files that are NOT kernel runtime stay in heads/java/src/main/java/ and are
# compiled by the developer rollup's `headsExtras` source set, not copied here:
#   - multi-coder drivers: Bootstrap, Generation, BenchInference, ProfileJavaCoder,
#     UpdateJavaJson (import every coder; live above the kernel layer)
#   - HydraTestBase (pulls in JUnit; belongs in a test-utility artifact)
#   - json/JsonIoCoder, json/JsonSerde (need com.cedarsoftware json-io; a future
#     bindings/java/hydra-jsonio package)
#
# The merge (cp -R contents) leaves generated siblings under hydra/<sub>/ untouched.
#
# Usage:
#   copy-kernel-runtime.sh [--dist-root <dir>] [--manifest <file>]
#
# --manifest <file> appends '<OUT_DIR>\t<relPath>' lines (tab-separated) for
# every file under OUT_DIR. Consumed by bootstrap-from-json --keep-paths-from to
# protect hand-copied runtime files from --prune-stale (#357).

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_JAVA_HEAD="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_JAVA_HEAD/../.." && pwd )"

DIST_ROOT="$HYDRA_ROOT_DIR/dist/java"
MANIFEST_FILE=""

while [ $# -gt 0 ]; do
    case "$1" in
        --dist-root) DIST_ROOT="$2"; shift 2 ;;
        --manifest)  MANIFEST_FILE="$2"; shift 2 ;;
        *) shift ;;
    esac
done

OVERLAY_DIR="$HYDRA_ROOT_DIR/overlay/java/hydra-kernel/src/main/java"
OUT_DIR="$DIST_ROOT/hydra-kernel/src/main/java"

if [ ! -d "$OVERLAY_DIR" ]; then
    echo "error: missing overlay dir $OVERLAY_DIR" >&2
    exit 1
fi

mkdir -p "$OUT_DIR"

# Merge the entire overlay tree onto the generated kernel dist. Trailing /. on the
# source copies CONTENTS into the dest, leaving generated siblings untouched.
cp -R "$OVERLAY_DIR/." "$OUT_DIR/"

if [ -n "$MANIFEST_FILE" ]; then
    # Emit '<OUT_DIR>\t<relPath>' for every regular file currently under OUT_DIR.
    ( cd "$OUT_DIR" && find . -type f -print | sed 's|^\./||' \
        | awk -v dir="$OUT_DIR" '{ printf "%s\t%s\n", dir, $0 }' \
        >> "$MANIFEST_FILE" )
fi

echo "  Overlaid hand-written Java kernel runtime from overlay/java/ into $OUT_DIR/hydra/"
