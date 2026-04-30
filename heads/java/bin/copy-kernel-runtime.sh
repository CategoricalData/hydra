#!/usr/bin/env bash
# Copy the hand-written Java runtime support from heads/java/src/main/java/
# into dist/java/hydra-kernel/src/main/java/ so the published hydra-kernel
# Maven artifact is self-contained.
#
# Per the 0.15 layout, hydra-kernel is special: it ships with not only the
# generated kernel source but also the runtime classes (hydra/{util,lib,
# dsl,json,tools}/, plus the top-level Adapters/Bootstrap/Coders/Generation/
# HydraTestBase) that every Hydra Java program needs.
#
# Cypher, GQL, and the RDF native binding belong in bindings/ once that
# subtree exists; they are explicitly NOT copied here.
#
# Usage:
#   copy-kernel-runtime.sh [--dist-root <dir>]

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_JAVA_HEAD="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_JAVA_HEAD/../.." && pwd )"

DIST_ROOT="$HYDRA_ROOT_DIR/dist/java"

while [ $# -gt 0 ]; do
    case "$1" in
        --dist-root) DIST_ROOT="$2"; shift 2 ;;
        *) shift ;;
    esac
done

SRC_DIR="$HYDRA_JAVA_HEAD/src/main/java"
OUT_DIR="$DIST_ROOT/hydra-kernel/src/main/java"

if [ ! -d "$SRC_DIR" ]; then
    echo "error: missing source dir $SRC_DIR" >&2
    exit 1
fi

mkdir -p "$OUT_DIR/hydra"

# Top-level kernel utility classes that the kernel itself needs at runtime.
#
# Adapters and Coders are pure kernel helpers (no cross-coder references).
#
# Bootstrap.java, Generation.java, and HydraTestBase.java are intentionally
# OMITTED from the published kernel artifact:
#   - Bootstrap.java and Generation.java are multi-coder driver classes that
#     import hydra.java.*, hydra.python.*, hydra.lisp.* — they depend on
#     every coder, so they live above the kernel layer (developer tooling,
#     not a kernel concern).
#   - HydraTestBase.java pulls in JUnit; it belongs in a separate test-utility
#     artifact, not a runtime artifact whose main jar would bring JUnit along.
# All three remain available under heads/java/src/main/java/ for the
# developer rollup and demos.
for f in Adapters.java Coders.java; do
    if [ -f "$SRC_DIR/hydra/$f" ]; then
        cp "$SRC_DIR/hydra/$f" "$OUT_DIR/hydra/$f"
    fi
done

# Kernel subpackages: util (Maybe/Either/etc.), lib (primitive impls),
# dsl (Java-side DSL helpers), json (parser/writer), tools (Function3/4 etc.).
#
# We MERGE the hand-written tree into the generated tree per file rather than
# removing-and-replacing each subdirectory wholesale. Several of these
# subdirectories overlap with generated content (e.g. hydra/json/model/Value.java
# is generated; hydra/json/JsonDecoding.java is hand-written), and
# `rm -rf <sub> && cp -R` would clobber the generated children.
for sub in util lib dsl json tools; do
    if [ -d "$SRC_DIR/hydra/$sub" ]; then
        mkdir -p "$OUT_DIR/hydra/$sub"
        # cp -R from inside the source dir copies CONTENTS into the dest,
        # leaving generated siblings under hydra/<sub>/ untouched.
        # Trailing /. on the source is portable across BSD and GNU cp.
        cp -R "$SRC_DIR/hydra/$sub/." "$OUT_DIR/hydra/$sub/"
    fi
done

# Excluded files that depend on third-party Maven artifacts the kernel
# does not (and shouldn't) bring along:
#   - tools/AntlrReaderBase.java       — needs ANTLR runtime
#   - json/JsonIoCoder.java            — needs com.cedarsoftware.util.io (json-io)
#   - json/JsonSerde.java              — needs json-io
# The JsonIoCoder/JsonSerde layer belongs in a future bindings/java/hydra-jsonio
# package; cypher/gql/RDF native bindings similarly live elsewhere.
rm -f "$OUT_DIR/hydra/tools/AntlrReaderBase.java"
rm -f "$OUT_DIR/hydra/json/JsonIoCoder.java"
rm -f "$OUT_DIR/hydra/json/JsonSerde.java"

echo "  Copied hand-written Java runtime into $OUT_DIR/hydra/"
