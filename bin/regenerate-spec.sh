#!/usr/bin/env bash
set -euo pipefail

# Regenerate the generated module-reference pages under
# docs/specification/{primitives,types}/ from the current kernel (#723).
#
# Java-host by default (contrast bin/regenerate-lexicon.sh, which runs the
# Haskell host via `stack ghci`; #723 moves generation logic off Haskell as
# anything but an alternate host). The generation logic itself is translingual
# (hydra.codegen's generateModuleDoc, alongside generateLexicon); this script
# is the thin Java driver + I/O shell around it, invoking hydra.RegenerateSpec.
#
# Usage:
#   ./bin/regenerate-spec.sh [--module <name>]...
#
# With no --module flags, regenerates every kernel module that has a
# committed page under docs/specification/{primitives,types}/ (the page-name
# mapping is config-driven inside hydra.RegenerateSpec, not this script).
# One or more --module flags scope regeneration to specific kernel modules
# (e.g. --module hydra.lib.lists), for iterating on a single page.
#
# Prerequisites: the Java head buildable (./gradlew tasks from heads/java),
# and dist/json populated (run bin/sync.sh or bin/sync-java.sh first).
#
# Failure handling: this script must FAIL LOUDLY when the driver exits
# non-zero or a targeted page is not actually rewritten — see
# bin/regenerate-lexicon.sh's header comment for the incident (a swallowed
# `|| true` once let a broken generator leave stale pages in place while
# exiting 0, silently defeating prepare-release.sh's freshness gate).

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/.." && pwd )"

MODULE_ARGS=()
while [ $# -gt 0 ]; do
    case "$1" in
        --module) MODULE_ARGS+=(--module "$2"); shift 2 ;;
        *) echo "Usage: $0 [--module <name>]..." >&2; exit 1 ;;
    esac
done

if [ ! -d "$HYDRA_ROOT/dist/json/hydra-kernel" ]; then
    echo "ERROR: dist/json/hydra-kernel not found. Run bin/sync.sh (or bin/sync-java.sh)" >&2
    echo "       first to populate dist/json." >&2
    exit 1
fi

cd "$HYDRA_ROOT/heads/java"

echo "Regenerating docs/specification/{primitives,types}/ pages..."

# headsExtras compiles hydra/RegenerateSpec.java + hydra.codegen's generated
# Java form (hydra.Codegen), matching the classpath-resolution mechanics
# already used by bin/inference-bench.sh for a same-shape driver (a Java
# main class reading dist/json and writing output files, no published/local
# host-shim complexity needed here since this driver never leaves the local
# checkout).
./gradlew --quiet :hydra-java:compileHeadsExtrasJava
JAVA_CP=$(./gradlew --quiet :hydra-java:printHeadsExtrasRuntimeClasspath)

LOG_FILE="$(mktemp)"
trap 'rm -f "$LOG_FILE"' EXIT

if ! HYDRA_ROOT="$HYDRA_ROOT" java -Xss64m -Xmx4g -cp "$JAVA_CP" hydra.RegenerateSpec \
        "${MODULE_ARGS[@]+"${MODULE_ARGS[@]}"}" \
        > "$LOG_FILE" 2>&1; then
    echo "ERROR: hydra.RegenerateSpec exited non-zero. Last 30 lines of output:" >&2
    tail -30 "$LOG_FILE" >&2
    exit 1
fi

# Positive confirmation, mirroring regenerate-lexicon.sh's explicit
# success-line check: a driver that loads but silently no-ops (e.g. finds
# zero modules to regenerate after a namespace typo) must not report success.
if ! grep -q "^Wrote [0-9]* page" "$LOG_FILE"; then
    echo "ERROR: no pages were written (no 'Wrote N page(s)' line in output)." >&2
    echo "Last 30 lines of output:" >&2
    tail -30 "$LOG_FILE" >&2
    exit 1
fi

grep -E "^(Wrote|Converged|Skipped)" "$LOG_FILE" || true
echo "Done."
