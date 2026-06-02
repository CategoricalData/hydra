#!/usr/bin/env bash
# Update dist/json/hydra-java from the Java DSL sources.
#
# Thin wrapper around hydra.UpdateJavaJson: compiles the Java rollup,
# resolves the runtime classpath, and invokes the driver. Mirrors
# bin/update-python-json.py on the Python side.
#
# Originally introduced for issue #344 as the "Java self-host demo"; now
# the canonical Java DSL → JSON step in the regular sync pipeline
# (Phase 5).
#
# Usage:
#   bin/update-java-json.sh                   # default settings
#   bin/update-java-json.sh --out-root DIR    # override output
#   bin/update-java-json.sh --help
#
# Additional --out-root semantics: the per-package driver writes under
# <dist-json-root>/hydra-java/src/main/json/. When --out-root names the
# four-segment ".../hydra-java/src/main/json" tail, the driver strips
# that tail to recover the dist-json root and writes back through it.
# Non-canonical paths (e.g. /tmp targets) are treated verbatim.
#
# This script assumes its prerequisites (kernel JSON, dist/java/hydra-java,
# packages/hydra-java/src/main/java/hydra/sources/java/) are already
# current. For the full prep-and-update flow with sync-on-demand and
# optional --compare against the Haskell-generated canonical, use
# bin/generate-hydra-java-from-java.sh.

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/.." && pwd )"

OUT_ROOT="$HYDRA_ROOT/dist/json/hydra-java/src/main/json"
EXTRA_ARGS=()

while [ $# -gt 0 ]; do
    case "$1" in
        --out-root) OUT_ROOT="$2"; shift 2 ;;
        --out-root=*) OUT_ROOT="${1#--out-root=}"; shift ;;
        --help|-h)
            sed -n '2,/^$/p' "$0" | sed 's/^# \{0,1\}//'
            exit 0 ;;
        *) EXTRA_ARGS+=("$1"); shift ;;
    esac
done

cd "$HYDRA_ROOT/heads/java"

echo "=== Compiling Java head (rollup) ==="
./gradlew --quiet :hydra-java:compileHeadsExtrasJava

echo "=== Resolving classpath ==="
JAVA_CP=$(./gradlew --quiet :hydra-java:printHeadsExtrasRuntimeClasspath)

echo "=== Running UpdateJavaJson ==="
# -Xss large for deeply nested type inference; -Xmx large for many bindings.
java -Xss64m -Xmx8g -cp "$JAVA_CP" hydra.UpdateJavaJson \
    --hydra-root "$HYDRA_ROOT" --out-root "$OUT_ROOT" \
    "${EXTRA_ARGS[@]+"${EXTRA_ARGS[@]}"}"
