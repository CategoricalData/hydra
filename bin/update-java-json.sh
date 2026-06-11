#!/usr/bin/env bash
# Update dist/json/hydra-java from the Java DSL sources.
#
# Thin wrapper around hydra.UpdateJavaJson: builds a classpath holding the
# hydra-java DSL authoring sources + the driver, then invokes the driver to
# regenerate dist/json/hydra-java. Mirrors bin/update-python-json.py on the
# Python side.
#
# Two classpath modes (#370):
#
#   --published-host  (DEFAULT)  Compile the DSL sources + driver against the
#                                PUBLISHED Java host
#                                net.fortytwo.hydra:hydra-java:<hostVersion>
#                                (resolved from Maven via hydra.json's
#                                hostVersion / hostVersionOverrides) in the
#                                standalone heads/java/json-driver project. No
#                                local Java host build is required — edits to the
#                                DSL sources flow straight through. This is the
#                                path the regular sync uses.
#
#   --local-host                 BOOTSTRAP SHIM. Compile + run via the local
#                                rollup's headsExtras source set
#                                (:hydra-java:compileHeadsExtrasJava), i.e. the
#                                full local dist/java build. Use this only when a
#                                backward-INCOMPATIBLE kernel change means the
#                                last published host cannot handle the new data
#                                yet; build a local interim host here, then
#                                publish it (mavenLocal) and bump
#                                hydra.json:hostVersion(Overrides) so the default
#                                published-host path picks it up.
#
# Originally introduced for issue #344 as the "Java self-host demo"; now the
# canonical Java DSL → JSON step in the regular sync pipeline (Phase 5).
#
# Usage:
#   bin/update-java-json.sh                    # default: published-host
#   bin/update-java-json.sh --local-host       # bootstrap shim (local build)
#   bin/update-java-json.sh --out-root DIR     # override output
#   bin/update-java-json.sh --help
#
# Additional --out-root semantics: the per-package driver writes under
# <dist-json-root>/hydra-java/src/main/json/. When --out-root names the
# four-segment ".../hydra-java/src/main/json" tail, the driver strips
# that tail to recover the dist-json root and writes back through it.
# Non-canonical paths (e.g. /tmp targets) are treated verbatim.
#
# This script assumes its prerequisites (kernel JSON in dist/json/hydra-kernel/,
# packages/hydra-java/src/main/java/hydra/sources/java/) are already current.
# For the full prep-and-update flow with sync-on-demand and optional --compare
# against the Haskell-generated canonical, use bin/generate-hydra-java-from-java.sh.

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/.." && pwd )"

OUT_ROOT="$HYDRA_ROOT/dist/json/hydra-java/src/main/json"
MODE="published-host"
EXTRA_ARGS=()

while [ $# -gt 0 ]; do
    case "$1" in
        --out-root) OUT_ROOT="$2"; shift 2 ;;
        --out-root=*) OUT_ROOT="${1#--out-root=}"; shift ;;
        --published-host) MODE="published-host"; shift ;;
        --local-host) MODE="local-host"; shift ;;
        --help|-h)
            sed -n '2,/^$/p' "$0" | sed 's/^# \{0,1\}//'
            exit 0 ;;
        *) EXTRA_ARGS+=("$1"); shift ;;
    esac
done

cd "$HYDRA_ROOT/heads/java"

if [ "$MODE" = "published-host" ]; then
    HOST_VERSION="$(python3 "$HYDRA_ROOT/bin/lib/hydra-packages.py" host-version hydra-java)"
    echo "=== Compiling DSL sources + driver against published host (hydra-java:$HOST_VERSION) ==="
    ./gradlew --quiet -p json-driver classes

    echo "=== Resolving classpath ==="
    JAVA_CP=$(./gradlew --quiet -p json-driver printRuntimeClasspath | tail -1)
else
    echo "=== Compiling Java head (local rollup, bootstrap shim) ==="
    ./gradlew --quiet :hydra-java:compileHeadsExtrasJava

    echo "=== Resolving classpath ==="
    JAVA_CP=$(./gradlew --quiet :hydra-java:printHeadsExtrasRuntimeClasspath | tail -1)
fi

echo "=== Running UpdateJavaJson ($MODE) ==="
# -Xss large for deeply nested type inference; -Xmx large for many bindings.
java -Xss64m -Xmx8g -cp "$JAVA_CP" hydra.UpdateJavaJson \
    --hydra-root "$HYDRA_ROOT" --out-root "$OUT_ROOT" \
    "${EXTRA_ARGS[@]+"${EXTRA_ARGS[@]}"}"
