#!/usr/bin/env bash
# #459 Layer 1 transform: JSON -> target language, scoped to a single package, via the
# JAVA host. Sibling to heads/haskell/bin/transform-json-to-target.sh, matching its CLI
# contract so assemble-distribution.sh scripts can call either behind --generator-host.
#
# Usage:
#   transform-json-to-target.sh <target> <pkg> [main|test] [--output <dir>]
#                               [--dist-json-root <dir>] [OPTIONS]
#
# Compiles hydra.TransformJsonToTarget (heads/java/target-driver/) against the PUBLISHED
# Java host + every published target-coder artifact from Maven Central, plus the local
# hydra-build overlay (not published; see target-driver/build.gradle) — no local Java
# head build required. This is the #459 counterpart to the Haskell bootstrap-from-json
# exec: one process, invoked per (package, source-set) pair.
#
# Target must be one of: java, python, scala, typescript, clojure, scheme, common-lisp,
#                        emacs-lisp, haskell.
#
# Extra flags forwarded: --include-dsls, --include-tests (test source-set implies this).
#
# Published-host / local-host fallback (#459, mirrors #370's --local-host shim for the
# DSL->JSON path): tries target-driver's PUBLISHED-artifact classpath first. If that fails
# to resolve (e.g. the #417 hostOverrides:local shim blocking a not-yet-republished
# hydra-java version), falls back AUTOMATICALLY to packages/hydra-java's local headsExtras
# classpath — the full local dist/java rollup, same shim :hydra-java:compileHeadsExtrasJava
# already uses for the DSL->JSON path (bin/update-java-json.sh --local-host). Unlike that
# script, no explicit flag is needed here: sync.sh callers don't know or care which mode
# is active, so detection is automatic and the fallback is logged to stderr.

set -euo pipefail

if [ $# -lt 2 ]; then
    echo "Usage: $0 <target> <package> [main|test] [OPTIONS]" >&2
    exit 1
fi

TARGET="$1"
PACKAGE="$2"
shift 2

SOURCE_SET="main"
if [ $# -gt 0 ] && [[ "$1" != --* ]]; then
    SOURCE_SET="$1"
    shift
fi

TEST_FLAG=""
if [ "$SOURCE_SET" = "test" ]; then
    TEST_FLAG="--include-tests"
fi

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_JAVA_HEAD="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_JAVA_HEAD/../.." && pwd )"

DIST_JSON_ROOT="$HYDRA_ROOT_DIR/dist/json"
OUTPUT=""
EXTRA_ARGS=()
while [ $# -gt 0 ]; do
    case "$1" in
        --output) OUTPUT="$2"; shift 2 ;;
        --dist-json-root) DIST_JSON_ROOT="$2"; shift 2 ;;
        *) EXTRA_ARGS+=("$1"); shift ;;
    esac
done
if [ -z "$OUTPUT" ]; then
    echo "Usage: $0 <target> <package> [main|test] --output <dir> [OPTIONS]" >&2
    exit 1
fi

cd "$HYDRA_JAVA_HEAD"

JAVA_CP=""
if ./gradlew --quiet -p target-driver classes >/dev/null 2>&1; then
    JAVA_CP=$(./gradlew --quiet -p target-driver printRuntimeClasspath | tail -1)
else
    echo "transform-json-to-target.sh: published-host classpath unresolvable" \
         "(target-driver); falling back to local headsExtras build" \
         "(#417 hostOverrides shim or similar — see #459)." >&2
    ./gradlew --quiet :hydra-java:compileHeadsExtrasJava >&2
    JAVA_CP=$(./gradlew --quiet :hydra-java:printHeadsExtrasRuntimeClasspath | tail -1)
fi

# -Xss large for deeply nested type inference; -Xmx large for many bindings. Mirrors
# update-java-json.sh's JVM flags for the DSL->JSON path.
java -Xss64m -Xmx8g -cp "$JAVA_CP" hydra.TransformJsonToTarget \
    --target "$TARGET" --package "$PACKAGE" \
    --dist-json-root "$DIST_JSON_ROOT" --output "$OUTPUT" \
    $TEST_FLAG "${EXTRA_ARGS[@]+"${EXTRA_ARGS[@]}"}"
