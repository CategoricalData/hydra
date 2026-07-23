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

# Ensure target-driver's classes are built (fast no-op if already up to date).
./gradlew --quiet -p target-driver classes >&2

JAVA_CP=$(./gradlew --quiet -p target-driver printRuntimeClasspath | tail -1)

# -Xss large for deeply nested type inference; -Xmx large for many bindings. Mirrors
# update-java-json.sh's JVM flags for the DSL->JSON path.
java -Xss64m -Xmx8g -cp "$JAVA_CP" hydra.TransformJsonToTarget \
    --target "$TARGET" --package "$PACKAGE" \
    --dist-json-root "$DIST_JSON_ROOT" --output "$OUTPUT" \
    $TEST_FLAG "${EXTRA_ARGS[@]+"${EXTRA_ARGS[@]}"}"
