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
#
# hostOverrides["java"] = "local" (#727/#719 un-red): target-driver's PUBLISHED-artifact
# classpath can resolve FINE yet still be semantically stale (not merely absent). This
# tool's own generic transcription logic -- including hydra.print.Core.term's rendering
# of a Literal.decimal, used when baking `expected`-value fixtures into EVERY target's
# generated test file -- comes from the published hydra-kernel/hydra-java 0.17.6 jars,
# which predate #719's printDecimal scale-fidelity fix (BigDecimal("42.0") instead of
# "42" for a scale-0 decimal). This bug shows on EVERY --target (java/python/scala/ts),
# regardless of that target's own hostOverrides entry, because it lives in the JVM
# RUNTIME CLASSES target-driver links against (hydra-kernel/hydra-java), not in any
# per-target coder artifact. So the override key checked here is "java" (this tool's
# own host), not $TARGET -- an explicit hostOverrides["java"]="local" must force the
# local classpath the same way an unresolvable artifact would, so target-driver's own
# runtime dependencies stay in sync with local source instead of a stale publish.

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

# HYDRA_HOST_VERSION forwards -PhostVersion to target-driver (see its build.gradle),
# pinning the published-host classpath explicitly instead of deriving it from
# hydra.json. Used by the cold-seed path (#703) to bypass a hostOverrides:local
# shim that would otherwise make target-driver's classpath unresolvable. An env
# var (not a CLI flag) so it threads transparently through run_layer1_transform /
# assemble-distribution.sh without changing their argument-forwarding contract.
GRADLE_HOST_PROP=()
if [ -n "${HYDRA_HOST_VERSION:-}" ]; then
    GRADLE_HOST_PROP=(-PhostVersion="$HYDRA_HOST_VERSION")
fi

# hostOverrides["java"] forces the local classpath even when the published artifact
# resolves fine (it may be resolvable yet semantically stale — #727/#719). Checked
# against "java", not $TARGET: this tool's runtime classes (hydra.print.Core etc.) come
# from published hydra-kernel/hydra-java regardless of which --target is requested.
FORCE_LOCAL=0
if [ -z "${HYDRA_HOST_VERSION:-}" ]; then
    OVERRIDE_RAW=$(python3 -c "
import json
try:
    with open('$HYDRA_ROOT_DIR/hydra.json') as f:
        cfg = json.load(f)
    print(cfg.get('hostOverrides', {}).get('java', ''))
except Exception:
    pass
" 2>/dev/null || true)
    if [ "$OVERRIDE_RAW" = "local" ]; then
        FORCE_LOCAL=1
    fi
fi

JAVA_CP=""
if [ "$FORCE_LOCAL" = "1" ]; then
    echo "transform-json-to-target.sh: hostOverrides[java]=local;" \
         "using local headsExtras build (published artifact may be stale, not just absent)." >&2
    ./gradlew --quiet :hydra-java:compileHeadsExtrasJava >&2
    JAVA_CP=$(./gradlew --quiet :hydra-java:printHeadsExtrasRuntimeClasspath | tail -1)
elif ./gradlew --quiet "${GRADLE_HOST_PROP[@]}" -p target-driver classes >/dev/null 2>&1; then
    JAVA_CP=$(./gradlew --quiet "${GRADLE_HOST_PROP[@]}" -p target-driver printRuntimeClasspath | tail -1)
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
