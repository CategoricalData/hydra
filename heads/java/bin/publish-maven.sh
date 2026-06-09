#!/usr/bin/env bash
# Upload the Hydra Java per-package Maven Central distributions in dependency
# order: leaves first.
#
# Java analog of heads/haskell/bin/publish-hackage.sh. The published artifacts
# live under group net.fortytwo.hydra. The 0.16.0 Java publish set is:
#   hydra-kernel -> hydra-rdf -> hydra-pg -> hydra-java
# (hydra-ext is intentionally excluded; see docs/release-workflow.md.)
#
# Each dist/java/<pkg>/ is a self-contained Gradle build whose generated
# build.gradle carries the nmcp `publishAggregationToCentralPortal` task. There
# is no single Gradle multi-project here, so we invoke gradle once per package.
#
# Two safety properties (mirroring the Hackage script):
#   1. DEPENDENCY CLOSURE: every Hydra package any published package depends on
#      must itself be in the publish set, else publishing strands it on Central.
#   2. LEAVES-FIRST ORDER: a dependency is always uploaded before its dependents,
#      so consumers' transitive POM resolution is satisfiable.
#
# Central Portal note: the generated build sets publishingType = USER_MANAGED, so
# an upload lands as a reviewable *pending deployment* in the Central Portal UI
# (the analog of a Hackage candidate). Nothing goes live until you click
# "Publish" per deployment at https://central.sonatype.com/publishing/deployments.
# There is therefore no separate --publish flag here: --upload IS the candidate
# step; finalization is the manual UI click.
#
# Usage:
#   publish-maven.sh [--upload] [--package <pkg>]
#
#   (default)       dry run: verify JDK + creds + dependency closure + that each
#                   build.gradle resolves; build artifacts locally; NO upload.
#   --upload        run `gradle publishAggregationToCentralPortal` per package in
#                   order (uploads pending deployments to the Central Portal).
#   --package <pkg> restrict to a single package (must still be in the set).
#
# Requirements:
#   - JDK 17+ to RUN gradle (the nmcp plugin requires it, even though artifacts
#     target Java 11). On Apple Silicon prefer a native arm64 JDK 18/19.
#   - Sonatype + signing credentials in ~/.gradle/gradle.properties.

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_JAVA_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT="$( cd "$HYDRA_JAVA_DIR/../.." && pwd )"

DO_UPLOAD=false
ONLY_PKG=""
ALLOW_JAVADOC_ERRORS=false

while [ $# -gt 0 ]; do
    case "$1" in
        --upload) DO_UPLOAD=true; shift ;;
        --package) ONLY_PKG="$2"; shift 2 ;;
        # TEMPORARY for the 0.16.0 publish: apply an init script that makes the
        # Javadoc task non-fatal, so the cosmetic unqualified-@link errors (#449)
        # don't block the build. Transient by design — the generated build.gradle
        # stays strict, so a plain `gradle build` keeps failing until #449 is
        # fixed (0.16.1 fast-follow). Remove this flag + the init script then.
        --allow-javadoc-errors) ALLOW_JAVADOC_ERRORS=true; shift ;;
        --help|-h)
            sed -n '2,/^$/p' "$0" | sed 's/^# \{0,1\}//'
            exit 0
            ;;
        *) echo "Unknown argument: $1" >&2; exit 1 ;;
    esac
done

VERSION="$(cat "$HYDRA_ROOT/VERSION")"

# The 0.16.0 Java publish set, in LEAVES-FIRST topological order.
PUBLISH_SET=(hydra-kernel hydra-rdf hydra-pg hydra-java)

GRADLE_TASK="publishAggregationToCentralPortal"

# --- Guard: JDK 17+ ----------------------------------------------------------
# The nmcp plugin fails to load under JDK 11. Catch this before any upload.
JAVA_BIN="${JAVA_HOME:+$JAVA_HOME/bin/}java"
JAVA_MAJOR="$("$JAVA_BIN" -version 2>&1 | head -1 | sed -E 's/.*version "([0-9]+).*/\1/')"
if [ -z "$JAVA_MAJOR" ] || [ "$JAVA_MAJOR" -lt 17 ]; then
    echo "ERROR: gradle must run under JDK 17+ (the nmcp Central Portal plugin requires it)." >&2
    echo "       Current: $("$JAVA_BIN" -version 2>&1 | head -1)" >&2
    echo "       Fix: export JAVA_HOME=\$(/usr/libexec/java_home -v 19)  # or 17/18" >&2
    exit 1
fi
echo "  JDK OK: $("$JAVA_BIN" -version 2>&1 | head -1)"

# --- Guard: Sonatype credentials present -------------------------------------
GRADLE_PROPS="${GRADLE_USER_HOME:-$HOME/.gradle}/gradle.properties"
if [ "$DO_UPLOAD" = true ]; then
    if [ ! -f "$GRADLE_PROPS" ] || ! grep -qiE "sonatype|signing" "$GRADLE_PROPS"; then
        echo "ERROR: no Sonatype/signing credentials in $GRADLE_PROPS." >&2
        echo "       See docs/release-workflow.md (Java releases) for the required keys." >&2
        exit 1
    fi
    echo "  Credentials OK: $GRADLE_PROPS has sonatype/signing keys"
fi

# --- Guard: dependency closure (same source as the Hackage script) -----------
echo "=== Checking dependency closure of Java publish set ==="
in_set() {
    local needle="$1"; local x
    for x in "${PUBLISH_SET[@]}"; do [ "$x" = "$needle" ] && return 0; done
    return 1
}
pkg_hydra_deps() {
    local json="$HYDRA_ROOT/packages/$1/package.json"
    [ -f "$json" ] || { echo "ERROR: no package.json for $1" >&2; return 1; }
    python3 -c "import json,sys; print(' '.join(json.load(open(sys.argv[1])).get('dependencies') or []))" "$json"
}
CLOSURE_OK=true
for pkg in "${PUBLISH_SET[@]}"; do
    for d in $(pkg_hydra_deps "$pkg"); do
        if in_set "$d"; then
            echo "  OK: $pkg -> $d (in publish set)"
        else
            echo "  ERROR: $pkg depends on '$d' which is NOT in the publish set" >&2
            CLOSURE_OK=false
        fi
    done
done
[ "$CLOSURE_OK" = true ] || { echo "FAIL: publish set is not dependency-closed." >&2; exit 1; }
echo "  Publish set is dependency-closed."
echo ""

# --- Verify each dist/java/<pkg>/ is present + versioned ---------------------
for pkg in "${PUBLISH_SET[@]}"; do
    [ -n "$ONLY_PKG" ] && [ "$ONLY_PKG" != "$pkg" ] && continue
    bg="$HYDRA_ROOT/dist/java/$pkg/build.gradle"
    [ -f "$bg" ] || { echo "ERROR: missing $bg (run a sync first)" >&2; exit 1; }
    if ! grep -q "version = '$VERSION'" "$bg"; then
        echo "ERROR: $pkg build.gradle version != $VERSION (stale dist?)" >&2; exit 1
    fi
done

# Transient #449 workaround: a runtime init script that relaxes Javadoc
# fail-on-error WITHOUT editing any generated build.gradle or the generator.
# Applied only with --allow-javadoc-errors, so strict javadoc remains the
# default and `gradle build` keeps failing on #449 until it is fixed.
GRADLE_INIT_ARGS=()
if [ "$ALLOW_JAVADOC_ERRORS" = true ]; then
    GRADLE_INIT_ARGS=(--init-script "$SCRIPT_DIR/javadoc-nonfatal.init.gradle")
    echo "  NOTE: --allow-javadoc-errors set — Javadoc non-fatal for this run only (#449)."
    echo ""
fi

# --- Per-package gradle invocation (leaves first) ----------------------------
# Each downstream package resolves its Hydra deps from mavenLocal()+mavenCentral()
# at build time. Across a pre-release dev cycle the version string (e.g. 0.16.0)
# does not change, so a STALE same-version jar can linger in ~/.m2 and silently
# satisfy the coordinate — making downstream builds compile against an old
# kernel/rdf (missing newly added/renamed symbols). To guarantee each package
# builds/publishes against its freshly-built siblings, we publishToMavenLocal
# every package in leaves-first order (with --refresh-dependencies) BEFORE the
# Central upload pass. (publishAggregationToCentralPortal only uploads; it does
# not refresh local deps.)
echo "=== Refreshing ~/.m2 with freshly-built siblings (leaves first) ==="
for pkg in "${PUBLISH_SET[@]}"; do
    [ -n "$ONLY_PKG" ] && [ "$ONLY_PKG" != "$pkg" ] && continue
    echo "--- publishToMavenLocal $pkg @ $VERSION ---"
    ( cd "$HYDRA_ROOT/dist/java/$pkg" \
        && gradle "${GRADLE_INIT_ARGS[@]}" publishToMavenLocal -x test --refresh-dependencies )
done
echo ""

for pkg in "${PUBLISH_SET[@]}"; do
    [ -n "$ONLY_PKG" ] && [ "$ONLY_PKG" != "$pkg" ] && continue
    pkgdir="$HYDRA_ROOT/dist/java/$pkg"
    if [ "$DO_UPLOAD" = true ]; then
        echo "=== gradle $GRADLE_TASK  ($pkg @ $VERSION) ==="
        ( cd "$pkgdir" && gradle "${GRADLE_INIT_ARGS[@]}" "$GRADLE_TASK" )
    else
        echo "=== [dry run] gradle build  ($pkg @ $VERSION) — verifying artifacts assemble, no upload ==="
        ( cd "$pkgdir" && gradle "${GRADLE_INIT_ARGS[@]}" build -x test --refresh-dependencies )
    fi
    echo ""
done

if [ "$DO_UPLOAD" = true ]; then
    echo "=== Uploaded ${#PUBLISH_SET[@]} package(s) at $VERSION as pending deployments. ==="
    echo "Finalize at https://central.sonatype.com/publishing/deployments —"
    echo "verify each passed validation, then click Publish (leaves first)."
else
    echo "=== Dry run complete (no upload). Re-run with --upload to push to the Central Portal. ==="
fi
