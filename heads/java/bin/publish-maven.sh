#!/usr/bin/env bash
# Upload the Hydra Java Maven Central distributions as ONE aggregated Central
# Portal deployment (#591).
#
# Java analog of heads/haskell/bin/publish-hackage.sh. The published artifacts
# live under group net.fortytwo.hydra.java (#519). The Java publish set is:
#   hydra-kernel -> hydra-haskell/hydra-java/hydra-python/hydra-lisp/
#   hydra-typescript/hydra-rdf -> hydra-scala/hydra-pg
# (hydra-ext is intentionally excluded; see docs/release-workflow.md.)
#
# Each dist/java/<pkg>/ is (still) an independently buildable Gradle build.
# Publishing, however, goes through a generated ROOT aggregator build at
# dist/java/{settings,build}.gradle (bin/lib/generate-java-package-build.py
# --root-aggregator) that includes every PUBLISH_SET package as a subproject
# and bundles all of their nmcp publications into ONE Central Portal
# deployment — one ~8 min validation cycle total, not one per package (#591).
# This restructure was necessary: nmcp's aggregation plugin only resolves
# project(":path") references within a single settings.gradle project tree;
# Gradle composite builds (includeBuild) are not supported for this purpose
# (confirmed against nmcp 1.6.1 source during #591 investigation).
#
# Two safety properties (mirroring the Hackage script):
#   1. DEPENDENCY CLOSURE: every Hydra package any published package depends on
#      must itself be in the publish set, else publishing strands it on Central.
#   2. LEAVES-FIRST ORDER: a dependency is always built+locally-published before
#      its dependents, so consumers' transitive POM resolution is satisfiable.
#      (The final aggregated upload itself has no "order" — it is one upload.)
#
# Central Portal note: the root aggregator sets publishingType = AUTOMATIC, so
# the single aggregated upload auto-publishes with no manual Central Portal UI
# step — this is the entire point of aggregating (#591). There is no --publish
# flag: --upload performs the complete upload-and-publish in one step.
#
# Usage:
#   publish-maven.sh [--upload] [--package <pkg>]
#
#   (default)       dry run: verify JDK + creds + dependency closure + that each
#                   build.gradle resolves; build artifacts locally; NO upload.
#   --upload        build+publishToMavenLocal every package (leaves first), then
#                   run ONE `gradle publishAggregationToCentralPortal` at the
#                   root aggregator (uploads + auto-publishes every package as a
#                   single Central Portal deployment).
#   --package <pkg> restrict the BUILD step to a single package (must still be
#                   in the set). The aggregated upload itself always covers the
#                   full PUBLISH_SET — a single-package Central Portal
#                   deployment defeats the purpose of aggregating; use this
#                   only to iterate on one package's build before a real
#                   publish run.
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

while [ $# -gt 0 ]; do
    case "$1" in
        --upload) DO_UPLOAD=true; shift ;;
        --package) ONLY_PKG="$2"; shift 2 ;;
        --help|-h)
            sed -n '2,/^$/p' "$0" | sed 's/^# \{0,1\}//'
            exit 0
            ;;
        *) echo "Unknown argument: $1" >&2; exit 1 ;;
    esac
done

if [ "$DO_UPLOAD" = true ] && [ -n "$ONLY_PKG" ]; then
    echo "ERROR: --upload with --package is not supported — a single-package Central" >&2
    echo "       Portal deployment defeats the purpose of aggregating (#591). Drop" >&2
    echo "       --package, or use --package alone (no --upload) to iterate on a build." >&2
    exit 1
fi

VERSION="$("$HYDRA_ROOT/bin/lib/hydra-packages.py" current-version)"

# The Java publish set, in LEAVES-FIRST topological order. hydra-ext joined as of
# 0.17.4 (#636) — its prior exclusion (a documented "parametric union case-elimination"
# coder limitation) was traced to #475 (Java/Python eta-expansion + re-inference gap
# on polymorphic/recursive/Set-typed encoder/decoder shapes), which was fixed
# 2026-06-13 but never re-validated against hydra-ext until #636 confirmed a clean
# generation (1105 files, no failures) with the fix in place.
PUBLISH_SET=(
    hydra-kernel
    hydra-build
    hydra-haskell
    hydra-jvm
    hydra-java
    hydra-python
    hydra-scala
    hydra-lisp
    hydra-typescript
    hydra-rdf
    hydra-pg
    hydra-ext
)

GRADLE_TASK="publishAggregationToCentralPortal"
ROOT_AGGREGATOR_DIR="$HYDRA_ROOT/dist/java"

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

# Javadoc runs strict (fail-on-error). The two generated-Javadoc bugs that once
# required a relaxation — unqualified @link refs (#449) and unescaped '&' in
# comments (#493) — are both fixed, so the publish set builds cleanly.

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
        && gradle publishToMavenLocal -x test --refresh-dependencies )
done
echo ""

for pkg in "${PUBLISH_SET[@]}"; do
    [ -n "$ONLY_PKG" ] && [ "$ONLY_PKG" != "$pkg" ] && continue
    pkgdir="$HYDRA_ROOT/dist/java/$pkg"
    echo "=== gradle build  ($pkg @ $VERSION) — verifying artifacts assemble ==="
    ( cd "$pkgdir" && gradle build -x test --refresh-dependencies )
    echo ""
done

# --- #621 Layer A: artifact-content completeness gate ------------------------
# HARD-FAIL if a built jar is missing content its manifest declares. This is the
# channel where the 0.17.2 defect actually SHIPPED: the published Java host was a
# whole #417 rename behind (jar had strings/Cat,Cat2,Intercalate instead of the
# #417 strings/Concat,Join; no dsl/lib/Ordering; no optionals/WithDefault) — from
# a stale publishing-machine dist that every dist/-TREE check passed. Two checks:
#   (1) directory-level: every manifest mainModules namespace is represented by a
#       .class in the jar (catches whole-module truncation, e.g. hydra-build).
#   (2) #417 required-symbols (hydra-kernel only): the specific post-#417 class
#       basenames must exist — catches the stale-rename defect the directory-level
#       check cannot see (the stale jar still had the namespace directories).
REQ_SYMS="$HYDRA_ROOT/bin/lib/java-417-required-symbols.json"
echo "=== Verifying jar completeness (#621: manifest mainModules + #417 symbols) ==="
gate_fail=0
for pkg in "${PUBLISH_SET[@]}"; do
    [ -n "$ONLY_PKG" ] && [ "$ONLY_PKG" != "$pkg" ] && continue
    manifest="$HYDRA_ROOT/dist/json/$pkg/src/main/json/manifest.json"
    jar=$(ls "$HYDRA_ROOT/dist/java/$pkg/build/libs/$pkg-$VERSION.jar" 2>/dev/null | head -1)
    if [ ! -f "$manifest" ]; then echo "  -- $pkg: no manifest.json (skipped)"; continue; fi
    if [ -z "$jar" ] || [ ! -f "$jar" ]; then
        echo "  ERROR: no jar for $pkg at $VERSION (dist/java/$pkg/build/libs/)" >&2; gate_fail=1; continue
    fi
    # hydra-kernel additionally gets the #417 required-symbols check.
    req_args=()
    [ "$pkg" = "hydra-kernel" ] && [ -f "$REQ_SYMS" ] && req_args=(--required-symbols "$REQ_SYMS")
    if python3 "$HYDRA_ROOT/bin/check-artifact-complete.py" \
          --manifest "$manifest" --artifact "$jar" --kind java-jar "${req_args[@]}"; then :; else
        echo "  ARTIFACT INCOMPLETE: $pkg-$VERSION.jar missing declared modules/#417 symbols — refusing to publish" >&2
        gate_fail=1
    fi
done
if [ "$gate_fail" != 0 ]; then
    echo "FAIL: one or more jars are content-incomplete (#621) — the 0.17.2 shape (stale dist →" >&2
    echo "      truncated / pre-#417 jar). Re-sync from a clean dist and rebuild." >&2
    exit 1
fi
echo "  OK: every jar contains all its manifest-declared modules and required #417 symbols."
echo ""

if [ "$DO_UPLOAD" = true ]; then
    # --- Generate the root aggregator build (#591) ---------------------------
    # Regenerated fresh on every --upload run so it always reflects the current
    # PUBLISH_SET and version, mirroring the per-package build.gradle freshness
    # check above.
    echo "=== Generating root aggregator build (dist/java/{settings,build}.gradle) ==="
    HYDRA_ROOT_DIR="$HYDRA_ROOT" "$HYDRA_ROOT/bin/lib/generate-java-package-build.py" \
        --root-aggregator "${PUBLISH_SET[@]}" --out-dir "$ROOT_AGGREGATOR_DIR"
    echo ""

    echo "=== gradle $GRADLE_TASK  (aggregated: ${#PUBLISH_SET[@]} packages @ $VERSION) ==="
    ( cd "$ROOT_AGGREGATOR_DIR" && gradle "$GRADLE_TASK" )
    echo ""
    echo "=== Uploaded ${#PUBLISH_SET[@]} package(s) at $VERSION as ONE aggregated deployment. ==="
    echo "publishingType = AUTOMATIC: validates once (~8 min) then auto-publishes — no manual"
    echo "Central Portal UI step required. Packages in the deployment:"
    printf '  %s\n' "${PUBLISH_SET[@]}"
else
    echo "=== Dry run complete (no upload). Re-run with --upload to push to the Central Portal. ==="
fi
