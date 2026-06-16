#!/usr/bin/env bash
# Verify the per-package Hydra Java distributions are self-contained and
# self-consistent as PUBLISHED Maven artifacts.
#
# Java analog of heads/haskell/bin/verify-distribution.sh and
# heads/python/bin/verify-distribution.sh. Same class of bug it guards against:
# a published package whose code references something that lives only in the
# worktree (heads/java/ or another package's source set), not in the packaged
# dist/java/<pkg>/ artifact. Locally everything compiles because the monolithic
# developer Gradle project (packages/hydra-java/) references dist/java/<pkg>/ as
# source sets; the break would only appear ACROSS the packaging boundary, when a
# consumer resolves the published jar + POM from Maven. No existing check
# exercises that boundary for Java — this does. (Cf. #472 for the Python wheel
# version of the same bug.)
#
# How it works (mirrors the Haskell verifier's "stage the not-yet-published trio
# together" approach, adapted to Maven coordinates):
#   1. Publish the Java publish-set packages (hydra-kernel -> hydra-rdf ->
#      hydra-pg -> hydra-java, leaves-first) from each dist/java/<pkg>/ build
#      into a FRESH, TEMPORARY local Maven repo (-Dmaven.repo.local=<tmp>), NOT
#      into ~/.m2 (so a stale or published artifact can't mask a gap).
#   2. Build a tiny throwaway consumer project that depends on
#      net.fortytwo.hydra:hydra-kernel:<version> (+ the other publish-set
#      coordinates) and resolves ONLY from that temp repo, OFFLINE. mavenCentral
#      is not consulted, so a dependent whose POM references a sibling missing
#      from the publish set fails loudly instead of silently pulling a published
#      (possibly-broken) version.
#   3. Compile a trivial source file that imports a public kernel type, proving
#      the published jar actually carries the classes its API surface needs.
#
# This is verification only — it does NOT upload anything (see publish-maven.sh).
#
# Requirements: JDK 17+ to RUN gradle (the nmcp/publish plugins require it, even
# though the artifacts target Java 11). If no 17+ JDK is found, the script SKIPS
# with a warning and exits 0 — it must not hard-fail an environment (CI/local)
# that only has the Java 11 runtime the artifacts target. CI provides a 17+ JDK.
#
# Usage:
#   verify-distribution.sh [--keep]
#     --keep  leave the temp repo + work dir in place for inspection.

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_JAVA_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT="$( cd "$HYDRA_JAVA_DIR/../.." && pwd )"

KEEP=false
[ "${1:-}" = "--keep" ] && KEEP=true

# Java publish set, leaves-first (mirror of publish-maven.sh PUBLISH_SET).
PUBLISH_SET=(hydra-kernel hydra-rdf hydra-pg hydra-java)
GROUP="net.fortytwo.hydra"
VERSION="$("$HYDRA_ROOT/bin/lib/hydra-packages.py" current-version)"

# --- JDK 17+ guard: skip (do not fail) if unavailable. -----------------------
jdk_major() {
    # Parses "11.0.19" / "17.0.10" / "1.8.0_xxx" from `java -version`.
    local v
    v="$(java -version 2>&1 | head -1 | sed -E 's/.*version "([0-9]+)\.([0-9]+).*/\1.\2/')"
    case "$v" in
        1.*) echo "${v#1.}" | cut -d. -f1 ;;   # 1.8 -> 8
        *)   echo "$v" | cut -d. -f1 ;;         # 17.0 -> 17
    esac
}
if ! command -v java >/dev/null 2>&1; then
    echo "SKIP: no 'java' on PATH; Java distribution verification needs a JDK 17+." >&2
    exit 0
fi
MAJOR="$(jdk_major)"
if [ -z "$MAJOR" ] || [ "$MAJOR" -lt 17 ] 2>/dev/null; then
    echo "SKIP: active JDK is $(java -version 2>&1 | head -1) — Java distribution" >&2
    echo "      verification needs JDK 17+ (gradle publish plugins). Skipping (exit 0)." >&2
    echo "      Run on a 17+ JDK (CI does) to exercise the packaging boundary." >&2
    exit 0
fi

WORK="$(mktemp -d -t hydra-verify-java-XXXXXX)"
REPO="$WORK/m2"        # temp local Maven repo (NOT ~/.m2)
CONSUMER="$WORK/consumer"
cleanup() { [ "$KEEP" = true ] || rm -rf "$WORK"; }
trap cleanup EXIT

echo "=== Verifying Hydra Java per-package distributions (published-artifact isolation) ==="
echo "  version:   $VERSION"
echo "  publish:   ${PUBLISH_SET[*]}"
echo "  temp repo: $REPO"
echo "  work dir:  $WORK"
echo ""

GRADLE="$HYDRA_JAVA_DIR/gradlew"
[ -x "$GRADLE" ] || GRADLE="gradle"

# --- 1. Publish each publish-set package into the temp repo, leaves-first. ----
for pkg in "${PUBLISH_SET[@]}"; do
    pkgdir="$HYDRA_ROOT/dist/java/$pkg"
    [ -d "$pkgdir" ] || { echo "ERROR: missing dist package: $pkgdir" >&2; exit 1; }
    echo "--- publishing $pkg to temp repo ---"
    ( cd "$pkgdir" && "$GRADLE" --quiet --no-daemon \
        -Dmaven.repo.local="$REPO" \
        publishToMavenLocal ) || {
        echo "ERROR: publishToMavenLocal failed for $pkg" >&2
        KEEP=true
        exit 1
    }
done
echo ""

# --- 2. Throwaway consumer resolving ONLY from the temp repo, offline. --------
echo "=== Building an isolated consumer against the published coordinates ==="
mkdir -p "$CONSUMER/src/main/java/probe"

# settings.gradle: point the consumer's only repository at the temp repo.
cat > "$CONSUMER/settings.gradle" <<GRADLE_SETTINGS
dependencyResolutionManagement {
    repositoriesMode = RepositoriesMode.FAIL_ON_PROJECT_REPOS
    repositories {
        maven { url = uri("${REPO}") }
    }
}
rootProject.name = "hydra-verify-consumer"
GRADLE_SETTINGS

# build.gradle: depend on every publish-set coordinate so each POM + jar must
# resolve from the temp repo. compileJava proves the classes are present.
{
    echo "plugins { id 'java' }"
    echo "dependencies {"
    for pkg in "${PUBLISH_SET[@]}"; do
        echo "    implementation '${GROUP}:${pkg}:${VERSION}'"
    done
    echo "}"
} > "$CONSUMER/build.gradle"

# A trivial probe that touches a public kernel type, forcing the kernel jar's
# classes onto the compile classpath (a jar missing them fails here).
cat > "$CONSUMER/src/main/java/probe/Probe.java" <<'JAVA'
package probe;

// Imports a public kernel type. If the published hydra-kernel jar does not
// carry this class (a self-broken artifact), compilation fails — which is
// exactly the packaging-boundary break this verifier exists to catch.
public final class Probe {
    public static void main(String[] args) {
        // Reference a stable, public kernel class. Name resolution at compile
        // time is enough; we do not run anything.
        Class<?> c = hydra.core.Name.class;
        System.out.println("hydra-kernel class on classpath: " + c.getName());
    }
}
JAVA

if ( cd "$CONSUMER" && "$GRADLE" --quiet --no-daemon --offline \
        -Dmaven.repo.local="$REPO" compileJava ); then
    echo ""
    echo "=== OK: published Java artifacts resolve + compile in isolation ==="
else
    echo ""
    echo "=== FAIL: consumer did not resolve/compile against the published jars ===" >&2
    echo "    A publish-set package is missing classes its API needs, or a POM" >&2
    echo "    references a sibling not in the publish set. See $WORK." >&2
    KEEP=true
    exit 1
fi
