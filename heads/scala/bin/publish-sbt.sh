#!/usr/bin/env bash
# Upload the Hydra Scala per-package Maven Central distributions in dependency
# order: leaves first.
#
# Scala analog of heads/java/bin/publish-maven.sh. The published artifacts
# live under group net.fortytwo.hydra.scala (the per-JVM-language group from
# #519, distinct from the Java set's net.fortytwo.hydra.java) with the Scala 3
# cross-version suffix (_3). The group is read from the generated build.sbt.
# The publish set is:
#   hydra-kernel -> hydra-haskell/hydra-jvm/hydra-python/hydra-lisp/
#   hydra-typescript/hydra-rdf/hydra-pg; hydra-jvm -> hydra-java/hydra-scala
# (hydra-jvm is the shared JVM base that hydra-java and hydra-scala depend on,
# so it must be in the set for dependency closure.)
# hydra-pg is included: the Scala pg coder's type-argument specialization on the
# multi-param Schema[S,T,V,E] is fixed (#589) so hydra-pg compiles standalone.
# hydra-ext is excluded (coder limitation; not in the standard sync matrix).
#
# Each dist/scala/<pkg>/ is a standalone sbt build whose generated build.sbt
# carries `sbt-sonatype` + `sbt-pgp` publishing. Credentials are read from
# ~/.sbt/1.0/sonatype.sbt or the environment:
#   SONATYPE_USERNAME / SONATYPE_PASSWORD
#   PGP_PASSPHRASE (or interactive prompt)
#
# Two safety properties (mirroring the Java script):
#   1. DEPENDENCY CLOSURE: every Hydra package any published package depends on
#      must itself be in the publish set.
#   2. LEAVES-FIRST ORDER: a dependency is always published before its dependents.
#
# Sonatype Central Portal note: `sbt publishSigned` + sbt-sonatype 3.12+
# submits a bundle to the Central Portal as a reviewable pending deployment.
# Nothing goes live until you log in and click "Publish" at
# https://central.sonatype.com/publishing/deployments.
#
# Usage:
#   publish-sbt.sh [--upload] [--package <pkg>] [--skip <pkg[,pkg...]>]
#
#   (default)       dry run: check deps + build jars locally; NO upload.
#   --upload        run `sbt publishSigned` per package (uploads pending
#                   deployments to the Central Portal).
#   --package <pkg> restrict to a single package (must be in the set).
#   --skip <list>   comma-separated packages to skip (e.g. to resume a batch
#                   after some coordinates already published — Maven Central
#                   versions are immutable, so re-uploading one fails).
#
# Requirements:
#   - sbt on PATH.
#   - GPG key + passphrase (for --upload).
#   - Sonatype credentials in ~/.sbt/1.0/sonatype.sbt or environment.

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_SCALA_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT="$( cd "$HYDRA_SCALA_DIR/../.." && pwd )"

DO_UPLOAD=false
ONLY_PKG=""
SKIP_PKGS=""

while [ $# -gt 0 ]; do
    case "$1" in
        --upload) DO_UPLOAD=true; shift ;;
        --package) ONLY_PKG="$2"; shift 2 ;;
        --skip) SKIP_PKGS=",$2,"; shift 2 ;;
        --help|-h)
            sed -n '2,/^$/p' "$0" | sed 's/^# \{0,1\}//'
            exit 0
            ;;
        *) echo "Unknown argument: $1" >&2; exit 1 ;;
    esac
done

VERSION="$("$HYDRA_ROOT/bin/lib/hydra-packages.py" current-version)"

# Publish set in LEAVES-FIRST topological order. Mirrors the Java publish set
# (hydra-pg included): the Scala pg coder's type-argument specialization on the
# multi-param hydra.pg.mapping.Schema[S,T,V,E] is fixed so hydra-pg compiles
# standalone. Experimental targets (go/coq/wasm), benchmarks (hydra-bench), and
# hydra-ext (coder limitation) are not published for Scala.
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
)

# --- Guard: sbt on PATH ------------------------------------------------------
if ! command -v sbt >/dev/null 2>&1; then
    echo "ERROR: sbt not found on PATH." >&2
    echo "       Install sbt and retry." >&2
    exit 1
fi
echo "  sbt OK: $(sbt --version 2>&1 | head -1)"

# --- Guard: Sonatype credentials (upload only) -------------------------------
if [ "$DO_UPLOAD" = true ]; then
    SBT_SONATYPE_CONF="${SBT_USER_HOME:-$HOME/.sbt}/1.0/sonatype.sbt"
    HAS_CREDS=false
    [ -f "$SBT_SONATYPE_CONF" ] && grep -q "sonatypeCredentialHost\|sonatype" "$SBT_SONATYPE_CONF" 2>/dev/null && HAS_CREDS=true
    [ -n "${SONATYPE_USERNAME:-}" ] && [ -n "${SONATYPE_PASSWORD:-}" ] && HAS_CREDS=true
    if [ "$HAS_CREDS" = false ]; then
        echo "ERROR: no Sonatype credentials found." >&2
        echo "       Set SONATYPE_USERNAME + SONATYPE_PASSWORD, or add credentials to" >&2
        echo "       $SBT_SONATYPE_CONF." >&2
        echo "       See docs/release-workflow.md (Scala releases) for details." >&2
        exit 1
    fi
    echo "  Credentials OK"
fi

# --- Guard: dependency closure -----------------------------------------------
echo "=== Checking dependency closure of Scala publish set ==="
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

# --- Guard: each dist/scala/<pkg>/ is present + versioned -------------------
for pkg in "${PUBLISH_SET[@]}"; do
    [ -n "$ONLY_PKG" ] && [ "$ONLY_PKG" != "$pkg" ] && continue
    bsbt="$HYDRA_ROOT/dist/scala/$pkg/build.sbt"
    [ -f "$bsbt" ] || { echo "ERROR: missing $bsbt (run a sync first)" >&2; exit 1; }
    if ! grep -q "\"$VERSION\"" "$bsbt"; then
        echo "ERROR: $pkg build.sbt version != $VERSION (stale dist?)" >&2; exit 1
    fi
done
echo "=== All dist/scala/<pkg>/build.sbt files present and versioned at $VERSION ==="
echo ""

# --- Per-package sbt invocation (leaves first) ------------------------------
# publishLocal each package before its dependents so that the per-package
# standalone build can resolve Hydra inter-package deps from the local ivy cache.
# Mirrors the publishToMavenLocal pattern in publish-maven.sh.

if [ "$DO_UPLOAD" = true ]; then
    # publishLocal covers the FULL set (even --skip'd packages) so downstream
    # siblings can always resolve their deps from the local ivy cache; --skip
    # only affects the Central upload below.
    echo "=== Publishing to local ivy cache first (leaves first, for dep resolution) ==="
    for pkg in "${PUBLISH_SET[@]}"; do
        [ -n "$ONLY_PKG" ] && [ "$ONLY_PKG" != "$pkg" ] && continue
        echo "--- publishLocal $pkg @ $VERSION ---"
        ( cd "$HYDRA_ROOT/dist/scala/$pkg" && sbt publishLocal )
    done
    echo ""

    echo "=== Uploading to Sonatype Central Portal (leaves first) ==="
    for pkg in "${PUBLISH_SET[@]}"; do
        [ -n "$ONLY_PKG" ] && [ "$ONLY_PKG" != "$pkg" ] && continue
        [ -n "$SKIP_PKGS" ] && [ "${SKIP_PKGS#*,$pkg,}" != "$SKIP_PKGS" ] && { echo "  (skipping $pkg)"; continue; }
        # Clear any staging bundle left by a prior (possibly failed) run.
        # sbt-sonatype refuses to overwrite non-SNAPSHOT artifacts in
        # target/sonatype-staging/, which surfaces as "Attempting to overwrite"
        # warnings and a BUNDLE_ZIP_ERROR; a clean dir avoids both.
        rm -rf "$HYDRA_ROOT/dist/scala/$pkg/target/sonatype-staging" \
               "$HYDRA_ROOT/dist/scala/$pkg"/target/*-bundle 2>/dev/null || true
        echo "=== sbt publishSigned  ($pkg @ $VERSION) ==="
        ( cd "$HYDRA_ROOT/dist/scala/$pkg" && sbt publishSigned sonatypeBundleRelease )
        echo ""
    done

    echo "=== Uploaded ${#PUBLISH_SET[@]} package(s) at $VERSION as pending deployments. ==="
    echo "Finalize at https://central.sonatype.com/publishing/deployments —"
    echo "verify each passed validation, then click Publish (leaves first)."
    printf '  %s\n' "${PUBLISH_SET[@]}"
else
    # Dry run: publishLocal leaves-first (so each package can resolve its
    # Hydra siblings from the local ivy cache), then verify package builds.
    echo "=== [dry run] Publishing to local ivy cache first (leaves first) ==="
    for pkg in "${PUBLISH_SET[@]}"; do
        [ -n "$ONLY_PKG" ] && [ "$ONLY_PKG" != "$pkg" ] && continue
        [ -n "$SKIP_PKGS" ] && [ "${SKIP_PKGS#*,$pkg,}" != "$SKIP_PKGS" ] && { echo "  (skipping $pkg)"; continue; }
        echo "--- publishLocal $pkg @ $VERSION ---"
        ( cd "$HYDRA_ROOT/dist/scala/$pkg" && sbt publishLocal )
        echo ""
    done
    echo "=== [dry run] Verifying package artifacts build (no upload) ==="
    for pkg in "${PUBLISH_SET[@]}"; do
        [ -n "$ONLY_PKG" ] && [ "$ONLY_PKG" != "$pkg" ] && continue
        [ -n "$SKIP_PKGS" ] && [ "${SKIP_PKGS#*,$pkg,}" != "$SKIP_PKGS" ] && { echo "  (skipping $pkg)"; continue; }
        echo "=== [dry run] sbt package  ($pkg @ $VERSION) ==="
        ( cd "$HYDRA_ROOT/dist/scala/$pkg" && sbt package )
        echo ""
    done
    echo "=== Dry run complete (no upload). Re-run with --upload to push to the Central Portal. ==="
fi
