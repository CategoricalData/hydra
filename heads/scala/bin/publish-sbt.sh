#!/usr/bin/env bash
# Upload the Hydra Scala Maven Central distributions as ONE aggregated Central
# Portal deployment (#591).
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
# carries `sbt-sonatype` + `sbt-pgp` publishing, AND points
# `sonatypeBundleDirectory` at ONE SHARED path across every package
# (bin/lib/generate-scala-package-build.py; default
# dist/scala/.central-portal-bundle). This is sbt-sonatype's documented
# mechanism for staging a multi-module bundle: per-package `publishSigned`
# only WRITES into that shared directory (fast, no network validation); a
# single trailing `sonatypeBundleRelease` then uploads the combined bundle as
# ONE Central Portal deployment — one ~8 min validation cycle total, not one
# per package (#591). Credentials are read from ~/.sbt/1.0/sonatype.sbt or the
# environment:
#   SONATYPE_USERNAME / SONATYPE_PASSWORD
#   PGP_PASSPHRASE (or interactive prompt)
#
# Two safety properties (mirroring the Java script):
#   1. DEPENDENCY CLOSURE: every Hydra package any published package depends on
#      must itself be in the publish set.
#   2. LEAVES-FIRST ORDER: a dependency is always published-local before its
#      dependents, so downstream builds resolve fresh siblings. (The final
#      aggregated release itself has no "order" — it is one release.)
#
# Sonatype Central Portal note: sbt-sonatype defaults to publishingType =
# AUTOMATIC, so the single aggregated `sonatypeBundleRelease` auto-publishes
# with no manual Central Portal UI step — this is the entire point of
# aggregating (#591). `sonatypeBundleRelease` (sbt-sonatype 3.12.2) is the
# ONLY task that targets the Central Portal API — `sonatypeBundleUpload`
# talks to the decommissioned legacy OSSRH Nexus (/service/local) and 404s,
# so do NOT use it. The single trailing release call uploads the combined
# bundle and blocks polling for validation (a few minutes total, not per
# package); on a rare client timeout mid-poll the deployment is left locked
# (drop the stuck deployment in the Portal UI and re-run).
#
# Usage:
#   publish-sbt.sh [--upload] [--package <pkg>] [--skip <pkg[,pkg...]>]
#
#   (default)       dry run: check deps + build jars locally; NO upload.
#   --upload        run `sbt publishSigned` per package (staging only, fast),
#                   then ONE `sbt sonatypeBundleRelease` against the shared
#                   bundle directory (uploads + auto-publishes every package
#                   as a single Central Portal deployment).
#   --package <pkg> restrict the BUILD/STAGE step to a single package (must be
#                   in the set). The final release always covers the full
#                   PUBLISH_SET — a single-package Central Portal deployment
#                   defeats the purpose of aggregating; use this only to
#                   iterate on one package's build before a real publish run.
#   --skip <list>   comma-separated packages to skip STAGING for (e.g. to
#                   resume a batch after some coordinates already published —
#                   Maven Central versions are immutable, so re-staging one
#                   fails). Has no effect on the trailing release step, which
#                   always releases whatever is in the shared bundle directory.
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

if [ "$DO_UPLOAD" = true ] && [ -n "$ONLY_PKG" ]; then
    echo "ERROR: --upload with --package is not supported — a single-package Central" >&2
    echo "       Portal deployment defeats the purpose of aggregating (#591). Drop" >&2
    echo "       --package, or use --package alone (no --upload) to iterate on a build." >&2
    exit 1
fi

VERSION="$("$HYDRA_ROOT/bin/lib/hydra-packages.py" current-version)"

# Shared sonatypeBundleDirectory (#591). Must match the default in
# bin/lib/generate-scala-package-build.py (--bundle-dir override), since every
# per-package build.sbt was generated pointing at this same path.
BUNDLE_DIR="$HYDRA_ROOT/dist/scala/.central-portal-bundle"

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
    # only affects staging below. (--package is rejected together with
    # --upload above, so ONLY_PKG is always empty here — the full set runs.)
    echo "=== Publishing to local ivy cache first (leaves first, for dep resolution) ==="
    for pkg in "${PUBLISH_SET[@]}"; do
        echo "--- publishLocal $pkg @ $VERSION ---"
        ( cd "$HYDRA_ROOT/dist/scala/$pkg" && sbt publishLocal )
    done
    echo ""

    # Clear any bundle left by a prior (possibly failed) run. sbt-sonatype
    # refuses to overwrite non-SNAPSHOT artifacts in the staging dir, which
    # surfaces as "Attempting to overwrite" warnings and a BUNDLE_ZIP_ERROR; a
    # clean SHARED dir avoids both, and must be wiped ONCE up front here
    # (not per-package) since every package stages into the same directory.
    rm -rf "$BUNDLE_DIR"
    mkdir -p "$BUNDLE_DIR"

    echo "=== Staging every package into the shared bundle directory (leaves first) ==="
    echo "  $BUNDLE_DIR"
    for pkg in "${PUBLISH_SET[@]}"; do
        [ -n "$SKIP_PKGS" ] && [ "${SKIP_PKGS#*,$pkg,}" != "$SKIP_PKGS" ] && { echo "  (skipping $pkg)"; continue; }
        echo "--- sbt publishSigned  ($pkg @ $VERSION) — staging only, no upload ---"
        ( cd "$HYDRA_ROOT/dist/scala/$pkg" && sbt publishSigned )
        echo ""
    done

    # sonatypeBundleRelease is the ONLY sbt-sonatype 3.12.2 task that targets
    # the Central Portal API (sonatypeBundleUpload 404s against the
    # decommissioned legacy OSSRH Nexus — do not use it). Called ONCE here,
    # against the shared bundle directory, instead of once per package: it
    # uploads the combined bundle and blocks polling for validation (a few
    # minutes total, not per package); on a rare client timeout mid-poll the
    # deployment is left locked (drop the stuck deployment in the Portal UI
    # and re-run).
    echo "=== sbt sonatypeBundleRelease  (aggregated: ${#PUBLISH_SET[@]} packages @ $VERSION) ==="
    ( cd "$HYDRA_ROOT/dist/scala/${PUBLISH_SET[0]}" && sbt sonatypeBundleRelease )
    echo ""
    echo "=== Uploaded ${#PUBLISH_SET[@]} package(s) at $VERSION as ONE aggregated deployment. ==="
    echo "sbt-sonatype's default publishingType = AUTOMATIC: validates once (~8 min) then"
    echo "auto-publishes — no manual Central Portal UI step required. Packages in the deployment:"
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
