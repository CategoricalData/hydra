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
# only WRITES into that shared directory (fast, no network validation). This
# script then zips that shared directory and POSTs it to the Central Portal
# publisher API as ONE deployment — one ~8 min validation cycle total, not one
# per package (#591).
#
# NOTE: we do NOT use sbt-sonatype's `sonatypeBundleRelease` for the upload.
# In 3.12.2, run from a single-package session (the only option, since each
# dist/scala/<pkg>/ is a standalone build), it zips from the plugin's in-session
# staging state — which is EMPTY, because each publishSigned ran in its own sbt
# process — producing a 22-byte empty bundle that Central rejects with "Bundle
# has no files". We zip the on-disk shared bundle dir directly instead.
#
# Credentials are read from ~/.sbt/1.0/sonatype.sbt or the environment:
#   SONATYPE_USERNAME / SONATYPE_PASSWORD
#   PGP_PASSPHRASE (or interactive prompt)
# Set HYDRA_SCALA_PUBLISH_HOLD=1 to upload as USER_MANAGED (validate + hold for
# review) instead of AUTOMATIC (validate + auto-publish).
#
# Two safety properties (mirroring the Java script):
#   1. DEPENDENCY CLOSURE: every Hydra package any published package depends on
#      must itself be in the publish set.
#   2. LEAVES-FIRST ORDER: a dependency is always published-local before its
#      dependents, so downstream builds resolve fresh siblings. (The final
#      aggregated release itself has no "order" — it is one release.)
#
# Sonatype Central Portal note: the direct upload uses publishingType=AUTOMATIC
# by default, so the single aggregated deployment validates then auto-publishes
# with no manual Central Portal UI step — this is the entire point of
# aggregating (#591). The upload endpoint is the Central Portal publisher API
# (`/api/v1/publisher/upload`); the legacy OSSRH Nexus (`sonatypeBundleUpload`,
# /service/local) is decommissioned and 404s — do NOT use it. This script POSTs
# the zipped bundle, then polls the status endpoint for validation (a few
# minutes total, not per package). On a client timeout mid-poll the deployment
# is left in the Portal (check its state in the UI and re-run or release there).
#
# Usage:
#   publish-sbt.sh [--upload] [--package <pkg>] [--skip <pkg[,pkg...]>]
#
#   (default)       dry run: check deps + build jars locally; NO upload.
#   --upload        run `sbt publishSigned` per package (staging only, fast),
#                   then zip the shared bundle directory and POST it to the
#                   Central Portal as one deployment (uploads + auto-publishes
#                   every package). HYDRA_SCALA_PUBLISH_HOLD=1 holds at VALIDATED.
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

# --- #621 Layer A: artifact-content completeness gate ------------------------
# HARD-FAIL if a built Scala jar is missing a module its manifest declares. The
# 0.17.2 defects (hydra-build 3/8; the Java host a whole #417 rename behind)
# shipped because the publishing machine's dist/ was stale and only the dist/
# TREE was ever checked — never the packaged artifact. This inspects the actual
# per-package .jar sbt produced, so a truncated jar can never reach Sonatype.
# Scala jars sit at dist/scala/<pkg>/target/scala-<v>/<pkg>_<binv>-<version>.jar.
# Call AFTER the jars are built (publishLocal / sbt package) in either path.
run_621_scala_gate() {
    echo "=== Verifying Scala jar completeness (#621: manifest mainModules present in each jar) ==="
    local gate_fail=0 pkg jar manifest
    for pkg in "${PUBLISH_SET[@]}"; do
        [ -n "$ONLY_PKG" ] && [ "$ONLY_PKG" != "$pkg" ] && continue
        [ -n "$SKIP_PKGS" ] && [ "${SKIP_PKGS#*,$pkg,}" != "$SKIP_PKGS" ] && { echo "  (skipping $pkg)"; continue; }
        manifest="$HYDRA_ROOT/dist/json/$pkg/src/main/json/manifest.json"
        # Newest matching jar (excludes sources/javadoc); the _N Scala binary-version
        # suffix (e.g. _3) is part of the sbt artifact name.
        jar=$(ls -t "$HYDRA_ROOT/dist/scala/$pkg"/target/scala-*/"$pkg"_*-"$VERSION".jar 2>/dev/null \
              | grep -viE 'sources|javadoc' | head -1)
        if [ ! -f "$manifest" ]; then echo "  -- $pkg: no manifest.json (skipped)"; continue; fi
        if [ -z "$jar" ] || [ ! -f "$jar" ]; then
            echo "  ERROR: no Scala jar found for $pkg at $VERSION" >&2; gate_fail=1; continue
        fi
        if python3 "$HYDRA_ROOT/bin/check-artifact-complete.py" \
              --manifest "$manifest" --artifact "$jar" --kind scala-jar; then :; else
            echo "  ARTIFACT INCOMPLETE: $(basename "$jar") missing declared modules — refusing to publish" >&2
            gate_fail=1
        fi
    done
    if [ "$gate_fail" -ne 0 ]; then
        echo "FAIL: one or more Scala jars are content-incomplete (#621) — re-sync from a clean dist and rebuild." >&2
        exit 1
    fi
    echo "  OK: every Scala jar contains all its manifest-declared modules."
    echo ""
}

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

# --- Guard: PGP signing works (upload only) ----------------------------------
# sbt-pgp shells out to gpg (useGpg default), which delegates to gpg-agent.
# Verify the agent can actually PRODUCE A SIGNATURE before we stage 11 packages
# and start an aggregated Central Portal deployment: a signing failure partway
# through publishSigned leaves a locked/partial deployment that must be dropped
# in the Portal UI. Fail fast here instead. (Set HYDRA_PGP_KEY to pin the key;
# otherwise gpg's default signing key is used.)
if [ "$DO_UPLOAD" = true ]; then
    echo "=== Preflight: verifying PGP signing (gpg-agent) ==="
    if ! command -v gpg >/dev/null 2>&1; then
        echo "ERROR: gpg not found on PATH — required for publishSigned." >&2
        exit 1
    fi
    _sign_probe="$(mktemp)"; printf 'hydra-release-signing-probe\n' > "$_sign_probe"
    _sign_key_flag=()
    [ -n "${HYDRA_PGP_KEY:-}" ] && _sign_key_flag=(--local-user "$HYDRA_PGP_KEY")
    if gpg --batch --yes ${_sign_key_flag[@]+"${_sign_key_flag[@]}"} --detach-sign --armor \
           -o "$_sign_probe.asc" "$_sign_probe" 2>"$_sign_probe.err" \
       && gpg --verify "$_sign_probe.asc" "$_sign_probe" 2>/dev/null; then
        echo "  PGP signing OK (gpg-agent produced and verified a signature)"
        rm -f "$_sign_probe" "$_sign_probe.asc" "$_sign_probe.err"
    else
        echo "ERROR: gpg could not sign — publishSigned would fail mid-bundle." >&2
        echo "       Diagnostic output:" >&2
        sed 's/^/         /' "$_sign_probe.err" >&2 2>/dev/null
        echo "       Common fix: restart the agent (gpgconf --kill gpg-agent)," >&2
        echo "       or use a stable gpg if a dev build (2.5.x) is active." >&2
        rm -f "$_sign_probe" "$_sign_probe.asc" "$_sign_probe.err"
        exit 1
    fi
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

    # #621 Layer A: gate the built jars BEFORE signing/staging/upload. publishLocal
    # produces the per-package jar under target/scala-*/; a truncated one dies here.
    run_621_scala_gate

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

    # Re-sign every staged artifact with the intended release key.
    #
    # WHY: sbt-pgp does not honour ANY documented key-selection mechanism here.
    # Verified empirically against real staged signatures: pgpSigningKey (short id
    # and 0x-prefixed long id), usePgpKeyHex(<40-char fingerprint>), and
    # Credentials("GnuPG Key ID", "gpg", <fp>, "ignored") -- the two the README
    # documents -- all still produce signatures from gpg's DEFAULT key. A logging
    # wrapper on gpgCommand proved the gpg binary is never exec'd at all, despite
    # useGpg reporting true, so the plugin signs internally (bouncycastle), where
    # the README states key selection is undocumented. See sbt/sbt-pgp#125, #47.
    #
    # Consequence if left alone: Maven artifacts carry the maintainer's personal
    # key rather than the release key in the repo-root KEYS file. That is exactly
    # what 0.17.5 shipped -- confirmed by verifying the live 0.17.5 jar on Central.
    #
    # The bundle is plain files on disk, so re-signing is straightforward and
    # verifiable. Skipped entirely when HYDRA_PGP_KEY is unset.
    if [ -n "${HYDRA_PGP_KEY:-}" ]; then
        echo "=== Re-signing staged artifacts with $HYDRA_PGP_KEY (sbt-pgp ignores key selection) ==="
        _resigned=0
        while IFS= read -r _asc; do
            _artifact="${_asc%.asc}"
            [ -f "$_artifact" ] || continue
            rm -f "$_asc"
            if ! gpg --batch --yes --local-user "$HYDRA_PGP_KEY" \
                     --armor --detach-sign --output "$_asc" "$_artifact" 2>/dev/null; then
                echo "ERROR: failed to re-sign $_artifact" >&2
                exit 1
            fi
            _resigned=$((_resigned + 1))
        done < <(find "$BUNDLE_DIR" -name '*.asc' -type f)
        echo "  re-signed $_resigned artifact(s)"

        # Verify EVERY signature is from the intended key. A single mismatch is
        # release-blocking: publishing a wrongly-signed artifact is irreversible.
        echo "=== Verifying every staged signature is from $HYDRA_PGP_KEY ==="
        _want="$(gpg --fingerprint --with-colons "$HYDRA_PGP_KEY" 2>/dev/null \
                 | awk -F: '/^fpr:/ {print $10; exit}')"
        if [ -z "$_want" ]; then
            echo "ERROR: could not resolve a fingerprint for $HYDRA_PGP_KEY" >&2
            exit 1
        fi
        _bad=0; _checked=0
        while IFS= read -r _asc; do
            _artifact="${_asc%.asc}"
            [ -f "$_artifact" ] || continue
            _checked=$((_checked + 1))
            if ! gpg --verify "$_asc" "$_artifact" 2>&1 | grep -q "$_want"; then
                echo "  MISMATCH: $_asc" >&2
                _bad=$((_bad + 1))
            fi
        done < <(find "$BUNDLE_DIR" -name '*.asc' -type f)
        if [ "$_bad" -gt 0 ]; then
            echo "ERROR: $_bad of $_checked signatures are not from $_want — refusing to upload." >&2
            exit 1
        fi
        echo "  OK: all $_checked signatures verified against $_want"
    fi

    # Upload the aggregated bundle to the Central Portal DIRECTLY, not via
    # sbt-sonatype's `sonatypeBundleRelease`.
    #
    # Why not sonatypeBundleRelease: with sbt-sonatype 3.12.2, running it from a
    # single package session (the only way, since each dist/scala/<pkg>/ is a
    # standalone build) zips from the plugin's IN-SESSION staging state rather
    # than the shared on-disk $BUNDLE_DIR. Because each publishSigned above ran
    # in its OWN sbt process, that session has staged NOTHING, so the task builds
    # an EMPTY 22-byte bundle.zip and the Central Portal rejects the deployment
    # with "Bundle has no files" — even though $BUNDLE_DIR holds every signed
    # artifact + checksum in correct Maven layout. (Observed on 0.17.2.)
    #
    # Instead we zip $BUNDLE_DIR (which already has the maven layout at its root:
    # net/fortytwo/hydra/scala/...) and POST it to the Central Portal publisher
    # API — the same endpoint sonatypeBundleRelease targets, minus the broken
    # zip step. publishingType=AUTOMATIC preserves the "validate then auto-publish,
    # no manual UI step" behavior; set HYDRA_SCALA_PUBLISH_HOLD=1 to upload as
    # USER_MANAGED (validate + HOLD) for a review checkpoint before releasing.
    echo "=== Building aggregated bundle zip from $BUNDLE_DIR ==="
    _bundle_zip="$HYDRA_ROOT/dist/scala/hydra-scala-$VERSION-central-bundle.zip"
    rm -f "$_bundle_zip"
    ( cd "$BUNDLE_DIR" && zip -qr "$_bundle_zip" net )
    _nfiles=$(unzip -l "$_bundle_zip" 2>/dev/null | tail -1 | awk '{print $2}')
    echo "  $_bundle_zip ($_nfiles files)"
    if [ "${_nfiles:-0}" -lt 1 ]; then
        echo "ERROR: bundle zip is empty — nothing was staged into $BUNDLE_DIR." >&2
        exit 1
    fi

    # Central Portal auth: Bearer base64(user:pass), from env or ~/.sbt sonatype.sbt.
    _cp_user="${SONATYPE_USERNAME:-}"; _cp_pass="${SONATYPE_PASSWORD:-}"
    if [ -z "$_cp_user" ] || [ -z "$_cp_pass" ]; then
        read -r _cp_user _cp_pass < <(python3 - "$SBT_SONATYPE_CONF" <<'PY'
import re,sys
try:
    t=open(sys.argv[1]).read()
    m=re.search(r'Credentials\(\s*"[^"]*",\s*"[^"]*",\s*"([^"]*)",\s*"([^"]*)"\s*\)',t)
    if m: print(m.group(1), m.group(2))
except Exception: pass
PY
)
    fi
    if [ -z "$_cp_user" ] || [ -z "$_cp_pass" ]; then
        echo "ERROR: no Central Portal credentials (SONATYPE_USERNAME/PASSWORD or $SBT_SONATYPE_CONF)." >&2
        exit 1
    fi
    _cp_bearer=$(printf '%s:%s' "$_cp_user" "$_cp_pass" | base64 | tr -d '\n')
    _pub_type="AUTOMATIC"; [ "${HYDRA_SCALA_PUBLISH_HOLD:-}" = "1" ] && _pub_type="USER_MANAGED"

    echo "=== Uploading bundle to Central Portal (publishingType=$_pub_type) ==="
    _dep_id=$(curl -s -X POST \
        "https://central.sonatype.com/api/v1/publisher/upload?publishingType=$_pub_type&name=net.fortytwo.hydra.scala-$VERSION" \
        -H "Authorization: Bearer $_cp_bearer" -F "bundle=@$_bundle_zip" 2>/dev/null)
    echo "  deployment id: $_dep_id"
    case "$_dep_id" in
        *[!0-9a-fA-F-]*|"") echo "ERROR: upload did not return a deployment id: $_dep_id" >&2; exit 1 ;;
    esac

    echo "=== Polling deployment $_dep_id for validation ==="
    _state=""
    for _i in $(seq 1 60); do
        _state=$(curl -s -X POST "https://central.sonatype.com/api/v1/publisher/status?id=$_dep_id" \
            -H "Authorization: Bearer $_cp_bearer" 2>/dev/null \
            | python3 -c "import json,sys
try: print(json.load(sys.stdin).get('deploymentState','?'))
except: print('poll-error')" 2>/dev/null)
        echo "  [$_i] state=$_state"
        case "$_state" in
            PUBLISHED) echo "=== PUBLISHED: ${#PUBLISH_SET[@]} packages at $VERSION ==="; break ;;
            VALIDATED) [ "$_pub_type" = "USER_MANAGED" ] && { echo "=== VALIDATED and HELD (USER_MANAGED). Release from the Portal UI or re-run without HYDRA_SCALA_PUBLISH_HOLD. ==="; break; } ;;
            FAILED) echo "ERROR: Central Portal deployment FAILED. Query the status API / Portal UI for details." >&2; exit 1 ;;
        esac
        sleep 20
    done
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
    # #621 Layer A: verify each freshly-packaged jar's content against its manifest.
    run_621_scala_gate
    echo "=== Dry run complete (no upload). Re-run with --upload to push to the Central Portal. ==="
fi
