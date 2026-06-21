#!/usr/bin/env bash
# Pre-release preparation script for Hydra.
#
# Verifies that the RELEASED SET is consistent and passing, then produces
# upload-ready release artifacts.
#
# Release gate vs. quality check (the cohesive principle, #418): the script
# HARD-FAILS (blocks the release) only on artifacts that are actually shipped —
# Haskell (Hackage: hydra-kernel/hydra-haskell/hydra), Java (Maven Central,
# per-package), Scala (Maven Central, per-package, #491), Python (PyPI/conda)
# — plus version sync, the JSON kernel, and lexicon freshness. Targets that are
# NOT released yet (the Lisp dialects and the Java bindings under bindings/java/)
# are run as non-blocking QUALITY CHECKS: a failure there is a WARNING, not an
# ERROR. When one of those becomes a released artifact, promote its step from
# WARNING to ERROR (gate).
#
# Steps performed:
#   1.  Version synchronization across all implementations                 [gate]
#   2.  Haskell tests (hydra-test Stack target: hydra-kernel + hydra-ext)  [gate]
#   3.  Java: hydra-java rollup tests [gate]; bindings build [quality check]
#   4.  Python tests [gate] + ruff code quality [quality check]
#   5.  TypeScript tests (tsc --strict + vitest)                           [gate]
#   6.  Scala build and tests                                    [quality check]
#   7.  Lisp tests (Clojure, Common Lisp, Emacs Lisp, Scheme)    [quality check]
#   8.  JSON kernel verification (round-trips vs the in-memory kernel)     [gate]
#   9.  Lexicon freshness (docs/hydra-lexicon.txt matches the kernel)      [gate]
#   10. Per-package Hackage sdist case-sensitivity check (assembles cleanly
#       on a case-sensitive filesystem + `cabal v2-build --dry-run`)       [gate]
#   11. Per-package Haddock-for-Hackage docs build (ready for `cabal upload`)[gate]
#   12. Canonical source archive + checksum + signature: a single
#       `git archive` source tarball (the Apache "release of record"),
#       its SHA-512 checksum, and a detached GPG signature.               [gate*]
#   13. Per-host published-package self-containment                        [gate]
#
# On success, the upload-ready per-package artifacts (sdists + docs tarballs)
# plus the canonical source archive (tarball + .sha512 + .asc) land in
# release-artifacts/ at the repo root. Logs land in verify-logs/.
#
# *Step 11 gate detail: the archive + checksum are a hard gate (LICENSE and
# NOTICE must be present; the tarball must build); the GPG signature degrades
# to a WARNING when no signing key is configured, so the script stays runnable
# outside a real release. Set HYDRA_RELEASE_SIGNING_KEY (a gpg key id/email) to
# sign; otherwise gpg's default key is used, and a missing key is a warning.
#
# Prerequisites:
#   - Stack, Java 11+, Python 3.12+, uv, sbt, Clojure, SBCL, Emacs, Guile,
#     cabal-install (with GHC 9.10+), and (on macOS) hdiutil are installed
#   - Run from the repository root directory
#
# Usage:
#   bin/prepare-release.sh          # Full preparation
#   bin/prepare-release.sh --help   # Show this help

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/.." && pwd )"

source "$HYDRA_ROOT/bin/lib/common.sh"

while [ $# -gt 0 ]; do
    case "$1" in
        --help|-h)
            sed -n '2,/^$/p' "$0" | sed 's/^# \{0,1\}//'
            exit 0
            ;;
        *)
            die "Unknown argument: $1 (try --help)"
            ;;
    esac
    shift
done

cd "$HYDRA_ROOT"

# Logs go to a dedicated directory (gitignored), not the repo root.
LOG_DIR="$HYDRA_ROOT/verify-logs"
mkdir -p "$LOG_DIR"

# Final upload-ready artifacts (gitignored).
ARTIFACT_DIR="$HYDRA_ROOT/release-artifacts"
mkdir -p "$ARTIFACT_DIR"

TOTAL_STEPS=13

ERRORS=0
WARNINGS=0

banner1 "Hydra Release Preparation"
echo ""

step 1 $TOTAL_STEPS "Checking version synchronization"
echo ""

CANONICAL_VERSION=$("$HYDRA_ROOT/bin/lib/hydra-packages.py" current-version 2>/dev/null || echo "")
HASKELL_VERSION=$(grep '^version:' heads/haskell/package.yaml | awk '{print $2}')
BOOT_HASKELL_VERSION=$(grep '^version:' demos/bootstrapping/resources/haskell/package.yaml | awk '{print $2}')
JAVA_VERSION=$(grep "version = " heads/java/build.gradle | head -1 | sed "s/.*version = '\\(.*\\)'/\\1/")
BOOT_JAVA_VERSION=$(grep "version = " demos/bootstrapping/resources/java/build.gradle | head -1 | sed "s/.*version = '\\(.*\\)'/\\1/")
PYTHON_VERSION=$(grep '^version' heads/python/pyproject.toml | sed 's/.*"\(.*\)"/\1/')
BOOT_PYTHON_VERSION=$(grep '^version' demos/bootstrapping/resources/python/pyproject.toml | sed 's/.*"\(.*\)"/\1/')
SCALA_VERSION=$(grep 'version :=' packages/hydra-scala/build.sbt | sed 's/.*"\(.*\)".*/\1/' 2>/dev/null || echo "")
TS_VERSION=$(python3 -c "import json; d=open('heads/typescript/package.json').read().split('\n'); lines=[l for l in d if '\"version\"' in l and '//' not in l]; print(lines[0].split('\"')[3])" 2>/dev/null || echo "")
BOOT_TS_VERSION=$(python3 -c "import json; d=open('demos/bootstrapping/resources/typescript/package.json').read().split('\n'); lines=[l for l in d if '\"version\"' in l and '//' not in l]; print(lines[0].split('\"')[3])" 2>/dev/null || echo "")

echo "  hydra.json currentVersion:            $CANONICAL_VERSION"
echo "  heads/haskell/package.yaml:           $HASKELL_VERSION"
echo "  demos/bootstrapping/.../haskell:      $BOOT_HASKELL_VERSION"
echo "  heads/java/build.gradle:              $JAVA_VERSION"
echo "  demos/bootstrapping/.../java:         $BOOT_JAVA_VERSION"
echo "  heads/python/pyproject.toml:          $PYTHON_VERSION"
echo "  demos/bootstrapping/.../python:       $BOOT_PYTHON_VERSION"
echo "  heads/typescript/package.json:        $TS_VERSION"
echo "  demos/bootstrapping/.../typescript:   $BOOT_TS_VERSION"
echo "  packages/hydra-scala/build.sbt:       $SCALA_VERSION"
echo ""

EXPECTED="$CANONICAL_VERSION"
if [ -z "$EXPECTED" ]; then
    echo "  ERROR: hydra.json currentVersion is missing or empty"
    EXPECTED="$HASKELL_VERSION"
fi

VERSION_MISMATCH=false
for pair in \
    "heads/haskell/package.yaml:$HASKELL_VERSION" \
    "demos/bootstrapping/.../haskell:$BOOT_HASKELL_VERSION" \
    "heads/java/build.gradle:$JAVA_VERSION" \
    "demos/bootstrapping/.../java:$BOOT_JAVA_VERSION" \
    "heads/python/pyproject.toml:$PYTHON_VERSION" \
    "demos/bootstrapping/.../python:$BOOT_PYTHON_VERSION" \
    "heads/typescript/package.json:$TS_VERSION" \
    "demos/bootstrapping/.../typescript:$BOOT_TS_VERSION" \
    "packages/hydra-scala/build.sbt:$SCALA_VERSION"; do
    file="${pair%%:*}"
    ver="${pair##*:}"
    if [ "$ver" != "$EXPECTED" ]; then
        echo "  ERROR: $file ($ver) != VERSION ($EXPECTED)"
        VERSION_MISMATCH=true
    fi
done

if [ "$VERSION_MISMATCH" = true ]; then
    echo "  FAIL: Versions are not synchronized (run bin/bump-version.sh to fix)"
    ERRORS=$((ERRORS + 1))
else
    echo "  OK: All versions are $EXPECTED"
fi

# --- Step 2: Haskell tests ---
# The hydra-test Stack target covers hydra-kernel and hydra-ext content.
# Post-#290, hydra-ext is a frozen Hydra package whose generated Haskell
# is part of the hydra Stack library; there is no separate hydra-ext
# Stack package to test.
step 2 $TOTAL_STEPS "Running Haskell tests"
echo ""

cd "$HYDRA_ROOT/heads/haskell"
if stack test 2>&1 | tee "$LOG_DIR/haskell.log"; then
    echo ""
    echo "  OK: Haskell tests passed"
else
    echo ""
    echo "  FAIL: Haskell tests failed (see verify-logs/haskell.log)"
    ERRORS=$((ERRORS + 1))
fi

# --- Step 3: Java build and tests ---
# RELEASE GATE: only the released Java artifacts are gated. The released set is
# the hydra-java rollup (which covers hydra-kernel/hydra-java + the per-package
# dist trees published to Maven Central). The bindings under bindings/java/
# (hydra-rdf4j, hydra-neo4j, hydra-pg-dsl) are NOT released artifacts yet and have
# no release solution, so building them is a non-blocking quality check, not a
# gate. They are wired as subprojects of heads/java only for development; bare
# `./gradlew test` would aggregate them. We gate on `:hydra-java:test`. When a
# binding graduates to a released artifact, move it into the gate below. (#418)
step 3 $TOTAL_STEPS "Running Java build and tests (released set)"
echo ""

cd "$HYDRA_ROOT/heads/java"
if ./gradlew :hydra-java:test 2>&1 | tee "$LOG_DIR/java.log"; then
    echo ""
    echo "  OK: Java (hydra-java rollup) tests passed"
else
    echo ""
    echo "  FAIL: Java tests failed (see verify-logs/java.log)"
    ERRORS=$((ERRORS + 1))
fi

# Non-blocking quality check: the bindings (not yet released artifacts).
if ./gradlew :hydra-rdf4j:test :hydra-neo4j:test :hydra-pg-dsl:test 2>&1 \
     | tee "$LOG_DIR/java-bindings.log" >/dev/null; then
    echo "  OK: Java bindings build/test passed (not a release gate)"
else
    echo "  WARNING: Java bindings build/test failed — not a release gate, but"
    echo "           see verify-logs/java-bindings.log (bindings have no release"
    echo "           solution yet; tracked separately)"
    WARNINGS=$((WARNINGS + 1))
fi

# --- Step 4: Python tests and code quality ---
step 4 $TOTAL_STEPS "Running Python tests and code quality checks"
echo ""

cd "$HYDRA_ROOT/heads/python"

if uv run pytest 2>&1 | tee "$LOG_DIR/python.log"; then
    echo ""
    echo "  OK: Python tests passed"
else
    echo ""
    echo "  FAIL: Python tests failed (see verify-logs/python.log)"
    ERRORS=$((ERRORS + 1))
fi

if uv run ruff check 2>&1; then
    echo "  OK: ruff check passed"
else
    echo "  WARNING: ruff check found issues"
    WARNINGS=$((WARNINGS + 1))
fi

if uv run ruff format --check 2>&1; then
    echo "  OK: ruff format check passed"
else
    echo "  WARNING: ruff format found issues"
    WARNINGS=$((WARNINGS + 1))
fi

# --- Step 5: TypeScript tests ---
# RELEASE GATE: TypeScript is a released artifact (npm). The test runner
# exercises tsc --strict on the dist tree plus the vitest common test suite.
step 5 $TOTAL_STEPS "Running TypeScript tests (tsc --strict + vitest)"
echo ""

cd "$HYDRA_ROOT"
TS_DIST="$HYDRA_ROOT/dist/typescript/hydra-kernel"
if [ ! -d "$TS_DIST/src/main/typescript/hydra" ]; then
    echo "  FAIL: TypeScript dist not found at $TS_DIST"
    echo "        Run bin/sync-typescript.sh first."
    ERRORS=$((ERRORS + 1))
elif "$HYDRA_ROOT/heads/typescript/bin/test-distribution.sh" hydra-kernel \
       2>&1 | tee "$LOG_DIR/typescript.log"; then
    echo ""
    echo "  OK: TypeScript tests passed"
else
    echo ""
    echo "  FAIL: TypeScript tests failed (see verify-logs/typescript.log)"
    ERRORS=$((ERRORS + 1))
fi

# --- Step 6: Scala build and tests (release gate, #491) ---
# Scala is a released artifact on Maven Central (per-package under
# net.fortytwo.hydra, published via heads/scala/bin/publish-sbt.sh), so a
# Scala test failure BLOCKS the release.
step 6 $TOTAL_STEPS "Running Scala build and tests"
echo ""

cd "$HYDRA_ROOT/packages/hydra-scala"
if sbt test 2>&1 | tee "$LOG_DIR/scala.log"; then
    echo ""
    echo "  OK: Scala tests passed"
else
    echo ""
    echo "  ERROR: Scala tests failed (see verify-logs/scala.log)" >&2
    ERRORS=$((ERRORS + 1))
fi

# --- Step 7: Lisp tests (quality check, NOT a release gate) ---
# The Lisp dialects (Clojure, Common Lisp, Emacs Lisp, Scheme) are not released
# artifacts (nothing published to Clojars / any registry), so failures here are
# non-blocking WARNINGS, not release gates. Each dialect runs through the unified
# packages/hydra-lisp/bin/run-tests.sh runner. (#418)
# NOTE: the runner is known to under-report (it can exit 0 despite individual
# test failures); since Lisp is non-gating that does not affect the release
# decision, but it is tracked separately as a runner bug.
step 7 $TOTAL_STEPS "Running Lisp tests (quality check)"
echo ""

cd "$HYDRA_ROOT"
LISP_RUNNER="$HYDRA_ROOT/packages/hydra-lisp/bin/run-tests.sh"

for dialect in clojure common-lisp emacs-lisp scheme; do
    echo "  Testing $dialect..."
    if bash "$LISP_RUNNER" "$dialect" 2>&1 | tee "$LOG_DIR/${dialect}.log" | tail -3; then
        echo "  OK: $dialect tests passed"
    else
        echo "  WARNING: $dialect tests failed (not a release gate; see verify-logs/${dialect}.log)"
        WARNINGS=$((WARNINGS + 1))
    fi
    echo ""
done

# --- Step 8: JSON kernel verification ---
step 8 $TOTAL_STEPS "Verifying JSON kernel"
echo ""

cd "$HYDRA_ROOT/heads/haskell"
if stack exec verify-json-kernel -- +RTS -K256M -A32M -RTS 2>&1; then
    echo ""
    echo "  OK: JSON kernel verification passed"
else
    echo ""
    echo "  FAIL: JSON kernel verification failed"
    ERRORS=$((ERRORS + 1))
fi

# --- Step 9: Lexicon freshness ---
step 9 $TOTAL_STEPS "Verifying lexicon freshness"
echo ""

cd "$HYDRA_ROOT"
LEXICON_PATH="$HYDRA_ROOT/docs/hydra-lexicon.txt"
LEXICON_BACKUP=$(mktemp)
if [ -f "$LEXICON_PATH" ]; then
    cp "$LEXICON_PATH" "$LEXICON_BACKUP"
else
    : > "$LEXICON_BACKUP"
fi

if bash "$HYDRA_ROOT/bin/regenerate-lexicon.sh" 2>&1 | tee "$LOG_DIR/lexicon.log"; then
    if diff -q "$LEXICON_BACKUP" "$LEXICON_PATH" >/dev/null 2>&1; then
        echo "  OK: docs/hydra-lexicon.txt is up to date"
    else
        echo "  FAIL: docs/hydra-lexicon.txt is stale (regeneration produced a diff)"
        echo "        Run bin/regenerate-lexicon.sh and commit the result before releasing."
        ERRORS=$((ERRORS + 1))
    fi
else
    echo "  FAIL: bin/regenerate-lexicon.sh exited non-zero (see verify-logs/lexicon.log)"
    ERRORS=$((ERRORS + 1))
fi
rm -f "$LEXICON_BACKUP"

# --- Step 10: Per-package Hackage sdists + case-sensitivity check ---
# Hydra now ships per-package Hackage distributions (hydra-kernel, hydra-haskell,
# and the hydra umbrella) rather than one monolithic `hydra` sdist (#418). We
# assemble all three (leaves first) on a case-sensitive volume — the Hackage
# build infra runs on Linux/ext4, and macOS HFS+/APFS is case-insensitive by
# default, masking case-only directory clashes — then extract and run
# `cabal v2-build --dry-run` per package to catch GHC-28623 module/path
# case-mismatches without the full compile cost.
step 10 $TOTAL_STEPS "Verifying per-package Hackage sdists on case-sensitive filesystem"
echo ""

cd "$HYDRA_ROOT"
SDIST_LOG="$LOG_DIR/hackage-sdist.log"
SDIST_OK=true
SDIST_WORK=""

# The 0.16.0 publish set (must match heads/haskell/bin/publish-hackage.sh).
HACKAGE_PKGS=(hydra-kernel hydra-haskell hydra)

case "$(uname -s)" in
    Darwin)
        SDIST_DMG="$(mktemp -t hydra-sdist-cs-XXXX).dmg"
        SDIST_MOUNT="$(mktemp -d -t hydra-sdist-mount-XXXX)"
        cleanup_sdist_dmg() {
            hdiutil detach "$SDIST_MOUNT" -quiet 2>/dev/null || true
            rm -f "$SDIST_DMG"
            rm -rf "$SDIST_MOUNT"
        }
        trap cleanup_sdist_dmg EXIT
        # 2000m holds three assembled sdists, their extracted trees, the
        # cabal dist-newstyle build dirs, and the per-package Haddock output.
        if ! hdiutil create -size 2000m -fs "Case-sensitive HFS+" \
             -volname HydraSdist "$SDIST_DMG" -quiet >"$SDIST_LOG" 2>&1; then
            echo "  FAIL: Could not create case-sensitive disk image (see verify-logs/hackage-sdist.log)"
            ERRORS=$((ERRORS + 1))
            SDIST_OK=false
        elif ! hdiutil attach "$SDIST_DMG" -mountpoint "$SDIST_MOUNT" \
             -quiet >>"$SDIST_LOG" 2>&1; then
            echo "  FAIL: Could not attach case-sensitive disk image (see verify-logs/hackage-sdist.log)"
            ERRORS=$((ERRORS + 1))
            SDIST_OK=false
        else
            SDIST_WORK="$SDIST_MOUNT"
        fi
        ;;
    *)
        SDIST_WORK="$(mktemp -d -t hydra-sdist-XXXX)"
        ;;
esac

# Assemble all three sdists (leaves first) + run the dependency-closure guard.
if [ "$SDIST_OK" = true ]; then
    if ! "$HYDRA_ROOT/heads/haskell/bin/publish-hackage.sh" \
         --out "$SDIST_WORK" >>"$SDIST_LOG" 2>&1; then
        echo "  FAIL: publish-hackage.sh assemble failed (see verify-logs/hackage-sdist.log)"
        ERRORS=$((ERRORS + 1))
        SDIST_OK=false
    fi
fi

# Per-package: extract + cabal dry-run. The dependents pin hydra-kernel ==
# <version> which is not yet on Hackage, so point cabal at the locally extracted
# sibling sdists via a cabal.project that lists all extracted package dirs.
if [ "$SDIST_OK" = true ]; then
    PROJECT_DIR="$SDIST_WORK/project"
    mkdir -p "$PROJECT_DIR"
    PROJECT_PKG_LINES=""
    for pkg in "${HACKAGE_PKGS[@]}"; do
        if ! ( cd "$SDIST_WORK" && tar -xzf "$pkg-${EXPECTED}.tar.gz" ) >>"$SDIST_LOG" 2>&1; then
            echo "  FAIL: Could not extract $pkg sdist (see verify-logs/hackage-sdist.log)"
            ERRORS=$((ERRORS + 1)); SDIST_OK=false; break
        fi
        PROJECT_PKG_LINES="$PROJECT_PKG_LINES  ../$pkg-${EXPECTED}/\n"
    done
    if [ "$SDIST_OK" = true ]; then
        printf "packages:\n%b" "$PROJECT_PKG_LINES" > "$PROJECT_DIR/cabal.project"
        if ( cd "$PROJECT_DIR" && cabal v2-build --dry-run all -w ghc-9.10.2 ) >>"$SDIST_LOG" 2>&1; then
            echo "  OK: all ${#HACKAGE_PKGS[@]} per-package sdists resolve cleanly on case-sensitive filesystem"
            for pkg in "${HACKAGE_PKGS[@]}"; do
                cp "$SDIST_WORK/$pkg-${EXPECTED}.tar.gz" "$ARTIFACT_DIR/$pkg-${EXPECTED}.tar.gz"
            done
        else
            echo "  FAIL: cabal v2-build --dry-run reported errors against the assembled"
            echo "        per-package sdists (see verify-logs/hackage-sdist.log)"
            ERRORS=$((ERRORS + 1)); SDIST_OK=false
        fi
    fi
fi

# --- Step 11: Per-package Haddock-for-Hackage docs build ---
# Hackage's auto-doc-builder is best-effort; pre-build docs per package so they
# can be uploaded via `cabal upload --documentation --publish` after release.
step 11 $TOTAL_STEPS "Building per-package Haddock-for-Hackage docs"
echo ""

DOC_LOG="$LOG_DIR/haddock.log"
DOC_OK=false

if [ "$SDIST_OK" = true ]; then
    DOC_FAIL=false
    DOC_BUILT=0
    : > "$DOC_LOG"
    for pkg in "${HACKAGE_PKGS[@]}"; do
        PKG_EXTRACT="$SDIST_WORK/$pkg-${EXPECTED}"
        if ( cd "$SDIST_WORK/project" \
             && cabal v2-haddock --haddock-for-hackage --enable-doc \
                -w ghc-9.10.2 "$pkg" ) >>"$DOC_LOG" 2>&1; then
            DOC_TARBALL="$(find "$SDIST_WORK/project/dist-newstyle" \
                -name "$pkg-${EXPECTED}-docs.tar.gz" | head -n 1)"
            if [ -n "$DOC_TARBALL" ] && [ -f "$DOC_TARBALL" ]; then
                cp "$DOC_TARBALL" "$ARTIFACT_DIR/$pkg-${EXPECTED}-docs.tar.gz"
                echo "  OK: $pkg Haddock docs built"
                DOC_BUILT=$((DOC_BUILT + 1))
            else
                echo "  FAIL: $pkg haddock succeeded but produced no docs tarball (see verify-logs/haddock.log)"
                DOC_FAIL=true
            fi
        else
            echo "  FAIL: $pkg cabal v2-haddock failed (see verify-logs/haddock.log)"
            DOC_FAIL=true
        fi
    done
    if [ "$DOC_FAIL" = true ]; then
        ERRORS=$((ERRORS + 1))
    elif [ "$DOC_BUILT" -eq "${#HACKAGE_PKGS[@]}" ]; then
        DOC_OK=true
    fi
else
    echo "  SKIP: skipped because Step 9 failed; haddock build needs buildable sdists"
    ERRORS=$((ERRORS + 1))
fi

# --- Step 12: Canonical source archive + checksum + signature ---
# The Apache "release of record" is a single source archive, not the per-registry
# convenience artifacts (Hackage sdists, Maven jars, PyPI wheels). We build it with
# `git archive` from HEAD so it is exactly the tracked source at the release commit
# (clean, reproducible, no build cruft), then emit a SHA-512 checksum and a detached
# GPG signature.
#
# Gate vs. warning: a missing LICENSE/NOTICE or a failed archive/checksum is a hard
# ERROR (Apache requires both files in every source release). The signature degrades
# to a WARNING when no signing key is configured, so a developer can run this script
# outside a real release without a key. Set HYDRA_RELEASE_SIGNING_KEY to a gpg key
# id/email to choose the signing identity; otherwise gpg's default key is used.
step 12 $TOTAL_STEPS "Building canonical source archive (tarball + sha512 + signature)"
echo ""

cd "$HYDRA_ROOT"
SRC_LOG="$LOG_DIR/source-archive.log"
: > "$SRC_LOG"

SRC_ARCHIVE="$ARTIFACT_DIR/hydra-${EXPECTED}-src.tar.gz"
SRC_PREFIX="hydra-${EXPECTED}/"

# Apache requires LICENSE + NOTICE in the source release. Assert both are tracked
# (so `git archive` will include them) before building.
SRC_META_OK=true
for required in LICENSE NOTICE; do
    if ! git ls-files --error-unmatch "$required" >/dev/null 2>&1; then
        echo "  FAIL: $required is missing or untracked — required in an Apache source release"
        SRC_META_OK=false
    fi
done

if [ "$SRC_META_OK" != true ]; then
    ERRORS=$((ERRORS + 1))
else
    if git archive --format=tar.gz --prefix="$SRC_PREFIX" -o "$SRC_ARCHIVE" HEAD 2>>"$SRC_LOG"; then
        echo "  OK: source archive built -> release-artifacts/hydra-${EXPECTED}-src.tar.gz"

        # SHA-512 checksum (portable: shasum on macOS, sha512sum on Linux).
        if command -v sha512sum >/dev/null 2>&1; then
            ( cd "$ARTIFACT_DIR" && sha512sum "hydra-${EXPECTED}-src.tar.gz" > "hydra-${EXPECTED}-src.tar.gz.sha512" )
        else
            ( cd "$ARTIFACT_DIR" && shasum -a 512 "hydra-${EXPECTED}-src.tar.gz" > "hydra-${EXPECTED}-src.tar.gz.sha512" )
        fi
        echo "  OK: SHA-512 checksum written -> hydra-${EXPECTED}-src.tar.gz.sha512"

        # Detached GPG signature. Missing key / gpg => WARNING, not a gate.
        if ! command -v gpg >/dev/null 2>&1; then
            echo "  WARNING: gpg not found — source archive is unsigned (install gpg and re-run to sign)"
            WARNINGS=$((WARNINGS + 1))
        else
            GPG_KEY_ARGS=()
            if [ -n "${HYDRA_RELEASE_SIGNING_KEY:-}" ]; then
                GPG_KEY_ARGS=(--local-user "$HYDRA_RELEASE_SIGNING_KEY")
            fi
            # Expand defensively: under `set -u`, "${arr[@]}" on an EMPTY array is
            # an "unbound variable" error in bash <= 4.3 (macOS ships bash 3.2),
            # which would abort the whole script mid-Step-11 when no signing key is
            # set. The ${arr[@]+...} guard yields nothing for an empty array.
            if gpg ${GPG_KEY_ARGS[@]+"${GPG_KEY_ARGS[@]}"} --armor --detach-sign --yes \
                 --output "$SRC_ARCHIVE.asc" "$SRC_ARCHIVE" 2>>"$SRC_LOG"; then
                echo "  OK: detached signature written -> hydra-${EXPECTED}-src.tar.gz.asc"
            else
                echo "  WARNING: gpg signing failed (no key configured? see verify-logs/source-archive.log)"
                echo "           Set HYDRA_RELEASE_SIGNING_KEY or configure a gpg default key; the"
                echo "           release of record must be signed before a real release."
                WARNINGS=$((WARNINGS + 1))
            fi
        fi
    else
        echo "  FAIL: git archive failed (see verify-logs/source-archive.log)"
        ERRORS=$((ERRORS + 1))
    fi
fi

# --- Step 13: Per-host published-package self-containment ---
# RELEASE GATE. Each head ships a verify-distribution.sh that builds its
# publish-set packages from the dist/ tree ALONE and proves they are
# self-contained when consumed as PUBLISHED artifacts — installed/resolved in
# isolation from the worktree (Python: fresh venv + --no-index; Haskell: the
# trio staged into one stack project; Java: published to a temp Maven repo and
# resolved by an offline consumer). This is the class of break that shipped in
# 0.16.0 — a package importing something present only in heads/, not in the
# packaged dist/ — which every in-repo test masked because heads/ is always on
# the path/source-set. The per-host tests above (Steps 2-4) do NOT exercise the
# packaging boundary; this step does. See #472 (Python wheel) and #473 (Haskell
# cold-build). Java's verifier hard-fails when no JDK 17+ is present.
step 13 $TOTAL_STEPS "Verifying per-host published-package self-containment"
echo ""

for hv in \
    "haskell:$HYDRA_ROOT/heads/haskell/bin/verify-distribution.sh" \
    "python:$HYDRA_ROOT/heads/python/bin/verify-distribution.sh" \
    "java:$HYDRA_ROOT/heads/java/bin/verify-distribution.sh" \
    "typescript:$HYDRA_ROOT/heads/typescript/bin/verify-distribution.sh"; do
    host="${hv%%:*}"
    script="${hv##*:}"
    echo "--- $host: verify-distribution ---"
    if [ ! -x "$script" ]; then
        echo "  FAIL: missing verifier: $script"
        ERRORS=$((ERRORS + 1))
        continue
    fi
    if "$script" 2>&1 | tee "$LOG_DIR/verify-dist-$host.log"; then
        echo "  OK: $host distribution is self-contained"
    else
        echo "  FAIL: $host distribution self-containment check failed (see verify-logs/verify-dist-$host.log)"
        ERRORS=$((ERRORS + 1))
    fi
    echo ""
done

# --- Summary ---
echo ""
banner1 "Release Preparation Summary"
echo ""
echo "  Version:  $EXPECTED"
echo "  Errors:   $ERRORS"
echo "  Warnings: $WARNINGS"
echo ""

if [ $ERRORS -eq 0 ]; then
    echo "All checks passed! Release artifacts are ready in:"
    echo "  $ARTIFACT_DIR"
    echo ""
    echo "  - hydra-${EXPECTED}-src.tar.gz         (canonical source archive — release of record)"
    echo "  - hydra-${EXPECTED}-src.tar.gz.sha512  (SHA-512 checksum)"
    if [ -f "$ARTIFACT_DIR/hydra-${EXPECTED}-src.tar.gz.asc" ]; then
        echo "  - hydra-${EXPECTED}-src.tar.gz.asc     (detached GPG signature)"
    fi
    for pkg in "${HACKAGE_PKGS[@]}"; do
        echo "  - $pkg-${EXPECTED}.tar.gz       (Hackage sdist)"
        if [ "$DOC_OK" = true ]; then
            echo "  - $pkg-${EXPECTED}-docs.tar.gz  (Haddock-for-Hackage docs)"
        fi
    done
    echo ""
    if [ ! -f "$ARTIFACT_DIR/hydra-${EXPECTED}-src.tar.gz.asc" ]; then
        echo "  NOTE: the source archive is UNSIGNED (no gpg key configured). The release of"
        echo "        record must be signed — set HYDRA_RELEASE_SIGNING_KEY and re-run before"
        echo "        a real release. Verify keys are published in the repo-root KEYS file."
        echo ""
    fi
    echo "Next steps:"
    echo "  1. Update CHANGELOG.md"
    echo "  2. Commit all changes"
    echo "  3. Tag: git tag $EXPECTED -m '$EXPECTED release' HEAD"
    echo "  4. Push: git push && git push --tags"
    echo "  5. Publish (LEAVES FIRST — order matters; the umbrella pins its siblings):"
    echo "       heads/haskell/bin/publish-hackage.sh --publish"
    echo "     or manually, in this order:"
    for pkg in "${HACKAGE_PKGS[@]}"; do
        echo "       cabal upload --publish $ARTIFACT_DIR/$pkg-${EXPECTED}.tar.gz"
    done
    if [ "$DOC_OK" = true ]; then
        for pkg in "${HACKAGE_PKGS[@]}"; do
            echo "       cabal upload --documentation --publish $ARTIFACT_DIR/$pkg-${EXPECTED}-docs.tar.gz"
        done
    fi
    echo "       (then Maven Central, npm, and conda-forge per docs/release-workflow.md)"
else
    echo "FAIL: $ERRORS check(s) failed. Please fix before releasing."
    exit 1
fi
