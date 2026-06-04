#!/usr/bin/env bash
# Pre-release preparation script for Hydra.
#
# Verifies that the RELEASED SET is consistent and passing, then produces
# upload-ready release artifacts.
#
# Release gate vs. quality check (the cohesive principle, #418): the script
# HARD-FAILS (blocks the release) only on artifacts that 0.16.0 actually ships —
# Haskell (Hackage: hydra-kernel/hydra-haskell/hydra), Java (Maven Central, via
# the hydra-java rollup), Python (PyPI/conda) — plus version sync, the JSON
# kernel, and lexicon freshness. Targets that are NOT released yet (Scala, the
# Lisp dialects, and the Java bindings under bindings/java/) are run as
# non-blocking QUALITY CHECKS: a failure there is a WARNING, not an ERROR. When
# one of those becomes a released artifact, promote its step from WARNING to
# ERROR (gate).
#
# Steps performed:
#   1.  Version synchronization across all implementations                 [gate]
#   2.  Haskell tests (hydra-test Stack target: hydra-kernel + hydra-ext)  [gate]
#   3.  Java: hydra-java rollup tests [gate]; bindings build [quality check]
#   4.  Python tests [gate] + ruff code quality [quality check]
#   5.  Scala build and tests                                    [quality check]
#   6.  Lisp tests (Clojure, Common Lisp, Emacs Lisp, Scheme)    [quality check]
#   7.  JSON kernel verification (round-trips vs the in-memory kernel)     [gate]
#   8.  Lexicon freshness (docs/hydra-lexicon.txt matches the kernel)      [gate]
#   9.  Per-package Hackage sdist case-sensitivity check (assembles cleanly
#       on a case-sensitive filesystem + `cabal v2-build --dry-run`)       [gate]
#   10. Per-package Haddock-for-Hackage docs build (ready for `cabal upload`)[gate]
#
# On success, the upload-ready per-package artifacts (sdists + docs tarballs)
# land in release-artifacts/ at the repo root. Logs land in verify-logs/.
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

TOTAL_STEPS=10

ERRORS=0
WARNINGS=0

banner1 "Hydra Release Preparation"
echo ""

step 1 $TOTAL_STEPS "Checking version synchronization"
echo ""

CANONICAL_VERSION=$(tr -d '[:space:]' < VERSION 2>/dev/null || echo "")
HASKELL_VERSION=$(grep '^version:' heads/haskell/package.yaml | awk '{print $2}')
BOOT_HASKELL_VERSION=$(grep '^version:' demos/bootstrapping/resources/haskell/package.yaml | awk '{print $2}')
JAVA_VERSION=$(grep "version = " heads/java/build.gradle | head -1 | sed "s/.*version = '\\(.*\\)'/\\1/")
BOOT_JAVA_VERSION=$(grep "version = " demos/bootstrapping/resources/java/build.gradle | head -1 | sed "s/.*version = '\\(.*\\)'/\\1/")
PYTHON_VERSION=$(grep '^version' heads/python/pyproject.toml | sed 's/.*"\(.*\)"/\1/')
BOOT_PYTHON_VERSION=$(grep '^version' demos/bootstrapping/resources/python/pyproject.toml | sed 's/.*"\(.*\)"/\1/')
SCALA_VERSION=$(grep 'version :=' packages/hydra-scala/build.sbt | sed 's/.*"\(.*\)".*/\1/' 2>/dev/null || echo "")

echo "  VERSION:                              $CANONICAL_VERSION"
echo "  heads/haskell/package.yaml:           $HASKELL_VERSION"
echo "  demos/bootstrapping/.../haskell:      $BOOT_HASKELL_VERSION"
echo "  heads/java/build.gradle:              $JAVA_VERSION"
echo "  demos/bootstrapping/.../java:         $BOOT_JAVA_VERSION"
echo "  heads/python/pyproject.toml:          $PYTHON_VERSION"
echo "  demos/bootstrapping/.../python:       $BOOT_PYTHON_VERSION"
echo "  packages/hydra-scala/build.sbt:       $SCALA_VERSION"
echo ""

EXPECTED="$CANONICAL_VERSION"
if [ -z "$EXPECTED" ]; then
    echo "  ERROR: VERSION file is missing or empty"
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

# --- Step 5: Scala build and tests (quality check, NOT a release gate) ---
# Scala is not a released artifact (nothing is published to Maven Central / any
# registry for the Scala target), so a Scala failure is a non-blocking WARNING,
# not a release gate. When Scala becomes a released target, promote this to a
# gate (ERRORS). (#418)
step 5 $TOTAL_STEPS "Running Scala build and tests (quality check)"
echo ""

cd "$HYDRA_ROOT/packages/hydra-scala"
if sbt test 2>&1 | tee "$LOG_DIR/scala.log"; then
    echo ""
    echo "  OK: Scala tests passed"
else
    echo ""
    echo "  WARNING: Scala tests failed (not a release gate; see verify-logs/scala.log)"
    WARNINGS=$((WARNINGS + 1))
fi

# --- Step 6: Lisp tests (quality check, NOT a release gate) ---
# The Lisp dialects (Clojure, Common Lisp, Emacs Lisp, Scheme) are not released
# artifacts (nothing published to Clojars / any registry), so failures here are
# non-blocking WARNINGS, not release gates. Each dialect runs through the unified
# packages/hydra-lisp/bin/run-tests.sh runner. (#418)
# NOTE: the runner is known to under-report (it can exit 0 despite individual
# test failures); since Lisp is non-gating that does not affect the release
# decision, but it is tracked separately as a runner bug.
step 6 $TOTAL_STEPS "Running Lisp tests (quality check)"
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

# --- Step 7: JSON kernel verification ---
step 7 $TOTAL_STEPS "Verifying JSON kernel"
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

# --- Step 8: Lexicon freshness ---
step 8 $TOTAL_STEPS "Verifying lexicon freshness"
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

# --- Step 9: Per-package Hackage sdists + case-sensitivity check ---
# Hydra now ships per-package Hackage distributions (hydra-kernel, hydra-haskell,
# and the hydra umbrella) rather than one monolithic `hydra` sdist (#418). We
# assemble all three (leaves first) on a case-sensitive volume — the Hackage
# build infra runs on Linux/ext4, and macOS HFS+/APFS is case-insensitive by
# default, masking case-only directory clashes — then extract and run
# `cabal v2-build --dry-run` per package to catch GHC-28623 module/path
# case-mismatches without the full compile cost.
step 9 $TOTAL_STEPS "Verifying per-package Hackage sdists on case-sensitive filesystem"
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

# --- Step 10: Per-package Haddock-for-Hackage docs build ---
# Hackage's auto-doc-builder is best-effort; pre-build docs per package so they
# can be uploaded via `cabal upload --documentation --publish` after release.
step 10 $TOTAL_STEPS "Building per-package Haddock-for-Hackage docs"
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
    for pkg in "${HACKAGE_PKGS[@]}"; do
        echo "  - $pkg-${EXPECTED}.tar.gz       (Hackage sdist)"
        if [ "$DOC_OK" = true ]; then
            echo "  - $pkg-${EXPECTED}-docs.tar.gz  (Haddock-for-Hackage docs)"
        fi
    done
    echo ""
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
    echo "       (then Maven Central and conda-forge per Release-process.md)"
else
    echo "FAIL: $ERRORS check(s) failed. Please fix before releasing."
    exit 1
fi
