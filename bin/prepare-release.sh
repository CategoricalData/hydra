#!/usr/bin/env bash
set -euo pipefail

# Pre-release preparation script for Hydra.
#
# Verifies that all implementations are consistent and passing, then
# produces upload-ready release artifacts:
#   1. Versions are synchronized across all implementation configs
#   2. Haskell tests pass (the hydra-test Stack target covers hydra-kernel
#      plus the hydra-ext content, since hydra-ext is a Hydra package
#      shipped as part of the hydra Stack library after #290)
#   3. Java tests pass
#   4. Python tests pass
#   5. Scala tests pass
#   6. Lisp tests pass (Clojure, Common Lisp, Emacs Lisp, Scheme)
#   7. JSON kernel is up to date and round-trips correctly
#   8. Lexicon is up to date (docs/hydra-lexicon.txt matches the
#      current Haskell kernel; release must not ship a stale lexicon)
#   9. Hackage sdist assembles cleanly on a case-sensitive filesystem
#      (catches case-clashes that macOS HFS+/APFS hides locally) and
#      passes `cabal v2-build --dry-run`
#  10. Haddock-for-Hackage docs build cleanly from the assembled sdist;
#      produces hydra-<version>-docs.tar.gz ready for `cabal upload`
#
# On success, the upload-ready artifacts (sdist + docs tarball) land in
# release-artifacts/ at the repo root.
#
# Prerequisites:
#   - Stack, Java 11+, Python 3.12+, uv, sbt, Clojure, SBCL, Emacs, Guile,
#     cabal-install (with GHC 9.10+), and (on macOS) hdiutil are installed
#   - Run from the repository root directory
#
# Usage:
#   ./bin/prepare-release.sh          # Full preparation
#   ./bin/prepare-release.sh --help   # Show this help

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/.." && pwd )"

source "$HYDRA_ROOT/bin/lib/common.sh"

for arg in "$@"; do
    case $arg in
        --help|-h)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Pre-release preparation for all Hydra implementations."
            echo ""
            echo "Options:"
            echo "  --help     Show this help message"
            echo ""
            echo "Steps performed:"
            echo "  1.  Version synchronization across all implementations"
            echo "  2.  Haskell tests"
            echo "  3.  Java build and tests"
            echo "  4.  Python tests and code quality"
            echo "  5.  Scala build and tests"
            echo "  6.  Lisp tests (Clojure, Common Lisp, Emacs Lisp, Scheme)"
            echo "  7.  JSON kernel verification"
            echo "  8.  Lexicon freshness"
            echo "  9.  Hackage sdist case-sensitivity check"
            echo "  10. Haddock-for-Hackage docs build"
            echo ""
            echo "Logs land in verify-logs/; upload-ready artifacts in"
            echo "release-artifacts/."
            exit 0
            ;;
        *)
            die "Unknown argument: $arg (try --help)"
            ;;
    esac
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
JAVA_VERSION=$(grep "version = " build.gradle | head -1 | sed "s/.*version = '\\(.*\\)'/\\1/")
BOOT_JAVA_VERSION=$(grep "version = " demos/bootstrapping/resources/java/build.gradle | head -1 | sed "s/.*version = '\\(.*\\)'/\\1/")
PYTHON_VERSION=$(grep '^version' heads/python/pyproject.toml | sed 's/.*"\(.*\)"/\1/')
BOOT_PYTHON_VERSION=$(grep '^version' demos/bootstrapping/resources/python/pyproject.toml | sed 's/.*"\(.*\)"/\1/')
SCALA_VERSION=$(grep 'version :=' packages/hydra-scala/build.sbt | sed 's/.*"\(.*\)".*/\1/' 2>/dev/null || echo "")
PIXI_VERSION=$(grep '^version' pixi.toml | sed 's/.*"\(.*\)"/\1/')

echo "  VERSION:                              $CANONICAL_VERSION"
echo "  heads/haskell/package.yaml:           $HASKELL_VERSION"
echo "  demos/bootstrapping/.../haskell:      $BOOT_HASKELL_VERSION"
echo "  build.gradle:                         $JAVA_VERSION"
echo "  demos/bootstrapping/.../java:         $BOOT_JAVA_VERSION"
echo "  heads/python/pyproject.toml:          $PYTHON_VERSION"
echo "  demos/bootstrapping/.../python:       $BOOT_PYTHON_VERSION"
echo "  packages/hydra-scala/build.sbt:       $SCALA_VERSION"
echo "  pixi.toml:                            $PIXI_VERSION"
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
    "build.gradle:$JAVA_VERSION" \
    "demos/bootstrapping/.../java:$BOOT_JAVA_VERSION" \
    "heads/python/pyproject.toml:$PYTHON_VERSION" \
    "demos/bootstrapping/.../python:$BOOT_PYTHON_VERSION" \
    "packages/hydra-scala/build.sbt:$SCALA_VERSION" \
    "pixi.toml:$PIXI_VERSION"; do
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
step 3 $TOTAL_STEPS "Running Java build and tests"
echo ""

cd "$HYDRA_ROOT"
if ./gradlew test 2>&1 | tee "$LOG_DIR/java.log"; then
    echo ""
    echo "  OK: Java tests passed"
else
    echo ""
    echo "  FAIL: Java tests failed (see verify-logs/java.log)"
    ERRORS=$((ERRORS + 1))
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

# --- Step 5: Scala build and tests ---
step 5 $TOTAL_STEPS "Running Scala build and tests"
echo ""

cd "$HYDRA_ROOT/packages/hydra-scala"
if sbt test 2>&1 | tee "$LOG_DIR/scala.log"; then
    echo ""
    echo "  OK: Scala tests passed"
else
    echo ""
    echo "  FAIL: Scala tests failed (see verify-logs/scala.log)"
    ERRORS=$((ERRORS + 1))
fi

# --- Step 6: Lisp tests ---
# Each dialect runs through the unified packages/hydra-lisp/bin/run-tests.sh
# runner. Per-dialect run-tests.sh scripts no longer exist post-#290.
step 6 $TOTAL_STEPS "Running Lisp tests"
echo ""

cd "$HYDRA_ROOT"
LISP_RUNNER="$HYDRA_ROOT/packages/hydra-lisp/bin/run-tests.sh"

for dialect in clojure common-lisp emacs-lisp scheme; do
    echo "  Testing $dialect..."
    if bash "$LISP_RUNNER" "$dialect" 2>&1 | tee "$LOG_DIR/${dialect}.log" | tail -3; then
        echo "  OK: $dialect tests passed"
    else
        echo "  FAIL: $dialect tests failed (see verify-logs/${dialect}.log)"
        ERRORS=$((ERRORS + 1))
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

# --- Step 9: Hackage sdist + case-sensitivity check ---
# The Hackage build infrastructure runs on Linux (case-sensitive ext4).
# macOS HFS+/APFS is case-insensitive by default, which masks duplicate
# directories that differ only in case (e.g. Hydra/Demos/GenPG/ vs
# Hydra/Demos/Genpg/). We assemble the sdist on a case-sensitive volume
# so any such clash surfaces, then run cabal v2-build --dry-run to catch
# GHC-28623 ("file name does not match module name") errors without the
# full compile cost.
step 9 $TOTAL_STEPS "Verifying Hackage sdist on case-sensitive filesystem"
echo ""

cd "$HYDRA_ROOT"
SDIST_LOG="$LOG_DIR/hackage-sdist.log"
SDIST_OK=true
SDIST_WORK=""

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
        if ! hdiutil create -size 500m -fs "Case-sensitive HFS+" \
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

if [ "$SDIST_OK" = true ]; then
    if ! "$HYDRA_ROOT/heads/haskell/bin/assemble-hackage-sdist.sh" \
         --out "$SDIST_WORK" >>"$SDIST_LOG" 2>&1; then
        echo "  FAIL: assemble-hackage-sdist.sh failed (see verify-logs/hackage-sdist.log)"
        ERRORS=$((ERRORS + 1))
        SDIST_OK=false
    fi
fi

SDIST_TARBALL="$SDIST_WORK/hydra-${EXPECTED}.tar.gz"
SDIST_EXTRACT="$SDIST_WORK/hydra-${EXPECTED}"

if [ "$SDIST_OK" = true ]; then
    if ! ( cd "$SDIST_WORK" && tar -xzf "hydra-${EXPECTED}.tar.gz" ) \
         >>"$SDIST_LOG" 2>&1; then
        echo "  FAIL: Could not extract assembled sdist (see verify-logs/hackage-sdist.log)"
        ERRORS=$((ERRORS + 1))
        SDIST_OK=false
    fi
fi

if [ "$SDIST_OK" = true ]; then
    if ( cd "$SDIST_EXTRACT" \
         && cabal v2-build --dry-run all -w ghc-9.10.2 ) \
         >>"$SDIST_LOG" 2>&1; then
        echo "  OK: Hackage sdist resolves cleanly on case-sensitive filesystem"
        # Stage the validated sdist for upload.
        cp "$SDIST_TARBALL" "$ARTIFACT_DIR/hydra-${EXPECTED}.tar.gz"
    else
        echo "  FAIL: cabal v2-build --dry-run reported errors against the assembled"
        echo "        sdist (likely module-name/path case-mismatch; see"
        echo "        verify-logs/hackage-sdist.log)"
        ERRORS=$((ERRORS + 1))
        SDIST_OK=false
    fi
fi

# --- Step 10: Haddock-for-Hackage docs build ---
# Hackage's auto-doc-builder is best-effort and frequently fails on
# packages with many transitive deps. We pre-build the docs from the
# already-validated sdist so we can upload them via
# `cabal upload --documentation --publish` after release.
step 10 $TOTAL_STEPS "Building Haddock-for-Hackage docs"
echo ""

DOC_LOG="$LOG_DIR/haddock.log"
DOC_OK=false

if [ "$SDIST_OK" = true ]; then
    if ( cd "$SDIST_EXTRACT" \
         && cabal v2-haddock --haddock-for-hackage --enable-doc \
            -w ghc-9.10.2 all ) >"$DOC_LOG" 2>&1; then
        DOC_TARBALL="$(find "$SDIST_EXTRACT/dist-newstyle" \
            -name "hydra-${EXPECTED}-docs.tar.gz" | head -n 1)"
        if [ -n "$DOC_TARBALL" ] && [ -f "$DOC_TARBALL" ]; then
            cp "$DOC_TARBALL" "$ARTIFACT_DIR/hydra-${EXPECTED}-docs.tar.gz"
            DOC_SIZE_BYTES="$(stat -f%z "$ARTIFACT_DIR/hydra-${EXPECTED}-docs.tar.gz" 2>/dev/null \
                || stat -c%s "$ARTIFACT_DIR/hydra-${EXPECTED}-docs.tar.gz")"
            DOC_SIZE_MB=$(( DOC_SIZE_BYTES / 1024 / 1024 ))
            echo "  OK: Haddock-for-Hackage docs built (${DOC_SIZE_MB} MB)"
            DOC_OK=true
        else
            echo "  FAIL: cabal v2-haddock succeeded but produced no docs tarball (see verify-logs/haddock.log)"
            ERRORS=$((ERRORS + 1))
        fi
    else
        echo "  FAIL: cabal v2-haddock failed (see verify-logs/haddock.log)"
        ERRORS=$((ERRORS + 1))
    fi
else
    echo "  SKIP: skipped because Step 9 failed; haddock build needs a buildable sdist"
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
    echo "  - hydra-${EXPECTED}.tar.gz       (Hackage sdist)"
    if [ "$DOC_OK" = true ]; then
        echo "  - hydra-${EXPECTED}-docs.tar.gz  (Haddock-for-Hackage docs)"
    fi
    echo ""
    echo "Next steps:"
    echo "  1. Update CHANGELOG.md"
    echo "  2. Commit all changes"
    echo "  3. Tag: git tag $EXPECTED -m '$EXPECTED release' HEAD"
    echo "  4. Push: git push && git push --tags"
    echo "  5. Publish:"
    echo "       cabal upload --publish $ARTIFACT_DIR/hydra-${EXPECTED}.tar.gz"
    echo "       cabal upload --documentation --publish \\"
    echo "                    $ARTIFACT_DIR/hydra-${EXPECTED}-docs.tar.gz"
    echo "       (then Maven Central and conda-forge per Release-process.md)"
else
    echo "FAIL: $ERRORS check(s) failed. Please fix before releasing."
    exit 1
fi
