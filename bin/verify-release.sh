#!/usr/bin/env bash
set -euo pipefail

# Pre-release verification script for Hydra.
#
# Checks that all implementations are consistent and passing:
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
#
# Prerequisites:
#   - Stack, Java 11+, Python 3.12+, uv, sbt, Clojure, SBCL, Emacs, and Guile are installed
#   - Run from the repository root directory
#
# Usage:
#   ./bin/verify-release.sh          # Full verification
#   ./bin/verify-release.sh --help   # Show this help

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/.." && pwd )"

source "$HYDRA_ROOT/bin/lib/common.sh"

for arg in "$@"; do
    case $arg in
        --help|-h)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Pre-release verification for all Hydra implementations."
            echo ""
            echo "Options:"
            echo "  --help     Show this help message"
            echo ""
            echo "Checks performed:"
            echo "  1. Version synchronization across all implementations"
            echo "  2. Haskell tests"
            echo "  3. Java build and tests"
            echo "  4. Python tests and code quality"
            echo "  5. Scala build and tests"
            echo "  6. Lisp tests (Clojure, Common Lisp, Emacs Lisp, Scheme)"
            echo "  7. JSON kernel verification"
            echo "  8. Lexicon freshness"
            echo ""
            echo "Logs are written to verify-logs/."
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

TOTAL_STEPS=8

ERRORS=0
WARNINGS=0

banner1 "Hydra Release Verification"
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

# --- Summary ---
echo ""
banner1 "Verification Summary"
echo ""
echo "  Version: $EXPECTED"
echo "  Errors:  $ERRORS"
echo "  Warnings: $WARNINGS"
echo ""

if [ $ERRORS -eq 0 ]; then
    echo "All checks passed! Ready for release."
    echo ""
    echo "Next steps:"
    echo "  1. Update CHANGELOG.md"
    echo "  2. Commit all changes"
    echo "  3. Tag: git tag $EXPECTED -m '$EXPECTED release' HEAD"
    echo "  4. Push: git push && git push --tags"
    echo "  5. Publish to Hackage, Maven Central, and conda-forge"
else
    echo "FAIL: $ERRORS check(s) failed. Please fix before releasing."
    exit 1
fi
