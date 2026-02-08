#!/bin/bash
set -e

# Pre-release verification script for Hydra.
#
# Checks that all three implementations are consistent and passing:
#   1. Versions are synchronized across all implementation configs
#   2. Haskell tests pass (hydra-haskell and hydra-ext)
#   3. Java tests pass
#   4. Python tests pass
#   5. JSON kernel is up to date and round-trips correctly
#
# Prerequisites:
#   - Stack, Java 18+, Python 3.12+, and uv are installed
#   - Run from the repository root directory
#
# Usage:
#   ./bin/verify-release.sh          # Full verification
#   ./bin/verify-release.sh --help   # Show this help

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
            echo "  2. Haskell tests (hydra-haskell)"
            echo "  3. Haskell tests (hydra-ext)"
            echo "  4. Java build and tests"
            echo "  5. Python tests and code quality"
            echo "  6. JSON kernel verification"
            exit 0
            ;;
    esac
done

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/.." && pwd )"

cd "$HYDRA_ROOT"

ERRORS=0
WARNINGS=0

echo "=========================================="
echo "Hydra Release Verification"
echo "=========================================="
echo ""

# --- Step 1: Version synchronization ---
echo "Step 1/6: Checking version synchronization..."
echo ""

HASKELL_VERSION=$(grep '^version:' hydra-haskell/package.yaml | awk '{print $2}')
EXT_VERSION=$(grep '^version:' hydra-ext/package.yaml | awk '{print $2}')
JAVA_VERSION=$(grep "version = " build.gradle | head -1 | sed "s/.*version = '\\(.*\\)'/\\1/")
PYTHON_VERSION=$(grep '^version' hydra-python/pyproject.toml | sed 's/.*"\(.*\)"/\1/')

echo "  hydra-haskell/package.yaml:  $HASKELL_VERSION"
echo "  hydra-ext/package.yaml:      $EXT_VERSION"
echo "  build.gradle:                $JAVA_VERSION"
echo "  hydra-python/pyproject.toml: $PYTHON_VERSION"
echo ""

VERSION_MISMATCH=false
if [ "$HASKELL_VERSION" != "$EXT_VERSION" ]; then
    echo "  ERROR: hydra-haskell ($HASKELL_VERSION) != hydra-ext ($EXT_VERSION)"
    VERSION_MISMATCH=true
fi
if [ "$HASKELL_VERSION" != "$JAVA_VERSION" ]; then
    echo "  ERROR: hydra-haskell ($HASKELL_VERSION) != build.gradle ($JAVA_VERSION)"
    VERSION_MISMATCH=true
fi
if [ "$HASKELL_VERSION" != "$PYTHON_VERSION" ]; then
    echo "  ERROR: hydra-haskell ($HASKELL_VERSION) != pyproject.toml ($PYTHON_VERSION)"
    VERSION_MISMATCH=true
fi

if [ "$VERSION_MISMATCH" = true ]; then
    echo "  FAIL: Versions are not synchronized"
    ERRORS=$((ERRORS + 1))
else
    echo "  OK: All versions are $HASKELL_VERSION"
fi

# --- Step 2: Haskell tests (hydra-haskell) ---
echo ""
echo "Step 2/6: Running Haskell tests (hydra-haskell)..."
echo ""

cd "$HYDRA_ROOT/hydra-haskell"
if stack test 2>&1 | tee "$HYDRA_ROOT/verify-haskell.log"; then
    echo ""
    echo "  OK: hydra-haskell tests passed"
else
    echo ""
    echo "  FAIL: hydra-haskell tests failed (see verify-haskell.log)"
    ERRORS=$((ERRORS + 1))
fi

# --- Step 3: Haskell tests (hydra-ext) ---
echo ""
echo "Step 3/6: Running Haskell tests (hydra-ext)..."
echo ""

cd "$HYDRA_ROOT/hydra-ext"
if stack test 2>&1 | tee "$HYDRA_ROOT/verify-ext.log"; then
    echo ""
    echo "  OK: hydra-ext tests passed"
else
    echo ""
    echo "  FAIL: hydra-ext tests failed (see verify-ext.log)"
    ERRORS=$((ERRORS + 1))
fi

# --- Step 4: Java build and tests ---
echo ""
echo "Step 4/6: Running Java build and tests..."
echo ""

cd "$HYDRA_ROOT"
if ./gradlew test 2>&1 | tee "$HYDRA_ROOT/verify-java.log"; then
    echo ""
    echo "  OK: Java tests passed"
else
    echo ""
    echo "  FAIL: Java tests failed (see verify-java.log)"
    ERRORS=$((ERRORS + 1))
fi

# --- Step 5: Python tests and code quality ---
echo ""
echo "Step 5/6: Running Python tests and code quality checks..."
echo ""

cd "$HYDRA_ROOT/hydra-python"

PYTHON_OK=true

if uv run pytest 2>&1 | tee "$HYDRA_ROOT/verify-python.log"; then
    echo ""
    echo "  OK: Python tests passed"
else
    echo ""
    echo "  FAIL: Python tests failed (see verify-python.log)"
    PYTHON_OK=false
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

# --- Step 6: JSON kernel verification ---
echo ""
echo "Step 6/6: Verifying JSON kernel..."
echo ""

cd "$HYDRA_ROOT/hydra-haskell"
if stack exec verify-json-kernel -- +RTS -K256M -A32M -RTS 2>&1; then
    echo ""
    echo "  OK: JSON kernel verification passed"
else
    echo ""
    echo "  FAIL: JSON kernel verification failed"
    ERRORS=$((ERRORS + 1))
fi

# --- Summary ---
echo ""
echo "=========================================="
echo "Verification Summary"
echo "=========================================="
echo ""
echo "  Version: $HASKELL_VERSION"
echo "  Errors:  $ERRORS"
echo "  Warnings: $WARNINGS"
echo ""

if [ $ERRORS -eq 0 ]; then
    echo "All checks passed! Ready for release."
    echo ""
    echo "Next steps:"
    echo "  1. Update CHANGELOG.md"
    echo "  2. Commit all changes"
    echo "  3. Tag: git tag $HASKELL_VERSION -m '$HASKELL_VERSION release' HEAD"
    echo "  4. Push: git push && git push --tags"
    echo "  5. Publish to Hackage, Sonatype, and PyPI"
else
    echo "FAIL: $ERRORS check(s) failed. Please fix before releasing."
    exit 1
fi
