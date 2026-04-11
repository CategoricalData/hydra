#!/usr/bin/env bash
set -euo pipefail

# Propagate the version from the VERSION file to all implementation config files.
#
# Usage:
#   bin/bump-version.sh          # Read VERSION and patch all config files
#   bin/bump-version.sh 0.13.0   # Set VERSION to 0.13.0 and patch all config files
#   bin/bump-version.sh --help   # Show this help

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
REPO_ROOT="$( cd "$SCRIPT_DIR/.." && pwd )"

source "$REPO_ROOT/bin/lib/common.sh"

VERSION_FILE="$REPO_ROOT/VERSION"

case "${1:-}" in
    --help|-h)
        echo "Usage: $0 [NEW_VERSION]"
        echo ""
        echo "Propagate the version from the VERSION file to all implementation config files."
        echo ""
        echo "If NEW_VERSION is provided, it is written to VERSION first."
        echo ""
        echo "Files patched:"
        echo "  packages/hydra-kernel/package.yaml"
        echo "  packages/hydra-haskell/package.yaml"
        echo "  packages/hydra-ext/package.yaml"
        echo "  packages/hydra-ext/demos/bootstrapping/resources/haskell/package.yaml"
        echo "  build.gradle"
        echo "  packages/hydra-ext/demos/bootstrapping/resources/java/build.gradle"
        echo "  packages/hydra-scala/build.sbt"
        echo "  packages/hydra-python/pyproject.toml"
        echo "  packages/hydra-ext/demos/bootstrapping/resources/python/pyproject.toml"
        echo "  pixi.toml"
        echo "  README.md"
        exit 0
        ;;
esac

if [ $# -ge 1 ]; then
    echo "$1" > "$VERSION_FILE"
fi

VERSION=$(tr -d '[:space:]' < "$VERSION_FILE")

if [ -z "$VERSION" ]; then
    die "VERSION file is empty"
fi

# Validate format
if ! echo "$VERSION" | grep -qE '^[0-9]+\.[0-9]+\.[0-9]+$'; then
    die "'$VERSION' does not look like a version (expected X.Y.Z)"
fi

echo "Setting version to $VERSION"
echo

# Files to patch: (file, sed pattern)
# Verifies the pattern actually matched by checking content before and after.
patch() {
    local file="$1"
    local pattern="$2"
    local relpath="${file#$REPO_ROOT/}"

    if [ ! -f "$file" ]; then
        echo "  SKIP  $relpath (not found)"
        return
    fi

    local before
    before=$(cat "$file")
    if ! sed_inplace "$pattern" "$file" 2>/dev/null; then
        echo "  FAIL  $relpath (sed error)"
        return 1
    fi
    local after
    after=$(cat "$file")
    if [ "$before" = "$after" ]; then
        echo "  NOOP  $relpath (no match or already $VERSION)"
    else
        echo "  OK    $relpath"
    fi
}

# Haskell (package.yaml): version line near the top
patch "$REPO_ROOT/packages/hydra-kernel/package.yaml" \
    "s/^version:.*$/version:      $VERSION/"

patch "$REPO_ROOT/packages/hydra-haskell/package.yaml" \
    "s/^version:.*$/version:      $VERSION/"

patch "$REPO_ROOT/packages/hydra-ext/package.yaml" \
    "s/^version:.*$/version:    $VERSION/"

patch "$REPO_ROOT/packages/hydra-ext/demos/bootstrapping/resources/haskell/package.yaml" \
    "s/^version:.*$/version:      $VERSION/"

# Java (build.gradle): version = 'X.Y.Z'
patch "$REPO_ROOT/build.gradle" \
    "s/version = '.*'/version = '$VERSION'/"

patch "$REPO_ROOT/packages/hydra-ext/demos/bootstrapping/resources/java/build.gradle" \
    "s/version = '.*'/version = '$VERSION'/"

# Scala (build.sbt): version := "X.Y.Z"
patch "$REPO_ROOT/packages/hydra-scala/build.sbt" \
    "s/version := \".*\"/version := \"$VERSION\"/"

# Python (pyproject.toml): version = "X.Y.Z"
patch "$REPO_ROOT/packages/hydra-python/pyproject.toml" \
    "s/^version = \".*\"/version = \"$VERSION\"/"

patch "$REPO_ROOT/packages/hydra-ext/demos/bootstrapping/resources/python/pyproject.toml" \
    "s/^version = \".*\"/version = \"$VERSION\"/"

# Pixi (pixi.toml): version = "X.Y.Z"
patch "$REPO_ROOT/pixi.toml" \
    "s/^version = \".*\"/version = \"$VERSION\"/"

# README.md: version references
patch "$REPO_ROOT/README.md" \
    "s/The current release is \*\*[0-9][0-9]*\.[0-9][0-9]*\.[0-9][0-9]*\*\*/The current release is \*\*$VERSION\*\*/"
patch "$REPO_ROOT/README.md" \
    "s/hydra-java:[0-9][0-9]*\.[0-9][0-9]*\.[0-9][0-9]*/hydra-java:$VERSION/"
patch "$REPO_ROOT/README.md" \
    "s/hydra-ext:[0-9][0-9]*\.[0-9][0-9]*\.[0-9][0-9]*/hydra-ext:$VERSION/"

echo
echo "Done. Verify with: git diff"
