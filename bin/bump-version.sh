#!/usr/bin/env bash
# Propagate the version from the VERSION file to all implementation config files.
#
# Usage:
#   bin/bump-version.sh          # Read VERSION and patch all config files
#   bin/bump-version.sh 0.13.0   # Set VERSION to 0.13.0 and patch all config files

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
VERSION_FILE="$REPO_ROOT/VERSION"

if [ $# -ge 1 ]; then
    echo "$1" > "$VERSION_FILE"
fi

VERSION=$(tr -d '[:space:]' < "$VERSION_FILE")

if [ -z "$VERSION" ]; then
    echo "Error: VERSION file is empty"
    exit 1
fi

# Validate format
if ! echo "$VERSION" | grep -qE '^[0-9]+\.[0-9]+\.[0-9]+$'; then
    echo "Error: '$VERSION' does not look like a version (expected X.Y.Z)"
    exit 1
fi

echo "Setting version to $VERSION"
echo

# Files to patch: (file, sed pattern)
patch() {
    local file="$1"
    local pattern="$2"
    local relpath="${file#$REPO_ROOT/}"

    if [ ! -f "$file" ]; then
        echo "  SKIP  $relpath (not found)"
        return
    fi

    if sed -i '' "$pattern" "$file" 2>/dev/null; then
        echo "  OK    $relpath"
    else
        echo "  FAIL  $relpath"
    fi
}

# Haskell (package.yaml): version line near the top
patch "$REPO_ROOT/hydra-haskell/package.yaml" \
    "s/^version:.*$/version:      $VERSION/"

patch "$REPO_ROOT/hydra-ext/package.yaml" \
    "s/^version:.*$/version:    $VERSION/"

patch "$REPO_ROOT/hydra-ext/demos/bootstrapping/resources/haskell/package.yaml" \
    "s/^version:.*$/version:      $VERSION/"

# Java (build.gradle): version = 'X.Y.Z'
patch "$REPO_ROOT/build.gradle" \
    "s/version = '.*'/version = '$VERSION'/"

patch "$REPO_ROOT/hydra-ext/demos/bootstrapping/resources/java/build.gradle" \
    "s/version = '.*'/version = '$VERSION'/"

# Python (pyproject.toml): version = "X.Y.Z"
patch "$REPO_ROOT/hydra-python/pyproject.toml" \
    "s/^version = \".*\"/version = \"$VERSION\"/"

patch "$REPO_ROOT/hydra-ext/demos/bootstrapping/resources/python/pyproject.toml" \
    "s/^version = \".*\"/version = \"$VERSION\"/"

echo
echo "Done. Verify with: git diff"
