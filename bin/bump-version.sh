#!/usr/bin/env bash
# Propagate the version from the VERSION file to all implementation config files.
#
# Usage:
#   bin/bump-version.sh          # Read VERSION and patch all config files
#   bin/bump-version.sh 0.16.0   # Set VERSION to 0.16.0 and patch all config files
#   bin/bump-version.sh --help
#
# Files patched:
#   heads/haskell/package.yaml
#   demos/bootstrapping/resources/haskell/package.yaml
#   build.gradle
#   demos/bootstrapping/resources/java/build.gradle
#   packages/hydra-scala/build.sbt
#   heads/python/pyproject.toml
#   demos/bootstrapping/resources/python/pyproject.toml
#   pixi.toml
#   README.md

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
REPO_ROOT="$( cd "$SCRIPT_DIR/.." && pwd )"

source "$REPO_ROOT/bin/lib/common.sh"

VERSION_FILE="$REPO_ROOT/VERSION"

case "${1:-}" in
    --help|-h)
        sed -n '2,/^$/p' "$0" | sed 's/^# \{0,1\}//'
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
patch "$REPO_ROOT/heads/haskell/package.yaml" \
    "s/^version:.*$/version:      $VERSION/"

patch "$REPO_ROOT/demos/bootstrapping/resources/haskell/package.yaml" \
    "s/^version:.*$/version:      $VERSION/"

# Java (build.gradle): version = 'X.Y.Z'
patch "$REPO_ROOT/build.gradle" \
    "s/version = '.*'/version = '$VERSION'/"

patch "$REPO_ROOT/demos/bootstrapping/resources/java/build.gradle" \
    "s/version = '.*'/version = '$VERSION'/"

# Scala (build.sbt): version := "X.Y.Z"
patch "$REPO_ROOT/packages/hydra-scala/build.sbt" \
    "s/version := \".*\"/version := \"$VERSION\"/"

# Python (pyproject.toml): version = "X.Y.Z"
patch "$REPO_ROOT/heads/python/pyproject.toml" \
    "s/^version = \".*\"/version = \"$VERSION\"/"

patch "$REPO_ROOT/demos/bootstrapping/resources/python/pyproject.toml" \
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
