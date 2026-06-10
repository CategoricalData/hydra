#!/usr/bin/env bash
# Propagate the CURRENT version (hydra.json:currentVersion) to all implementation
# config files. hydra.json is the single source of truth for the version; the old
# standalone VERSION file has been retired (#347).
#
# This bumps the package *release* version — each host's own package version and
# its inter-package release dependency coordinates. It does NOT touch hostVersion
# (the published-host version the build/sync depends on) — that has its own bump
# script, bin/bump-host-version.sh.
#
# Usage:
#   bin/bump-version.sh          # Read currentVersion and patch all config files
#   bin/bump-version.sh 0.16.0   # Set currentVersion to 0.16.0 and patch all config files
#   bin/bump-version.sh --help
#
# Files patched:
#   heads/haskell/package.yaml
#   demos/bootstrapping/resources/haskell/package.yaml
#   heads/java/build.gradle
#   demos/bootstrapping/resources/java/build.gradle
#   packages/hydra-scala/build.sbt
#   heads/python/pyproject.toml
#   demos/bootstrapping/resources/python/pyproject.toml
#   README.md

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
REPO_ROOT="$( cd "$SCRIPT_DIR/.." && pwd )"

source "$REPO_ROOT/bin/lib/common.sh"

PACKAGES_PY="$REPO_ROOT/bin/lib/hydra-packages.py"

case "${1:-}" in
    --help|-h)
        sed -n '2,/^$/p' "$0" | sed 's/^# \{0,1\}//'
        exit 0
        ;;
esac

# Validate the requested version before writing it.
if [ $# -ge 1 ]; then
    if ! echo "$1" | grep -qE '^[0-9]+\.[0-9]+\.[0-9]+$'; then
        die "'$1' does not look like a version (expected X.Y.Z)"
    fi
    "$PACKAGES_PY" set-current-version "$1"
fi

VERSION=$("$PACKAGES_PY" current-version)

if [ -z "$VERSION" ]; then
    die "hydra.json:currentVersion is empty"
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
patch "$REPO_ROOT/heads/java/build.gradle" \
    "s/version = '.*'/version = '$VERSION'/"

patch "$REPO_ROOT/demos/bootstrapping/resources/java/build.gradle" \
    "s/version = '.*'/version = '$VERSION'/"

# Scala (build.sbt): version := "X.Y.Z"
patch "$REPO_ROOT/packages/hydra-scala/build.sbt" \
    "s/version := \".*\"/version := \"$VERSION\"/"

patch "$REPO_ROOT/demos/bootstrapping/resources/scala/build.sbt" \
    "s/version := \".*\"/version := \"$VERSION\"/"

# Python (pyproject.toml): version = "X.Y.Z"
patch "$REPO_ROOT/heads/python/pyproject.toml" \
    "s/^version = \".*\"/version = \"$VERSION\"/"

patch "$REPO_ROOT/demos/bootstrapping/resources/python/pyproject.toml" \
    "s/^version = \".*\"/version = \"$VERSION\"/"

# TypeScript (package.json): "version": "X.Y.Z"
patch "$REPO_ROOT/heads/typescript/package.json" \
    "s/\"version\": \"[0-9][0-9]*\.[0-9][0-9]*\.[0-9][0-9]*\"/\"version\": \"$VERSION\"/"

patch "$REPO_ROOT/demos/bootstrapping/resources/typescript/package.json" \
    "s/\"version\": \"[0-9][0-9]*\.[0-9][0-9]*\.[0-9][0-9]*\"/\"version\": \"$VERSION\"/"

# README.md: version references.
# NOTE: README's "latest Hydra release" line states the latest *published* version
# and is intentionally NOT bumped here — bump it only when the artifacts are actually
# published (publish-before-tag), so the README never claims an unpublished release.
patch "$REPO_ROOT/README.md" \
    "s/The current release is \*\*[0-9][0-9]*\.[0-9][0-9]*\.[0-9][0-9]*\*\*/The current release is \*\*$VERSION\*\*/"
patch "$REPO_ROOT/README.md" \
    "s/hydra-java:[0-9][0-9]*\.[0-9][0-9]*\.[0-9][0-9]*/hydra-java:$VERSION/"
patch "$REPO_ROOT/README.md" \
    "s/hydra-ext:[0-9][0-9]*\.[0-9][0-9]*\.[0-9][0-9]*/hydra-ext:$VERSION/"

echo
echo "Done. Verify with: git diff"
