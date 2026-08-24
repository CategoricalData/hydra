#!/usr/bin/env bash
# #703: Java-host cold-seeder for dist/haskell. Replaces the retired Haskell
# cold-seeder (heads/haskell/json-driver/, deleted by #703).
#
# On a fresh checkout where dist/haskell/ is ABSENT, this seeds all 16 package
# source trees from dist/json (tracked) using the PUBLISHED Java host
# (net.fortytwo.hydra.java:hydra-{java,build,python,scala,typescript,lisp,haskell}),
# then emits each package's package.yaml manifest so every dist/haskell/<pkg>/ is
# a self-contained, buildable Haskell package — same Default A contract the old
# Haskell cold-seeder provided.
#
# Why this replaces the old cold-seeder: the retired ColdSeedMain compiled
# HEAD's Types DSL sources against a PUBLISHED hydra-kernel dependency —a link
# that breaks on every kernel-shape change (the recurring failure class #703
# was filed to kill; see docs/build-system.md). This driver instead runs
# hydra.TransformJsonToTarget (heads/java/target-driver/), a pure schema-walking
# JSON->Haskell-text decoder: it reads dist/json's term-AST and transcribes it,
# with no dependency on the shape of any compiled Haskell type. A kernel shape
# change changes the JSON shape; the transcription is agnostic to it.
#
# -PhostVersion (HYDRA_HOST_VERSION below) pins the published Java host version
# explicitly, bypassing a hostOverrides:local shim on hydra.json (java=local) —
# needed only because the shim blocks hydra-packages.py's normal host-version
# resolution, not because the cold-seed path itself is shimmed.
#
# Usage:
#   heads/java/bin/seed-dist-haskell.sh [--repo-root DIR]
#
# Env:
#   HYDRA_ROOT_DIR     worktree root (default: derived from this script's path)
#   HYDRA_HOST_VERSION published hydra-java host version to pin (default: read
#                      from hydra.json's hostVersion)
set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_JAVA_HEAD="$( cd "$SCRIPT_DIR/.." && pwd )"

REPO_ROOT="${HYDRA_ROOT_DIR:-}"
while [ $# -gt 0 ]; do
    case "$1" in
        --repo-root) REPO_ROOT="$2"; shift 2 ;;
        --repo-root=*) REPO_ROOT="${1#--repo-root=}"; shift ;;
        *) echo "Unknown argument: $1" >&2; exit 2 ;;
    esac
done
if [ -z "$REPO_ROOT" ]; then
    REPO_ROOT="$( cd "$HYDRA_JAVA_HEAD/../.." && pwd )"
fi
export HYDRA_ROOT_DIR="$REPO_ROOT"

if [ -z "${HYDRA_HOST_VERSION:-}" ]; then
    HYDRA_HOST_VERSION="$(python3 -c "import json; print(json.load(open('$REPO_ROOT/hydra.json'))['hostVersion'])")"
fi
export HYDRA_HOST_VERSION

echo "=== #703 cold-seed dist/haskell (Java host) ==="
echo "  repo root:    $REPO_ROOT"
echo "  host version: $HYDRA_HOST_VERSION"
echo ""

PACKAGES="$(python3 "$REPO_ROOT/bin/lib/hydra-packages.py" list)"

for pkg in $PACKAGES; do
    # Load flags mirror the retired seeder's baseline-vs-coder-package split.
    case "$pkg" in
        hydra-kernel|hydra-haskell) LOAD_FLAGS="" ;;
        *)                          LOAD_FLAGS="--include-coders" ;;
    esac
    case "$pkg" in
        hydra-jvm|hydra-wasm|hydra-ext|hydra-build|hydra-bench) DSL_FLAG="" ;;
        *)                                                       DSL_FLAG="--include-dsls" ;;
    esac
    echo "  seeding $pkg (main) ..."
    "$HYDRA_JAVA_HEAD/bin/transform-json-to-target.sh" \
        haskell "$pkg" main \
        --output "$REPO_ROOT/dist/haskell" \
        --dist-json-root "$REPO_ROOT/dist/json" \
        $LOAD_FLAGS $DSL_FLAG \
        --prune-stale

    TEST_JSON_DIR="$REPO_ROOT/dist/json/$pkg/src/test/json"
    if [ -d "$TEST_JSON_DIR" ]; then
        echo "  seeding $pkg (test) ..."
        "$HYDRA_JAVA_HEAD/bin/transform-json-to-target.sh" \
            haskell "$pkg" test \
            --output "$REPO_ROOT/dist/haskell" \
            --dist-json-root "$REPO_ROOT/dist/json" \
            $LOAD_FLAGS \
            --include-tests \
            --prune-stale
    fi
done

# Emit each package's package.yaml manifest so every dist/haskell/<pkg>/ is a
# self-contained buildable package, and copy LICENSE/CHANGELOG/NOTICE (stack
# build's copy/register step fails without them; generate-haskell-package-
# build.py always declares them in extra-source-files). Mirrors the retired
# cold-seeder's own final step exactly.
echo ""
echo "Emitting per-package manifests (all 16)..."
for pkg in $PACKAGES; do
    python3 "$REPO_ROOT/bin/lib/generate-haskell-package-build.py" "$pkg" \
        --repo-root "$REPO_ROOT"
    cp "$REPO_ROOT/heads/haskell/LICENSE" "$REPO_ROOT/dist/haskell/$pkg/LICENSE"
    cp "$REPO_ROOT/CHANGELOG.md" "$REPO_ROOT/dist/haskell/$pkg/CHANGELOG.md"
    cp "$REPO_ROOT/NOTICE" "$REPO_ROOT/dist/haskell/$pkg/NOTICE"
done

echo ""
echo "=== cold-seed complete: dist/haskell seeded + manifests emitted ==="
