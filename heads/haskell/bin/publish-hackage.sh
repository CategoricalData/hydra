#!/usr/bin/env bash
# Assemble (and optionally upload) the Hydra Haskell per-package Hackage
# distributions in dependency order: leaves first, umbrella last.
#
# Replaces the monolithic single-`hydra` upload (#418). The 0.16.0 publish set
# is the trio: hydra-kernel -> hydra-haskell -> hydra. The publish ORDER matters
# because the dependents pin their siblings with `== <version>`; uploading a
# dependent before its dependency would leave it transiently unsatisfiable on
# Hackage.
#
# Two safety properties enforced here:
#   1. DEPENDENCY CLOSURE: every Hydra package any published package depends on
#      must itself be in the publish set. A package depending on an unpublished
#      Hydra package is a hard error (would strand it on Hackage). This turns the
#      maturity/publish gate into a checked invariant rather than a convention.
#   2. LEAVES-FIRST ORDER: packages are assembled/uploaded in topological order
#      so a dependency is always present before its dependents.
#
# Usage:
#   publish-hackage.sh [--out <dir>] [--upload] [--publish]
#
#   (default)   assemble all sdists into <out> (build/hackage/) in order; no upload.
#   --upload    after assembling, run `cabal upload` (candidate) per package in order.
#   --publish   run `cabal upload --publish` (FINAL, irreversible) per package in order.
#               Implies upload. Prompts for confirmation unless HYDRA_YES=1.
#
# Output: <out>/<pkg>-<version>.tar.gz for each package in the publish set.

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_HASKELL_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT="$( cd "$HYDRA_HASKELL_DIR/../.." && pwd )"

OUT_DIR="$HYDRA_ROOT/build/hackage"
DO_UPLOAD=false
DO_PUBLISH=false

while [ $# -gt 0 ]; do
    case "$1" in
        --out) OUT_DIR="$2"; shift 2 ;;
        --upload) DO_UPLOAD=true; shift ;;
        --publish) DO_UPLOAD=true; DO_PUBLISH=true; shift ;;
        --help|-h)
            sed -n '2,/^$/p' "$0" | sed 's/^# \{0,1\}//'
            exit 0
            ;;
        *) echo "Unknown argument: $1" >&2; exit 1 ;;
    esac
done

VERSION="$(cat "$HYDRA_ROOT/VERSION")"

# The 0.16.0 publish set, in LEAVES-FIRST topological order. To expand the set
# (0.16.1+: add hydra-java, hydra-python, hydra-scala, hydra-lisp, hydra-pg,
# hydra-rdf), append here in topo order — and ensure each one's deps precede it.
PUBLISH_SET=(hydra-kernel hydra-haskell hydra)

# --- Guard 1: dependency closure ---------------------------------------------
# For each package in the set, read its Hydra dependencies and assert each is
# also in the set. Kernel/haskell deps come from packages/<pkg>/package.json;
# the umbrella's are known (hydra-kernel, hydra-haskell).
echo "=== Checking dependency closure of publish set ==="
in_set() {
    local needle="$1"; local x
    for x in "${PUBLISH_SET[@]}"; do [ "$x" = "$needle" ] && return 0; done
    return 1
}

pkg_hydra_deps() {
    local pkg="$1"
    if [ "$pkg" = "hydra" ]; then
        echo "hydra-kernel hydra-haskell"
        return 0
    fi
    local json="$HYDRA_ROOT/packages/$pkg/package.json"
    if [ ! -f "$json" ]; then
        echo "ERROR: no package.json for $pkg" >&2
        return 1
    fi
    # Extract the "dependencies" array entries (Hydra package names).
    python3 -c "import json,sys; print(' '.join(json.load(open(sys.argv[1])).get('dependencies') or []))" "$json"
}

CLOSURE_OK=true
for pkg in "${PUBLISH_SET[@]}"; do
    deps="$(pkg_hydra_deps "$pkg")"
    for d in $deps; do
        if in_set "$d"; then
            echo "  OK: $pkg -> $d (in publish set)"
        else
            echo "  ERROR: $pkg depends on '$d' which is NOT in the publish set" >&2
            echo "         Publishing $pkg would strand it on Hackage. Add '$d' to" >&2
            echo "         PUBLISH_SET (and ensure it is mature enough to publish), or" >&2
            echo "         remove the dependency." >&2
            CLOSURE_OK=false
        fi
    done
done
if [ "$CLOSURE_OK" != true ]; then
    echo "FAIL: publish set is not dependency-closed." >&2
    exit 1
fi
echo "  Publish set is dependency-closed."
echo ""

# --- Assemble each package (leaves first) ------------------------------------
mkdir -p "$OUT_DIR"
echo "=== Assembling sdists (leaves first) into $OUT_DIR ==="
for pkg in "${PUBLISH_SET[@]}"; do
    "$SCRIPT_DIR/assemble-haskell-distribution.sh" "$pkg" --out "$OUT_DIR"
done
echo ""

echo "=== sdists produced (upload order) ==="
for pkg in "${PUBLISH_SET[@]}"; do
    tarball="$OUT_DIR/$pkg-$VERSION.tar.gz"
    if [ -f "$tarball" ]; then
        echo "  $tarball"
    else
        echo "  ERROR: missing expected tarball: $tarball" >&2
        exit 1
    fi
done
echo ""

if [ "$DO_UPLOAD" != true ]; then
    echo "Assemble-only (no upload). To upload candidates: --upload; to publish: --publish."
    exit 0
fi

# --- Upload (leaves first) ---------------------------------------------------
if [ "$DO_PUBLISH" = true ] && [ "${HYDRA_YES:-}" != "1" ]; then
    echo "About to PUBLISH (irreversible) ${#PUBLISH_SET[@]} packages to Hackage at version $VERSION:"
    printf '    %s\n' "${PUBLISH_SET[@]}"
    printf 'Type "publish" to proceed: '
    read -r confirm
    [ "$confirm" = "publish" ] || { echo "Aborted."; exit 1; }
fi

UPLOAD_FLAGS=()
[ "$DO_PUBLISH" = true ] && UPLOAD_FLAGS+=(--publish)

for pkg in "${PUBLISH_SET[@]}"; do
    tarball="$OUT_DIR/$pkg-$VERSION.tar.gz"
    echo "=== cabal upload ${UPLOAD_FLAGS[*]:-} $tarball ==="
    cabal upload "${UPLOAD_FLAGS[@]}" "$tarball"
    echo ""
done

if [ "$DO_PUBLISH" = true ]; then
    echo "=== Published ${#PUBLISH_SET[@]} packages at $VERSION. ==="
    echo "Next: upload Haddock docs per package if desired (cabal upload --documentation)."
else
    echo "=== Uploaded ${#PUBLISH_SET[@]} candidates at $VERSION. ==="
    echo "Review on Hackage, then re-run with --publish to finalize."
fi
