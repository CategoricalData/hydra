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

VERSION="$("$HYDRA_ROOT/bin/lib/hydra-packages.py" current-version)"

# The full publish set, in LEAVES-FIRST topological order (#376). DERIVED from the
# hydra.json registry via `hydra-packages.py topo` (deps first) so it stays complete
# automatically as packages are added — NO hardcoded list to fall out of sync (the
# original 0.16 trio-only hardcoding is exactly what left the coder packages off
# Hackage; #376). The hand-written `hydra` umbrella is not in the registry, so it is
# appended last (it depends on kernel+haskell, both earlier in topo order). Guard 1
# below still asserts dependency-closure, so any ordering error is caught before upload.
# `topo` prints the packages on ONE space-separated line, so word-split with
# `read -ra` (not `mapfile`, which is line-based and would yield a single element).
read -ra _REGISTRY_TOPO < <("$HYDRA_ROOT/bin/lib/hydra-packages.py" topo \
    $("$HYDRA_ROOT/bin/lib/hydra-packages.py" list))
if [ "${#_REGISTRY_TOPO[@]}" -eq 0 ]; then
    echo "ERROR: could not derive publish set from hydra.json registry" >&2
    exit 1
fi
PUBLISH_SET=("${_REGISTRY_TOPO[@]}" hydra)
HACKAGE_BASE="https://hackage.haskell.org/package"

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

# A dependency is closure-satisfied if it is either in this upload batch OR
# already published on Hackage at VERSION (#376: incremental re-publish — kernel/
# haskell/umbrella are already live at 0.17.0, so the coder batch does not
# re-upload them, yet they still satisfy the coders' dependency).
already_published() {
    local pkg="$1"
    local code
    code="$(curl -s -o /dev/null -w '%{http_code}' --max-time 15 \
        "$HACKAGE_BASE/$pkg-$VERSION" 2>/dev/null || echo 000)"
    [ "$code" = "200" ]
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
        elif already_published "$d"; then
            echo "  OK: $pkg -> $d (already on Hackage at $VERSION)"
        else
            echo "  ERROR: $pkg depends on '$d' which is NEITHER in the publish set" >&2
            echo "         NOR already published on Hackage at $VERSION." >&2
            echo "         Publishing $pkg would strand it. Add '$d' to PUBLISH_SET" >&2
            echo "         (and ensure it is mature enough to publish), or remove the dep." >&2
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

# --- Narrow to the packages actually needing upload --------------------------
# Skip any package already live on Hackage at VERSION (#376 incremental publish):
# re-uploading an existing published version errors, and it is not the goal here.
TO_PUBLISH=()
echo "=== Selecting packages to publish (skipping any already live at $VERSION) ==="
for pkg in "${PUBLISH_SET[@]}"; do
    if already_published "$pkg"; then
        echo "  SKIP $pkg (already on Hackage at $VERSION)"
    else
        echo "  QUEUE $pkg"
        TO_PUBLISH+=("$pkg")
    fi
done
echo ""
if [ "${#TO_PUBLISH[@]}" -eq 0 ]; then
    echo "Nothing to publish — every package in the set is already live at $VERSION."
    exit 0
fi

# --- Assemble each package (leaves first) ------------------------------------
mkdir -p "$OUT_DIR"
echo "=== Assembling sdists (leaves first) into $OUT_DIR ==="
for pkg in "${TO_PUBLISH[@]}"; do
    "$SCRIPT_DIR/assemble-haskell-distribution.sh" "$pkg" --out "$OUT_DIR"
done
echo ""

echo "=== sdists produced (upload order) ==="
for pkg in "${TO_PUBLISH[@]}"; do
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
    echo "About to PUBLISH (irreversible) ${#TO_PUBLISH[@]} packages to Hackage at version $VERSION:"
    printf '    %s\n' "${TO_PUBLISH[@]}"
    printf 'Type "publish" to proceed: '
    read -r confirm
    [ "$confirm" = "publish" ] || { echo "Aborted."; exit 1; }
fi

UPLOAD_FLAGS=()
[ "$DO_PUBLISH" = true ] && UPLOAD_FLAGS+=(--publish)

for pkg in "${TO_PUBLISH[@]}"; do
    tarball="$OUT_DIR/$pkg-$VERSION.tar.gz"
    echo "=== cabal upload ${UPLOAD_FLAGS[*]:-} $tarball ==="
    # Expand to nothing (not an empty-string arg) when UPLOAD_FLAGS is empty, so
    # candidate uploads work under `set -u` (an empty array's [@] is otherwise an
    # unbound-variable error).
    cabal upload ${UPLOAD_FLAGS[@]+"${UPLOAD_FLAGS[@]}"} "$tarball"
    echo ""
done

if [ "$DO_PUBLISH" = true ]; then
    echo "=== Published ${#TO_PUBLISH[@]} packages at $VERSION. ==="
    echo "Next: upload Haddock docs per package if desired (cabal upload --documentation)."
    echo "Published package pages:"
    for pkg in "${TO_PUBLISH[@]}"; do
        echo "  $HACKAGE_BASE/$pkg-$VERSION"
    done
else
    echo "=== Uploaded ${#TO_PUBLISH[@]} candidates at $VERSION. ==="
    echo "Review candidate pages, then re-run with --publish to finalize:"
    for pkg in "${TO_PUBLISH[@]}"; do
        echo "  $HACKAGE_BASE/$pkg-$VERSION/candidate"
    done
fi
