#!/usr/bin/env bash
# Verify the Hydra TypeScript distribution is self-contained across the
# source -> npm packaging boundary.
#
# TypeScript analog of heads/python/bin/verify-distribution.sh and
# heads/java/bin/verify-distribution.sh: builds the npm publish-set packages
# from the dist/ tree ALONE and proves they are self-contained in isolation
# from the worktree. Uniform name across heads so prepare-release.sh / CI can
# run the same step per host. (#492)
#
# What it does:
#   1. Compiles each publish-set package's TypeScript sources to JS + .d.ts.
#   2. Packs each into an npm tarball via `npm pack`.
#   3. Installs ONLY those tarballs into a fresh, isolated node_modules (no
#      npm registry access) and verifies that the kernel's core module loads
#      cleanly — catches a missing-from-pack file before upload.
#
# Usage:
#   verify-distribution.sh                     # compile, pack, then verify
#   verify-distribution.sh --tarballs <dir>    # skip compile+pack; verify tarballs in <dir>
#
# Exit 0 = all packages pack and the kernel core imports cleanly.
# non-zero = a package fails to compile, pack, or import.

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_TS_HEAD="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT="$( cd "$HYDRA_TS_HEAD/../.." && pwd )"

# Mirror of publish-npm.sh PUBLISH_SET (leaves first).
PUBLISH_SET=(hydra-kernel hydra-rdf hydra-pg hydra-typescript)

TARBALLS_DIR=""
while [ $# -gt 0 ]; do
    case "$1" in
        --tarballs) TARBALLS_DIR="$2"; shift 2 ;;
        --help|-h) sed -n '2,/^$/p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
        *) echo "Unknown argument: $1" >&2; exit 1 ;;
    esac
done

# Ensure tsc is available.
if [ ! -d "$HYDRA_TS_HEAD/node_modules" ]; then
    echo "Installing heads/typescript devDependencies..."
    (cd "$HYDRA_TS_HEAD" && npm install --no-audit --no-fund --loglevel=error)
fi
TSC_BIN="$HYDRA_TS_HEAD/node_modules/.bin/tsc"
[ -x "$TSC_BIN" ] || { echo "ERROR: tsc not found at $TSC_BIN" >&2; exit 1; }

TMP_TARBALLS=""
TMP_INSTALL=""
cleanup() {
    [ -n "$TMP_TARBALLS" ] && rm -rf "$TMP_TARBALLS"
    [ -n "$TMP_INSTALL" ] && rm -rf "$TMP_INSTALL"
}
trap cleanup EXIT

# --- Compile + pack unless --tarballs given ----------------------------------
if [ -z "$TARBALLS_DIR" ]; then
    TMP_TARBALLS="$(mktemp -d "${TMPDIR:-/tmp}/hydra-verify-ts.XXXXXX")"
    TARBALLS_DIR="$TMP_TARBALLS"
    VERSION="$("$HYDRA_ROOT/bin/lib/hydra-packages.py" current-version)"

    echo "=== Compiling and packing npm publish-set from dist/typescript/ ==="
    for pkg in "${PUBLISH_SET[@]}"; do
        pkgdir="$HYDRA_ROOT/dist/typescript/$pkg"
        [ -d "$pkgdir" ] || { echo "ERROR: missing dist package: $pkgdir (run sync first)" >&2; exit 1; }
        [ -f "$pkgdir/tsconfig.build.json" ] || {
            echo "ERROR: missing tsconfig.build.json in $pkgdir (run sync first)" >&2; exit 1
        }
        echo "--- $pkg ---"
        (
            cd "$pkgdir"
            "$TSC_BIN" --project tsconfig.build.json \
                --typeRoots "$HYDRA_TS_HEAD/node_modules/@types"
            npm pack --pack-destination "$TARBALLS_DIR" 2>/dev/null
        )
    done
    echo ""
fi

TARBALLS_DIR="$( cd "$TARBALLS_DIR" && pwd )"
shopt -s nullglob
tarballs=( "$TARBALLS_DIR"/*.tgz )
shopt -u nullglob
[ "${#tarballs[@]}" -gt 0 ] || { echo "ERROR: no *.tgz files in $TARBALLS_DIR" >&2; exit 1; }

# --- Install into an isolated directory and verify imports -------------------
echo "=== Packaging-boundary npm smoke test ==="
echo "  tarballs dir: $TARBALLS_DIR"
echo "  tarballs:     ${#tarballs[@]}"

TMP_INSTALL="$(mktemp -d "${TMPDIR:-/tmp}/hydra-npm-smoke.XXXXXX")"

(
    cd "$TMP_INSTALL"
    # Minimal package.json so npm install works.
    printf '{"name":"smoke","version":"0.0.0","type":"module","private":true}\n' > package.json
    npm install --no-audit --no-fund --loglevel=error "${tarballs[@]}" 2>/dev/null
    # Verify the kernel core module loads — catches a file missing from the pack.
    node --input-type=module <<'EOF'
import {} from 'hydra-kernel/dist/hydra/core.js';
console.log('  OK   hydra-kernel/dist/hydra/core.js');
EOF
)

echo ""
echo "SMOKE TEST PASSED: hydra-kernel core imports cleanly from the packed tarballs."
