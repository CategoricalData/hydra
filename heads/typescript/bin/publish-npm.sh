#!/usr/bin/env bash
# Build (and optionally publish) Hydra TypeScript per-package npm tarballs.
#
# TypeScript analog of heads/haskell/bin/publish-hackage.sh,
# heads/java/bin/publish-maven.sh, and heads/python/bin/publish-pypi.sh.
#
# The npm publish set for 0.17 is:
#   hydra-kernel -> hydra-rdf -> hydra-pg
#                -> hydra-typescript
# (dependency-ordered; leaves first)
#
# Each dist/typescript/<pkg>/ is a self-contained npm package directory
# with a generated package.json and tsconfig.build.json (written by
# bin/lib/generate-typescript-package-build.py via assemble-distribution.sh).
# This script:
#   1. Checks the publish set is dependency-closed.
#   2. Compiles each package's TypeScript sources to JS + .d.ts via tsc.
#   3. Runs `npm pack` to produce a tarball in <out>/.
#   4. Smoke-tests the packed tarball in an isolated node env.
#   5. (With --upload) runs `npm publish` for each package in leaves-first order.
#
# npm uploads are permanent: once <pkg>@<version> is published it cannot be
# re-uploaded. Use --dry-run (default) to verify packaging before committing.
#
# Usage:
#   publish-npm.sh [--out <dir>] [--upload] [--package <pkg>] [--otp <code>]
#
#   (default)      compile + pack into <out> (npm-tarballs/); no upload.
#   --upload       after packing, `npm publish` each tarball in order.
#                  Requires NPM_TOKEN env var or `npm login` session.
#   --package <p>  restrict to one package (must still be in the publish set).
#   --otp <code>   one-time 2FA code, passed to `npm publish --otp`. For an
#                  account with 2FA-required publishing; a granular token with
#                  "bypass 2FA" is more robust for the multi-package batch (a
#                  single OTP code may expire across sequential publishes).
#   --out <dir>    override output directory (default: <repo-root>/npm-tarballs).
#
# Access prerequisites:
#   npm account that is Owner of hydra-kernel, hydra-rdf, hydra-pg,
#   hydra-typescript on npmjs.com, and either:
#     - NPM_TOKEN=<token> in the environment, or
#     - an active `npm login` session in the shell.

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_TS_HEAD="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT="$( cd "$HYDRA_TS_HEAD/../.." && pwd )"

OUT_DIR="$HYDRA_ROOT/npm-tarballs"
DO_UPLOAD=false
ONLY_PKG=""
OTP=""

while [ $# -gt 0 ]; do
    case "$1" in
        --out) OUT_DIR="$2"; shift 2 ;;
        --upload) DO_UPLOAD=true; shift ;;
        --package) ONLY_PKG="$2"; shift 2 ;;
        # One-time password for accounts with 2FA-required publishing (an
        # alternative to a granular token with "bypass 2FA"). Passed through to
        # `npm publish --otp`. The code is short-lived, so publish promptly.
        --otp) OTP="$2"; shift 2 ;;
        --help|-h) sed -n '2,/^$/p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
        *) echo "Unknown argument: $1" >&2; exit 1 ;;
    esac
done

VERSION="$("$HYDRA_ROOT/bin/lib/hydra-packages.py" current-version)"

# Leaves-first publish order (dependency-ordered).
PUBLISH_SET=(
    hydra-kernel
    hydra-rdf
    hydra-pg
    hydra-typescript
)

# --- Guard: dependency closure -----------------------------------------------
echo "=== Checking dependency closure of npm publish set ==="
in_set() {
    local needle="$1" x
    for x in "${PUBLISH_SET[@]}"; do [ "$x" = "$needle" ] && return 0; done
    return 1
}
pkg_deps() {
    local json="$HYDRA_ROOT/packages/$1/package.json"
    [ -f "$json" ] || { echo "ERROR: no package.json for $1" >&2; return 1; }
    python3 -c "import json,sys; print(' '.join(json.load(open(sys.argv[1])).get('dependencies') or []))" "$json"
}
CLOSURE_OK=true
for pkg in "${PUBLISH_SET[@]}"; do
    for d in $(pkg_deps "$pkg"); do
        if in_set "$d"; then
            echo "  OK: $pkg -> $d"
        else
            echo "  ERROR: $pkg depends on '$d' not in publish set" >&2
            CLOSURE_OK=false
        fi
    done
done
[ "$CLOSURE_OK" = true ] || { echo "FAIL: publish set not dependency-closed." >&2; exit 1; }
echo ""

# --- Ensure tsc is available -------------------------------------------------
if [ ! -d "$HYDRA_TS_HEAD/node_modules" ]; then
    echo "Installing heads/typescript devDependencies (typescript, @types/node)..."
    (cd "$HYDRA_TS_HEAD" && npm install --no-audit --no-fund --loglevel=error)
fi
TSC_BIN="$HYDRA_TS_HEAD/node_modules/.bin/tsc"
if [ ! -x "$TSC_BIN" ]; then
    echo "ERROR: tsc not found at $TSC_BIN" >&2
    exit 1
fi

# --- Clean output dir --------------------------------------------------------
if [ -d "$OUT_DIR" ]; then
    echo "Clearing stale tarballs from $OUT_DIR"
    rm -f "$OUT_DIR"/*.tgz
fi
mkdir -p "$OUT_DIR"

# --- Compile + pack each package ---------------------------------------------
echo "=== Compiling and packing into $OUT_DIR ==="
for pkg in "${PUBLISH_SET[@]}"; do
    [ -n "$ONLY_PKG" ] && [ "$ONLY_PKG" != "$pkg" ] && continue

    pkgdir="$HYDRA_ROOT/dist/typescript/$pkg"
    pkg_json="$pkgdir/package.json"
    tsconfig_build="$pkgdir/tsconfig.build.json"

    [ -f "$pkg_json" ] || {
        echo "ERROR: missing $pkg_json (run bin/sync-typescript.sh first)" >&2; exit 1
    }
    # Verify version matches (catches stale dist).
    pkg_ver=$(python3 -c "import json,sys; print(json.load(open(sys.argv[1]))['version'])" "$pkg_json" 2>/dev/null || echo "")
    if [ "$pkg_ver" != "$VERSION" ]; then
        echo "ERROR: $pkg package.json version '$pkg_ver' != $VERSION (stale dist? run sync first)" >&2
        exit 1
    fi

    [ -f "$tsconfig_build" ] || {
        echo "ERROR: missing $tsconfig_build (run bin/sync-typescript.sh first)" >&2; exit 1
    }

    echo "--- $pkg @ $VERSION ---"

    # Compile TypeScript → JS + .d.ts into <pkgdir>/dist/
    echo "  Compiling TypeScript..."
    (
        cd "$pkgdir"
        "$TSC_BIN" --project tsconfig.build.json \
            --typeRoots "$HYDRA_TS_HEAD/node_modules/@types"
    )

    # Pack into a tarball
    echo "  Packing..."
    (cd "$pkgdir" && npm pack --pack-destination "$OUT_DIR" 2>/dev/null)
done
echo ""

echo "=== Tarballs in $OUT_DIR ==="
ls -1 "$OUT_DIR"/*.tgz 2>/dev/null | sed 's/^/  /'
echo ""

# --- Smoke-gate: verify packed tarballs resolve imports ----------------------
echo "=== Smoke-testing packed tarballs ==="
SMOKE_DIR="$(mktemp -d)"
trap 'rm -rf "$SMOKE_DIR"' EXIT

(
    cd "$SMOKE_DIR"
    # Install from tarballs with no-index so only our artifacts + their
    # declared deps (from tarballs) resolve — catches missing-from-pack modules.
    # We install all tarballs at once so peer deps link correctly.
    npm install --no-audit --no-fund --loglevel=error \
        "$OUT_DIR"/*.tgz 2>/dev/null

    # Verify the kernel's top-level core module imports cleanly.
    # Use the bare "." export (main entry), not a subpath — the "./*" pattern
    # maps "hydra-kernel/X" to "./dist/X.js", so "hydra-kernel/dist/hydra/core.js"
    # would double-nest to "./dist/dist/hydra/core.js.js".
    node --input-type=module <<'EOF'
import { } from 'hydra-kernel';
console.log('hydra-kernel: OK');
EOF
)
echo "  Smoke gate: PASS"
echo ""

if [ "$DO_UPLOAD" != true ]; then
    echo "Build-only (no upload). To publish to npm: --upload"
    exit 0
fi

# --- Upload (leaves-first, matching PUBLISH_SET order) -----------------------
echo "=== Publishing to npm ==="

# Authenticate via NPM_TOKEN if set, otherwise rely on existing npm login.
if [ -n "${NPM_TOKEN:-}" ]; then
    echo "  Using NPM_TOKEN for authentication"
    NPM_AUTH_ARGS=(--//registry.npmjs.org/:_authToken="$NPM_TOKEN")
else
    # Verify the user is logged in.
    if ! npm whoami >/dev/null 2>&1; then
        echo "ERROR: not logged in to npm. Run \`npm login\` or set NPM_TOKEN." >&2
        exit 1
    fi
    echo "  Authenticated as: $(npm whoami)"
    NPM_AUTH_ARGS=()
fi

OTP_ARGS=()
[ -n "$OTP" ] && OTP_ARGS=(--otp="$OTP")

for pkg in "${PUBLISH_SET[@]}"; do
    [ -n "$ONLY_PKG" ] && [ "$ONLY_PKG" != "$pkg" ] && continue
    tarball=$(ls "$OUT_DIR/${pkg}-${VERSION}.tgz" 2>/dev/null || true)
    [ -f "$tarball" ] || {
        echo "ERROR: tarball not found: $OUT_DIR/${pkg}-${VERSION}.tgz" >&2; exit 1
    }
    echo "--- publishing $pkg @ $VERSION ---"
    npm publish "${NPM_AUTH_ARGS[@]+"${NPM_AUTH_ARGS[@]}"}" \
        "${OTP_ARGS[@]+"${OTP_ARGS[@]}"}" "$tarball" --access public
done

echo "=== Published ${#PUBLISH_SET[@]} packages to npm at $VERSION. ==="
