#!/usr/bin/env bash
# Validate the generated TypeScript dist tree by running `tsc --strict`
# against it. The dist tree must already exist; run bin/sync-typescript.sh
# first to populate it.
#
# Usage:
#   test-distribution.sh [hydra-kernel]  (default: hydra-kernel)
#
# Exit 0 if tsc reports zero errors. Exit 1 otherwise (prints the first
# 50 lines of tsc output for diagnostics).

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_TS_HEAD="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_TS_HEAD/../.." && pwd )"

PACKAGE="${1:-hydra-kernel}"
DIST_ROOT="$HYDRA_ROOT_DIR/dist/typescript"
PKG_ROOT="$DIST_ROOT/$PACKAGE/src"
TS_MAIN="$PKG_ROOT/main/typescript"
TS_TEST="$PKG_ROOT/test/typescript"

if [ ! -d "$TS_MAIN/hydra" ]; then
    echo "error: no TypeScript dist at $TS_MAIN/hydra" >&2
    echo "       run bin/sync-typescript.sh first" >&2
    exit 1
fi

# Build a unified tsconfig that covers main + test trees. The test files
# emit cross-tree imports of the form `../../../../main/typescript/hydra/...`
# which resolve correctly under NodeNext module resolution.
TS_CONFIG="$PKG_ROOT/tsconfig.json"
cat > "$TS_CONFIG" <<EOF
{
  "compilerOptions": {
    "target": "ES2022",
    "module": "NodeNext",
    "moduleResolution": "nodenext",
    "strict": true,
    "noEmit": true,
    "skipLibCheck": true
  },
  "include": ["main/typescript/hydra/**/*.ts", "test/typescript/hydra/**/*.ts"]
}
EOF

echo "=========================================="
echo "Testing TypeScript distribution: $PACKAGE"
echo "  Main: $TS_MAIN"
if [ -d "$TS_TEST/hydra" ]; then
    echo "  Test: $TS_TEST"
fi
echo "=========================================="

cd "$PKG_ROOT"
set +e
TSC_OUTPUT=$(npx --yes -p typescript@5.6 tsc --noEmit 2>&1)
TSC_EXIT=$?
set -e
ERROR_COUNT=$(echo "$TSC_OUTPUT" | grep -cE "^(main|test)/typescript/" || true)

# Detect internal tsc crashes ("Debug Failure", uncaught exceptions). These
# produce a non-zero exit with no file-anchored diagnostics, and used to slip
# past the grep-based count above. Any such crash means the generated code
# is pathological enough to defeat the compiler — must be a hard fail.
if echo "$TSC_OUTPUT" | grep -qE "^Error: Debug Failure|^Error: |throw e;"; then
    echo "  tsc --strict: INTERNAL COMPILER CRASH. FAIL."
    echo ""
    echo "First 50 lines of tsc output:"
    echo "$TSC_OUTPUT" | head -50
    exit 1
fi

if [ "$TSC_EXIT" -ne 0 ] && [ "$ERROR_COUNT" -eq 0 ]; then
    echo "  tsc --strict: non-zero exit ($TSC_EXIT) with no parsed errors. FAIL."
    echo ""
    echo "First 50 lines of tsc output:"
    echo "$TSC_OUTPUT" | head -50
    exit 1
fi

if [ "$ERROR_COUNT" -eq 0 ]; then
    echo "  tsc --strict: 0 errors. PASS."
    exit 0
else
    echo "  tsc --strict: $ERROR_COUNT errors. FAIL."
    echo ""
    echo "First 50 lines of tsc output:"
    echo "$TSC_OUTPUT" | head -50
    exit 1
fi
