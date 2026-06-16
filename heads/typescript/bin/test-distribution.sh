#!/usr/bin/env bash
# Validate the generated TypeScript dist tree by running `tsc --strict`
# against it, then exercising the vitest common-test-suite runner. The
# dist tree must already exist; run bin/sync-typescript.sh first to
# populate it.
#
# Usage:
#   test-distribution.sh [hydra-kernel]   (default: hydra-kernel)
#   test-distribution.sh --no-vitest …    (skip vitest; keep tsc only)
#
# Exit 0 if both steps pass. Exit 1 on any tsc error or vitest failure.

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_TS_HEAD="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_TS_HEAD/../.." && pwd )"

RUN_VITEST=1
PACKAGE=""
while [ $# -gt 0 ]; do
    case "$1" in
        --no-vitest) RUN_VITEST=0; shift ;;
        --*) echo "Unknown flag: $1" >&2; exit 2 ;;
        *)
            if [ -n "$PACKAGE" ]; then
                echo "Unexpected extra argument: $1" >&2; exit 2
            fi
            PACKAGE="$1"; shift
            ;;
    esac
done
PACKAGE="${PACKAGE:-hydra-kernel}"
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
#
# - `types: ["node"]` — hand-written runtime modules import `node:fs`,
#   `node:path`, `node:url`, and reference `process` / `Buffer`. Without
#   `@types/node` in scope, tsc fails with TS2307/TS2580 even though the
#   code runs fine under node.
# - `bootstrap.ts` is excluded: it imports from `hydra-lisp` (sibling
#   package, not present under `hydra-kernel/`) and is intended for
#   bootstrap-time use only, not part of the standalone kernel check.
TS_CONFIG="$PKG_ROOT/tsconfig.json"
TS_TYPE_ROOTS="$HYDRA_TS_HEAD/node_modules/@types"
cat > "$TS_CONFIG" <<EOF
{
  "compilerOptions": {
    "target": "ES2022",
    "module": "NodeNext",
    "moduleResolution": "nodenext",
    "typeRoots": ["$TS_TYPE_ROOTS"],
    "types": ["node"],
    "strict": true,
    "noEmit": true,
    "skipLibCheck": true
  },
  "include": ["main/typescript/hydra/**/*.ts", "test/typescript/hydra/**/*.ts"],
  "exclude": ["main/typescript/hydra/bootstrap.ts"]
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
# Use heads/typescript's installed typescript + @types/node rather than
# `npx --yes -p typescript@5.6 tsc`. The npx form downloads typescript on
# the fly but has no `@types/node` in scope, so the runtime's `node:*`
# imports and `process` / `Buffer` references fail with TS2307/TS2580 even
# though they're valid at execution time. The head's node_modules has both
# (devDependencies in heads/typescript/package.json), so we cd-and-run.
if [ ! -d "$HYDRA_TS_HEAD/node_modules" ]; then
    echo "  Installing heads/typescript devDependencies (typescript, @types/node, vitest)…"
    (cd "$HYDRA_TS_HEAD" && npm install --no-audit --no-fund --loglevel=error 2>&1 | tail -5)
fi
TSC_BIN="$HYDRA_TS_HEAD/node_modules/.bin/tsc"
if [ ! -x "$TSC_BIN" ]; then
    echo "  fallback: head's node_modules missing tsc; using npx (may flag node typings as missing)"
    TSC_BIN="npx --yes -p typescript@5.6 tsc"
fi
set +e
TSC_OUTPUT=$($TSC_BIN --noEmit 2>&1)
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

if [ "$ERROR_COUNT" -ne 0 ]; then
    echo "  tsc --strict: $ERROR_COUNT errors. FAIL."
    echo ""
    echo "First 50 lines of tsc output:"
    echo "$TSC_OUTPUT" | head -50
    exit 1
fi
echo "  tsc --strict: 0 errors. PASS."

if [ "$RUN_VITEST" = "0" ]; then
    exit 0
fi

# Vitest run: exercise the common test suite against the dist tree.
# `heads/typescript/package.json` already has `vitest` as a devDependency
# and a `test` script that runs against the dist tree via the test-suite
# runner's relative import. So we just `npm test` from there.
echo ""
echo "=========================================="
echo "Vitest: common test suite"
echo "=========================================="
cd "$HYDRA_TS_HEAD"
if [ ! -d node_modules ]; then
    echo "  Installing npm devDependencies (vitest, typescript)…"
    npm install --no-audit --no-fund --loglevel=error 2>&1 | tail -5
fi
if npm test --silent; then
    echo "  vitest: PASS."
    exit 0
else
    echo "  vitest: FAIL."
    exit 1
fi
