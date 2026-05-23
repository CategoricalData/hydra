#!/bin/bash
# Clean output directory and copy static resources for a TypeScript bootstrap target.
#
# Layout produced under <output-dir>:
#   package.json          (from resources/typescript/)
#   tsconfig.json         (from resources/typescript/)
#   vitest.config.ts      (from resources/typescript/)
#   src/main/typescript/  (hand-written runtime: core, lib/*, primitives, libraries)
#   src/test/typescript/  (hand-written test runner + testEnv + jsonBindings)
#   (generated kernel files are written into the above src/main/.../hydra/
#    tree by invoke-<host>-host.sh --target typescript)
#
# Usage: ./setup-typescript-target.sh <output-dir>

set -e

OUTPUT_DIR="$1"
if [ -z "$OUTPUT_DIR" ]; then
    echo "Usage: $0 <output-dir>"
    exit 1
fi

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../.." && pwd )"
HYDRA_TS_HEAD="$HYDRA_ROOT/heads/typescript"
TS_RESOURCES="$SCRIPT_DIR/../resources/typescript"

# Clean and create output directory
echo "Preparing output directory: $OUTPUT_DIR"
rm -rf "$OUTPUT_DIR"
mkdir -p "$OUTPUT_DIR"

# Copy static resources
echo "Copying static resources for TypeScript target..."

# Build files
echo "  Copying build files..."
cp "$TS_RESOURCES/package.json" "$OUTPUT_DIR/"
cp "$TS_RESOURCES/tsconfig.json" "$OUTPUT_DIR/"
cp "$TS_RESOURCES/vitest.config.ts" "$OUTPUT_DIR/"

# Hand-written runtime: runtime.ts (formerly core.ts; renamed in #126 to
# avoid clobbering the generated kernel `core.ts`), primitives.ts,
# lib/* (including libraries.ts). These are copied so the demo layout
# matches the in-tree heads/typescript layout. The host invoker writes
# generated kernel files alongside; `runtime.ts` does not collide.
echo "  Copying hand-written runtime..."
mkdir -p "$OUTPUT_DIR/src/main/typescript/hydra/lib"
cp "$HYDRA_TS_HEAD/src/main/typescript/hydra/runtime.ts" "$OUTPUT_DIR/src/main/typescript/hydra/"
cp "$HYDRA_TS_HEAD/src/main/typescript/hydra/primitives.ts" "$OUTPUT_DIR/src/main/typescript/hydra/"
cp -r "$HYDRA_TS_HEAD/src/main/typescript/hydra/lib/." "$OUTPUT_DIR/src/main/typescript/hydra/lib/"

# Mark the bootstrap target tree as ESM so `import.meta` works under
# NodeNext (same as dist/typescript/hydra-kernel/package.json written by
# copy-kernel-runtime.sh). Without it, tsc walks up to the user's home
# package.json (CommonJS) and rejects import.meta in jsonBindings.ts.
if ! grep -q '"type"[[:space:]]*:[[:space:]]*"module"' "$OUTPUT_DIR/package.json"; then
    python3 -c "import json,sys; p='$OUTPUT_DIR/package.json'; d=json.load(open(p)); d['type']='module'; json.dump(d, open(p,'w'), indent=2)"
fi

# Hand-written test runtime: test runner + testEnv + jsonBindings
echo "  Copying hand-written test runtime..."
mkdir -p "$OUTPUT_DIR/src/test/typescript/hydra/test"
cp "$HYDRA_TS_HEAD/src/test/typescript/test-suite-runner.test.ts" "$OUTPUT_DIR/src/test/typescript/"
for f in "$HYDRA_TS_HEAD"/src/test/typescript/hydra/test/*.ts; do
    [ -f "$f" ] || continue
    cp "$f" "$OUTPUT_DIR/src/test/typescript/hydra/test/"
done

# Rewrite the test runner's import of testSuite from the heads/typescript
# dev path (`../../../../../dist/typescript/hydra-kernel/.../testSuite.ts`)
# to the demo-local path (`./hydra/test/testSuite.ts`). In the demo
# layout, the generated testSuite is a sibling of the runner under
# src/test/typescript/.
RUNNER="$OUTPUT_DIR/src/test/typescript/test-suite-runner.test.ts"
if [ -f "$RUNNER" ]; then
    # Use a stable Perl one-liner that works on both BSD and GNU.
    perl -i -pe 's{from "\.\./\.\./\.\./\.\./\.\./dist/typescript/hydra-kernel/src/test/typescript/hydra/test/testSuite\.[jt]s"}{from "./hydra/test/testSuite.js"}g' "$RUNNER"
fi

# npm install for vitest + typescript. Use --no-audit/--no-fund to keep
# the bootstrap log clean. Allowed to fail if offline; the test step will
# report a clearer error.
echo "  Installing npm dependencies..."
(cd "$OUTPUT_DIR" && npm install --no-audit --no-fund --loglevel=error 2>&1 | tail -10) || \
    echo "  warning: npm install failed; tests will not be runnable"

# Summary
STATIC_COUNT=$(find "$OUTPUT_DIR/src" -name "*.ts" 2>/dev/null | wc -l | tr -d ' ')
echo "  Static resources: $STATIC_COUNT TypeScript files"
echo ""
