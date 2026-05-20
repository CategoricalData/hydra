#!/bin/bash
# Run TypeScript tests on bootstrapped code via vitest.
#
# Usage: ./test-typescript-target.sh <output-dir>

set -e

OUTPUT_DIR="$1"
if [ -z "$OUTPUT_DIR" ]; then
    echo "Usage: $0 <output-dir>"
    exit 1
fi

echo "Running TypeScript tests..."

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../.." && pwd )"
HYDRA_TS_HEAD="$HYDRA_ROOT/heads/typescript"

# Restore hand-written runtime files that the haskell host overwrote with
# generated kernel versions. The generated core.ts/primitives.ts only
# declare kernel data types; the hand-written versions also export the
# Maybe<T>/Just/Nothing/Either/Left/Right helpers used by lib/*.ts. This
# mirrors heads/typescript/bin/copy-kernel-runtime.sh's post-sync
# behavior. The lib/*.ts files don't overlap with the generated output
# (they live under hydra/lib/, while generated puts top-level files like
# literals.ts, parsers.ts, etc.) so no restore needed there.
if [ -f "$HYDRA_TS_HEAD/src/main/typescript/hydra/core.ts" ]; then
    cp "$HYDRA_TS_HEAD/src/main/typescript/hydra/core.ts" \
       "$OUTPUT_DIR/src/main/typescript/hydra/core.ts"
fi
if [ -f "$HYDRA_TS_HEAD/src/main/typescript/hydra/primitives.ts" ]; then
    cp "$HYDRA_TS_HEAD/src/main/typescript/hydra/primitives.ts" \
       "$OUTPUT_DIR/src/main/typescript/hydra/primitives.ts"
fi
# Same for test-tree files: testEnv.ts and jsonBindings.ts are
# hand-written counterparts of the DSL stubs `hydra.test.testEnv` and
# `hydra.test.jsonBindings`. The TS host emits stubbed (empty) versions
# when --include-tests is set; restore the hand-written versions so the
# test graph has primitives + kernel JSON loaded.
for f in testEnv.ts jsonBindings.ts; do
    if [ -f "$HYDRA_TS_HEAD/src/test/typescript/hydra/test/$f" ]; then
        cp "$HYDRA_TS_HEAD/src/test/typescript/hydra/test/$f" \
           "$OUTPUT_DIR/src/test/typescript/hydra/test/$f"
    fi
done
# The libraries.ts primitive registry is hand-written under src/main/lib/
# (see gap 4 commit b6d603409). The TS host doesn't emit it (no DSL stub
# for it), but in the bootstrap layout it lives where testEnv expects it.
if [ -f "$HYDRA_TS_HEAD/src/main/typescript/hydra/lib/libraries.ts" ]; then
    cp "$HYDRA_TS_HEAD/src/main/typescript/hydra/lib/libraries.ts" \
       "$OUTPUT_DIR/src/main/typescript/hydra/lib/libraries.ts"
fi

# Resolve HYDRA_JSON_DIR so jsonBindings.ts can locate the kernel JSON.
# The bootstrap demo's output dir doesn't have a `dist/json` sibling
# (lives in /tmp), so the test runner's upward-walk fallback fails. Use
# the repo's dist/json regardless of cwd.
export HYDRA_JSON_DIR="${HYDRA_JSON_DIR:-$HYDRA_ROOT/dist/json/hydra-kernel/src/main/json}"

cd "$OUTPUT_DIR"

START_MS=$(python3 -c 'import time; print(int(time.monotonic() * 1000))')
VITEST_LOG=$(mktemp)
VITEST_JSON="$OUTPUT_DIR/vitest-out.json"

set +e
# `npm test` runs `vitest run --reporter=json --outputFile=vitest-out.json`
# (see package.json template). Capture exit code; vitest exits non-zero on
# any failure but still writes the JSON.
npm test 2>&1 | tee "$VITEST_LOG"
EXIT_CODE=${PIPESTATUS[0]}
set -e
END_MS=$(python3 -c 'import time; print(int(time.monotonic() * 1000))')
ELAPSED_MS=$((END_MS - START_MS))

# Write benchmark JSON if requested
if [ -n "${HYDRA_BENCHMARK_OUTPUT:-}" ]; then
    PASSED=0
    FAILED=0
    SKIPPED=0
    if [ -f "$VITEST_JSON" ]; then
        PASSED=$(python3 -c "import json; d=json.load(open('$VITEST_JSON')); print(d.get('numPassedTests', 0))" 2>/dev/null || echo 0)
        FAILED=$(python3 -c "import json; d=json.load(open('$VITEST_JSON')); print(d.get('numFailedTests', 0))" 2>/dev/null || echo 0)
        SKIPPED=$(python3 -c "import json; d=json.load(open('$VITEST_JSON')); print(d.get('numPendingTests', 0))" 2>/dev/null || echo 0)
    fi
    cat > "$HYDRA_BENCHMARK_OUTPUT" <<ENDJSON
{
  "groups": [],
  "summary": {
    "totalPassed": $PASSED,
    "totalFailed": $FAILED,
    "totalSkipped": $SKIPPED,
    "totalTimeMs": $ELAPSED_MS
  }
}
ENDJSON
    echo "Benchmark written to: $HYDRA_BENCHMARK_OUTPUT"
fi

rm -f "$VITEST_LOG"
exit $EXIT_CODE
