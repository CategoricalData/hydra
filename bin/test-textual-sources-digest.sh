#!/usr/bin/env bash
# End-to-end test (#837): editing ONLY a textually-sourced .hydra file must invalidate the Phase-1 cache.
# Works on a scratch copy of dist/json (never the real one), with "textualSources": "all" on hydra-kernel:
#   run 1  update-json-main            (cold or warm; primes the scratch digest)
#   run 2  update-json-main, no edits  -> must report a cache hit
#   edit   one .hydra definition's description (the edit is reverted on exit)
#   run 3  update-json-main            -> must NOT be a cache hit, and the scratch JSON must contain the edit
# Run under bin/with-stack-slot.sh; needs a built update-json-main.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
HEAP=10g
MIN_MB=7000
while [ $# -gt 0 ]; do
  case "$1" in
    --heap) HEAP="$2"; shift ;;
    --min-mb) MIN_MB="$2"; shift ;;
    *) echo "unknown argument: $1" >&2; exit 2 ;;
  esac
  shift
done

AVAIL_MB=$(free -m | awk '/^Mem:/ {print $7}')
if [ "$AVAIL_MB" -lt "$MIN_MB" ]; then
  echo "test-textual-sources-digest: only ${AVAIL_MB} MB available (< ${MIN_MB}); refusing to start" >&2
  exit 2
fi
case "$HEAP" in
  *g) HEAP_MB=$(( ${HEAP%g} * 1024 )) ;;
  *m) HEAP_MB=${HEAP%m} ;;
  *) echo "--heap must end in g or m" >&2; exit 2 ;;
esac
SPARE_MB=$(( AVAIL_MB - 3000 ))
if [ "$HEAP_MB" -gt "$SPARE_MB" ]; then HEAP_MB=$SPARE_MB; fi

PKG_JSON="$ROOT/packages/hydra-kernel/package.json"
PROBE_FILE="$ROOT/packages/hydra-kernel/src/main/hydra/hydra/core/arity.hydra"
PROBE_JSON_REL="hydra-kernel/src/main/json/hydra/core/arity.json"
PROBE_TEXT="digest probe"

if ! grep -q '"textualSources"' "$PKG_JSON"; then
  echo "test-textual-sources-digest: hydra-kernel does not declare textualSources; nothing to test" >&2
  exit 2
fi

SCRATCH="$(mktemp -d "${TMPDIR:-/tmp}/test-textual-digest.XXXXXX")"
PROBE_BACKUP="$SCRATCH/arity.hydra.orig"
cp "$PROBE_FILE" "$PROBE_BACKUP"
cleanup() {
  cp "$PROBE_BACKUP" "$PROBE_FILE" 2>/dev/null || true
  rm -rf "$SCRATCH"
}
trap cleanup EXIT

cp -a "$ROOT/dist/json" "$SCRATCH/json"
run() {
  (cd "$ROOT/heads/haskell" && stack exec update-json-main -- --dist-root "$SCRATCH/json" +RTS -M"${HEAP_MB}m" -RTS) > "$SCRATCH/$1.log" 2>&1 \
    || { echo "test-textual-sources-digest: update-json-main FAILED in $1" >&2; tail -20 "$SCRATCH/$1.log" >&2; exit 4; }
}

echo "test-textual-sources-digest: run 1 (prime) ..."
run run1
echo "test-textual-sources-digest: run 2 (no edits; expect cache hit) ..."
run run2
if ! grep -q "Cache hit" "$SCRATCH/run2.log"; then
  echo "test-textual-sources-digest: FAILED: run 2 with no edits was not a cache hit" >&2
  tail -20 "$SCRATCH/run2.log" >&2
  exit 1
fi

echo "test-textual-sources-digest: editing one description in $(basename "$PROBE_FILE") ..."
python3 - "$PROBE_FILE" "$PROBE_TEXT" <<'PY'
import sys
p, text = sys.argv[1], sys.argv[2]
lines = open(p).read().split("\n")
for i, l in enumerate(lines):
    if l.startswith("## ") and not l.startswith("## @") and i + 1 < len(lines) and lines[i + 1].startswith("term "):
        lines[i] = l + " (" + text + ")"
        break
else:
    raise SystemExit("no suitable '## ...' line before a term in " + p)
open(p, "w").write("\n".join(lines))
PY

echo "test-textual-sources-digest: run 3 (after the .hydra-only edit; must regenerate) ..."
run run3
if grep -q "Cache hit" "$SCRATCH/run3.log"; then
  echo "test-textual-sources-digest: FAILED: run 3 was a cache hit despite the .hydra edit" >&2
  exit 1
fi
if grep -q "$PROBE_TEXT" "$SCRATCH/json/$PROBE_JSON_REL"; then
  echo "test-textual-sources-digest: OK (a .hydra-only edit regenerated the JSON)"
else
  echo "test-textual-sources-digest: FAILED: run 3 regenerated but the JSON lacks the edit" >&2
  exit 1
fi
