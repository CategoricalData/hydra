#!/usr/bin/env bash
# Identity guard for textual sources (#837): regenerate hydra-kernel's dist/json from the .hydra sources into a
# SCRATCH dist root with update-json-main (the bounded-memory per-package pipeline) and diff the result against
# the real dist/json. Passes iff there is no difference.
#
# Usage (run under bin/with-stack-slot.sh; needs a built update-json-main):
#   bin/verify-hydra-sources.sh [--flip | --dsl] [--keep] [--heap 10g] [--min-mb 14000]
# --flip  temporarily sets "textualSources": "all" in packages/hydra-kernel/package.json for the run (restored on
#         exit); without it the package.json flag decides.
# --dsl   the OTHER direction of the dual-edit check: temporarily removes the "textualSources" fields so Phase 1
#         regenerates from the Haskell DSL modules, and diffs that against the committed dist/json (which came from
#         the .hydra sources). A difference means the DSL copy has drifted from its .hydra file.
#
# Writes ONLY under the scratch root (digests and per-package caches live under the dist root); this script checks
# that nothing under dist/ or heads/haskell/.stack-work was modified outside it.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PKG_JSON="$ROOT/packages/hydra-kernel/package.json"
if ! grep -q '"textualSources"' "$PKG_JSON"; then
  echo "verify-hydra-sources: $PKG_JSON is missing textualSources — a previous --flip/--dsl run" >&2
  echo "verify-hydra-sources: was interrupted before its cleanup trap could restore it. Fix:" >&2
  echo "verify-hydra-sources:   git checkout -- packages/hydra-kernel/package.json" >&2
  echo "verify-hydra-sources: then re-run." >&2
  exit 2
fi
HEAP=10g
MIN_MB=7000
FLIP=0
DSL=0
KEEP=0
while [ $# -gt 0 ]; do
  case "$1" in
    --flip) FLIP=1 ;;
    --dsl) DSL=1 ;;
    --keep) KEEP=1 ;;
    --heap) HEAP="$2"; shift ;;
    --min-mb) MIN_MB="$2"; shift ;;
    *) echo "unknown argument: $1" >&2; exit 2 ;;
  esac
  shift
done

AVAIL_MB=$(free -m | awk '/^Mem:/ {print $7}')
if [ "$AVAIL_MB" -lt "$MIN_MB" ]; then
  echo "verify-hydra-sources: only ${AVAIL_MB} MB available (< ${MIN_MB}); refusing to start" >&2
  exit 2
fi
# Cap the Haskell heap at the requested size, but never above what the box can spare (leave 3 GB headroom).
case "$HEAP" in
  *g) HEAP_MB=$(( ${HEAP%g} * 1024 )) ;;
  *m) HEAP_MB=${HEAP%m} ;;
  *) echo "verify-hydra-sources: --heap must end in g or m" >&2; exit 2 ;;
esac
SPARE_MB=$(( AVAIL_MB - 3000 ))
if [ "$HEAP_MB" -gt "$SPARE_MB" ]; then HEAP_MB=$SPARE_MB; fi
HEAP="${HEAP_MB}m"

SCRATCH="$(mktemp -d "${TMPDIR:-/tmp}/verify-hydra-sources.XXXXXX")"
PKG_BACKUP=""
cleanup() {
  if [ -n "$PKG_BACKUP" ] && [ -f "$PKG_BACKUP" ]; then cp "$PKG_BACKUP" "$PKG_JSON"; fi
  if [ "$KEEP" = 1 ]; then echo "verify-hydra-sources: scratch kept at $SCRATCH" >&2; else rm -rf "$SCRATCH"; fi
}
# Trap INT/TERM too, not just EXIT: a plain EXIT trap does not fire on SIGKILL, but it also
# would not fire on SIGINT/SIGTERM delivered to a `set -e` script unless explicitly trapped —
# without this, a killed --flip/--dsl run leaves package.json permanently stripped (#711).
trap cleanup EXIT INT TERM

# Print the first lines of the unified diff of EVERY differing file (so each category of difference is visible),
# bounded to ~600 lines overall.
report_diffs() {
  local a="$1" b="$2" n=0
  diff -rq "$a" "$b" | awk '/^Files /{print $2" "$4}' | while read -r fa fb; do
    n=$((n+1)); [ "$n" -gt 60 ] && break
    echo "--- ${fa#$a/}" >&2
    diff -u "$fa" "$fb" | sed -n '3,14p' >&2 || true
  done
  return 0
}

if [ "$FLIP" = 1 ]; then
  PKG_BACKUP="$SCRATCH/package.json.orig"
  cp "$PKG_JSON" "$PKG_BACKUP"
  python3 - "$PKG_JSON" <<'PY'
import json, sys
p = sys.argv[1]
d = json.load(open(p))
d["textualSources"] = "all"
open(p, "w").write(json.dumps(d, indent=2) + "\n")
PY
fi

if [ "$DSL" = 1 ]; then
  PKG_BACKUP="$SCRATCH/package.json.orig"
  cp "$PKG_JSON" "$PKG_BACKUP"
  python3 - "$PKG_JSON" <<'PY'
import json, sys
p = sys.argv[1]
d = json.load(open(p))
d.pop("textualSources", None); d.pop("textualSourcesExclude", None)
open(p, "w").write(json.dumps(d, indent=2) + "\n")
PY
fi

echo "verify-hydra-sources: copying dist/json to $SCRATCH/json ..."
cp -a "$ROOT/dist/json" "$SCRATCH/json"
# The copied digests describe the REAL tree and would make both passes report a cache hit (skipping inference and
# writes), turning the comparison into a tautology. Drop every digest so the scratch run really regenerates.
find "$SCRATCH/json" -path '*/build/*' -name 'digest.json' -delete
rm -f "$SCRATCH/json/build/digest.json"
MARK="$SCRATCH/.marker"
touch "$MARK"

echo "verify-hydra-sources: regenerating from .hydra (heap cap $HEAP) ..."
(cd "$ROOT/heads/haskell" && stack exec update-json-main -- --dist-root "$SCRATCH/json" +RTS -M"$HEAP" -RTS)

STRAY=$(find "$ROOT/dist" "$ROOT/heads/haskell/.stack-work" -newer "$MARK" -type f 2>/dev/null | head -5 || true)
if [ -n "$STRAY" ]; then
  echo "verify-hydra-sources: ERROR: files outside the scratch root were modified:" >&2
  echo "$STRAY" >&2
  exit 3
fi

echo "verify-hydra-sources: comparing hydra-kernel JSON ..."
RC=0
if diff -r -q "$ROOT/dist/json/hydra-kernel/src/main/json" "$SCRATCH/json/hydra-kernel/src/main/json"; then
  echo "verify-hydra-sources: main modules OK (byte-identical to dist/json)"
else
  echo "verify-hydra-sources: FAILED: main modules differ (see above)" >&2
  report_diffs "$ROOT/dist/json/hydra-kernel/src/main/json" "$SCRATCH/json/hydra-kernel/src/main/json"
  RC=1
fi

echo "verify-hydra-sources: regenerating TEST modules from .hydra (heap cap $HEAP) ..."
(cd "$ROOT/heads/haskell" && stack exec update-json-test -- --dist-root "$SCRATCH/json" +RTS -M"$HEAP" -RTS) || { echo "verify-hydra-sources: update-json-test FAILED" >&2; exit 4; }
STRAY=$(find "$ROOT/dist" "$ROOT/heads/haskell/.stack-work" -newer "$MARK" -type f 2>/dev/null | head -5 || true)
if [ -n "$STRAY" ]; then
  echo "verify-hydra-sources: ERROR: files outside the scratch root were modified:" >&2
  echo "$STRAY" >&2
  exit 3
fi
if diff -r -q "$ROOT/dist/json/hydra-kernel/src/test/json" "$SCRATCH/json/hydra-kernel/src/test/json"; then
  echo "verify-hydra-sources: test modules OK (byte-identical to dist/json)"
else
  echo "verify-hydra-sources: FAILED: test modules differ (see above)" >&2
  report_diffs "$ROOT/dist/json/hydra-kernel/src/test/json" "$SCRATCH/json/hydra-kernel/src/test/json"
  RC=1
fi
if [ "$RC" = 0 ]; then
  echo "verify-hydra-sources: OK (the .hydra-sourced regeneration of main and test modules is byte-identical to dist/json)"
fi
exit $RC
