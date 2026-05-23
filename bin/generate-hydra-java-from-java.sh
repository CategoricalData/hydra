#!/usr/bin/env bash
# Generate dist/json/hydra-java entirely from the Java DSL sources.
#
# Companion to bin/generate-hydra-python-from-python.sh; issue #344.
# Checks whether the Java host is built (kernel JSON + dist/java/hydra-java
# compiled), runs `bin/sync-java.sh` to build it if not, then invokes the
# Java self-host driver (hydra.JavaSelfHostDemo).
#
# Usage:
#   bin/generate-hydra-java-from-java.sh                   # default settings
#   bin/generate-hydra-java-from-java.sh --out-root DIR    # override output
#   bin/generate-hydra-java-from-java.sh --force-rebuild   # run sync-java even
#                                                          # if host present
#   bin/generate-hydra-java-from-java.sh --compare         # after generation,
#                                                          # byte-compare to the
#                                                          # Haskell-generated
#                                                          # canonical
#   bin/generate-hydra-java-from-java.sh --help

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/.." && pwd )"

CANON_ROOT="$HYDRA_ROOT/dist/json/hydra-java/src/main/json"
OUT_ROOT="$CANON_ROOT"
USER_SET_OUT_ROOT=0
FORCE_REBUILD=0
DO_COMPARE=0
EXTRA_ARGS=()

while [ $# -gt 0 ]; do
    case "$1" in
        --out-root) OUT_ROOT="$2"; USER_SET_OUT_ROOT=1; shift 2 ;;
        --out-root=*) OUT_ROOT="${1#--out-root=}"; USER_SET_OUT_ROOT=1; shift ;;
        --force-rebuild) FORCE_REBUILD=1; shift ;;
        --compare) DO_COMPARE=1; shift ;;
        --help|-h)
            sed -n '2,/^$/p' "$0" | sed 's/^# \{0,1\}//'
            exit 0 ;;
        *) EXTRA_ARGS+=("$1"); shift ;;
    esac
done

# Ensure all dist trees this script needs are present and up to date.
# heads/java's headsExtras source set (compiled below by gradle) imports
# hydra.{python,haskell,lisp,scala,go}.* from dist/java/hydra-*/, so we
# need a full sync — not just sync-java. Warm-cache full sync is fast
# (~3 min); cold-cache is self-healing.
#
# HYDRA_IN_SYNC=1 indicates sync.sh is already calling us (Phase 5);
# don't recurse.
if [ "${HYDRA_IN_SYNC:-0}" != "1" ]; then
    if [ "$FORCE_REBUILD" = "1" ]; then
        echo "=== Forcing full sync rebuild via bin/sync.sh ==="
    else
        echo "=== Running bin/sync.sh to ensure all dist trees are current ==="
    fi
    "$HYDRA_ROOT/bin/sync.sh"
fi

# Check whether Java DSL source files exist at all. They live under
# packages/hydra-java/src/main/java/hydra/sources/java/. As of 2026-05-11
# these are being ported from the Haskell DSL sources; the Java self-host
# is only usable once that port is complete.
SOURCES_DIR="$HYDRA_ROOT/packages/hydra-java/src/main/java/hydra/sources/java"
if [ ! -d "$SOURCES_DIR" ]; then
    echo ""
    echo "WARNING: $SOURCES_DIR does not exist."
    echo "The Java DSL sources for the hydra-java coder have not been ported yet."
    echo "See feature_344_self_hosting_coders-plan.md."
    echo ""
fi

# Compile the Java rollup (which includes JavaSelfHostDemo) and get classpath.
cd "$HYDRA_ROOT"
echo ""
echo "=== Compiling Java head (rollup) ==="
./gradlew --quiet :hydra-java:compileHeadsExtrasJava
echo ""
echo "=== Resolving classpath ==="
JAVA_CP=$(./gradlew --quiet :hydra-java:printHeadsExtrasRuntimeClasspath)

echo ""
echo "=== Running JavaSelfHostDemo ==="
# -Xss large for deeply nested type inference; -Xmx large for many bindings.

# If --compare was requested without an explicit --out-root, write to a
# temp directory so we can compare against the in-tree canonical (which
# would otherwise overwrite itself).
ACTUAL_OUT_ROOT="$OUT_ROOT"
COMPARE_CLEANUP=""
if [ "$DO_COMPARE" = "1" ] && [ "$USER_SET_OUT_ROOT" = "0" ]; then
    ACTUAL_OUT_ROOT="$(mktemp -d)/json"
    COMPARE_CLEANUP="$(dirname "$ACTUAL_OUT_ROOT")"
    mkdir -p "$ACTUAL_OUT_ROOT"
fi

java -Xss64m -Xmx8g -cp "$JAVA_CP" hydra.JavaSelfHostDemo \
    --hydra-root "$HYDRA_ROOT" --out-root "$ACTUAL_OUT_ROOT" \
    "${EXTRA_ARGS[@]+"${EXTRA_ARGS[@]}"}"

if [ "$DO_COMPARE" = "1" ]; then
    # Per-package driver writes under <dist-json-root>/hydra-java/src/main/json/.
    # When --out-root was the per-package tail path (canonical mode), the demo
    # strips four segments to recover the dist-json root and writes back
    # through the same tail; ours_path matches the original out-root. When
    # --out-root was a tmp directory (the default --compare-without-out-root
    # flow), the demo treats it as the dist-json root, so the actual
    # per-package output lives at <out-root>/hydra-java/src/main/json/.
    OURS_PATH="$ACTUAL_OUT_ROOT"
    if [ "$USER_SET_OUT_ROOT" = "0" ]; then
        OURS_PATH="$ACTUAL_OUT_ROOT/hydra-java/src/main/json"
    fi
    echo ""
    echo "=== Byte-comparing $OURS_PATH against the Haskell-generated canonical at $CANON_ROOT ==="
    python3 - "$OURS_PATH" "$CANON_ROOT" <<'PY'
import os, subprocess, sys
ours_root, canon_root = sys.argv[1], sys.argv[2]
canon_dir = os.path.join(canon_root, "hydra", "java")
ours_dir = os.path.join(ours_root, "hydra", "java")
if not os.path.isdir(canon_dir):
    print(f"  No canonical at {canon_dir}; cannot compare", file=sys.stderr)
    sys.exit(1)
modules = sorted(f[:-5] for f in os.listdir(canon_dir) if f.endswith(".json"))
print(f"{'module':>16}  {'status':<10}  {'ours':>10}  {'canon':>10}  {'delta':>8}  {'diff lines':>10}")
print("-" * 75)
eq = 0
for m in modules:
    o = os.path.join(ours_dir, f"{m}.json")
    c = os.path.join(canon_dir, f"{m}.json")
    if not os.path.exists(o):
        print(f"{m:>16}  MISSING")
        continue
    so = os.path.getsize(o); sc = os.path.getsize(c)
    r = subprocess.run(["diff", o, c], capture_output=True, text=True)
    if r.returncode == 0:
        print(f"{m:>16}  {'BYTE-EQ':<10}  {so:>10}  {sc:>10}  {so-sc:>+8}  {'0':>10}")
        eq += 1
    else:
        n = len(r.stdout.splitlines())
        print(f"{m:>16}  {'DIFFER':<10}  {so:>10}  {sc:>10}  {so-sc:>+8}  {n:>10}")
print("-" * 75)
print(f"Summary: {eq} / {len(modules)} byte-identical")
sys.exit(0 if eq == len(modules) else 1)
PY
    if [ -n "$COMPARE_CLEANUP" ]; then
        rm -rf "$COMPARE_CLEANUP"
    fi
fi
