#!/usr/bin/env bash
# Generate dist/json/hydra-scala from the Scala DSL sources (#509).
#
# Companion to bin/generate-hydra-{java,python}-from-{java,python}.sh.
# Invokes the Scala DSL→JSON driver (hydra.updateScalaJson, in
# heads/scala/) via sbt to regenerate dist/json/hydra-scala/.
#
# Writes JSON for all 5 source modules: Syntax, Language, Utils, Serde, Coder.
# 4 of 5 are byte-identical to Haskell baseline; Coder.json is structurally
# correct but with 7 simplified function bodies (encodeTerm/encodeFunction/
# encodeCase/encodeLetBinding/encodeLocalDef/encodeComplexTermDef) that
# produce smaller JSON than the Haskell version. Use --compare to see deltas.
#
# Usage:
#   bin/generate-hydra-scala-from-scala.sh                   # default
#   bin/generate-hydra-scala-from-scala.sh --compare         # diff vs canonical
#   bin/generate-hydra-scala-from-scala.sh --help

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/.." && pwd )"

CANON_ROOT="$HYDRA_ROOT/dist/json/hydra-scala/src/main/json"
DO_COMPARE=0

while [ $# -gt 0 ]; do
    case "$1" in
        --compare) DO_COMPARE=1; shift ;;
        --help|-h)
            sed -n '2,/^$/p' "$0" | sed 's/^# \{0,1\}//'
            exit 0 ;;
        *) shift ;;
    esac
done

# Ensure JAVA_HOME is set for sbt.
if [ -z "${JAVA_HOME:-}" ]; then
    if [ -d "/usr/lib/jvm/java-17-openjdk-amd64" ]; then
        export JAVA_HOME=/usr/lib/jvm/java-17-openjdk-amd64
    fi
fi

# The driver requires the universe (kernel + jvm + java + scala dist trees)
# to be present. If we're being called from sync.sh, those are already
# fresh; otherwise call bin/sync.sh first.
if [ "${HYDRA_IN_SYNC:-0}" != "1" ]; then
    echo "=== Running bin/sync.sh to ensure all dist trees are current ==="
    "$HYDRA_ROOT/bin/sync.sh"
fi

# Snapshot canonical before the driver writes (for --compare).
SNAPSHOT_DIR=""
if [ "$DO_COMPARE" = "1" ]; then
    SNAPSHOT_DIR="$(mktemp -d)"
    cp -r "$CANON_ROOT" "$SNAPSHOT_DIR/json-baseline"
    echo "Snapshotted canonical to $SNAPSHOT_DIR/json-baseline for diff."
fi

# Run the Scala DSL→JSON driver via sbt.
echo "=== Running hydra.updateScalaJson via sbt ==="
cd "$HYDRA_ROOT/packages/hydra-scala"
sbt --no-colors "runMain hydra.updateScalaJson"

if [ "$DO_COMPARE" = "1" ]; then
    echo ""
    echo "=== Byte-comparing fresh dist against snapshot ==="
    python3 - "$CANON_ROOT" "$SNAPSHOT_DIR/json-baseline" <<'PY'
import os, subprocess, sys
fresh, snap = sys.argv[1], sys.argv[2]
fresh_dir = os.path.join(fresh, "hydra", "scala")
snap_dir = os.path.join(snap, "hydra", "scala")
modules = sorted(f[:-5] for f in os.listdir(snap_dir) if f.endswith(".json"))
print(f"{'module':>16}  {'status':<10}  {'fresh':>10}  {'snap':>10}  {'delta':>8}")
print("-" * 65)
eq = 0
for m in modules:
    f = os.path.join(fresh_dir, f"{m}.json")
    s = os.path.join(snap_dir, f"{m}.json")
    if not os.path.exists(f):
        print(f"{m:>16}  MISSING")
        continue
    sf = os.path.getsize(f); ss = os.path.getsize(s)
    r = subprocess.run(["diff", "-q", f, s], capture_output=True, text=True)
    if r.returncode == 0:
        print(f"{m:>16}  {'BYTE-EQ':<10}  {sf:>10}  {ss:>10}  {sf-ss:>+8}")
        eq += 1
    else:
        print(f"{m:>16}  {'DIFFER':<10}  {sf:>10}  {ss:>10}  {sf-ss:>+8}")
print("-" * 65)
print(f"Summary: {eq} / {len(modules)} byte-identical")
sys.exit(0 if eq == len(modules) else 1)
PY
    rm -rf "$SNAPSHOT_DIR"
fi
