#!/usr/bin/env bash
# #703 bootstrap patch (Step 8, extending-hydra-core.md): the PUBLISHED hydra-haskell
# host's Coder predates #684's "always import Data.Void" emission rule, so the
# Java-host cold-seed of dist/haskell/hydra-kernel omits `import Data.Void` from
# Hydra/Dsl/Lib/Functions.hs even though it references the Void type (via `absurd`).
# Without this import, the local Haskell host's own `stack build` fails before it can
# even run — a chicken-and-egg the seed step alone can't resolve (a LOCAL, currently-
# correct hydra-haskell host would emit the import; but nothing can build that host
# without first getting past this file).
#
# This patch is minimal and self-verifying: it is a deliberate bootstrap patch, not a
# permanent source change (dist/haskell/ is generated) — the very next
# GENERATOR_HOST=haskell regeneration (run by a build of the host this patch enables)
# emits Hydra/Dsl/Lib/Functions.hs itself, complete with the import, from the CURRENT
# (locally-built) coder — overwriting this patch with equivalent, generator-produced
# content. See docs/build-system.md for the invariant this documents.
#
# Usage:
#   heads/java/bin/patch-void-import.sh [--repo-root DIR]
set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_JAVA_HEAD="$( cd "$SCRIPT_DIR/.." && pwd )"

REPO_ROOT="${HYDRA_ROOT_DIR:-}"
while [ $# -gt 0 ]; do
    case "$1" in
        --repo-root) REPO_ROOT="$2"; shift 2 ;;
        --repo-root=*) REPO_ROOT="${1#--repo-root=}"; shift ;;
        *) echo "Unknown argument: $1" >&2; exit 2 ;;
    esac
done
if [ -z "$REPO_ROOT" ]; then
    REPO_ROOT="$( cd "$HYDRA_JAVA_HEAD/../.." && pwd )"
fi

TARGET="$REPO_ROOT/dist/haskell/hydra-kernel/src/main/haskell/Hydra/Dsl/Lib/Functions.hs"
if [ ! -f "$TARGET" ]; then
    echo "patch-void-import.sh: $TARGET not found (did the seed step run first?)" >&2
    exit 1
fi

if grep -q '^import Data.Void' "$TARGET"; then
    echo "patch-void-import.sh: Data.Void already imported in $TARGET; nothing to patch."
    exit 0
fi

if ! grep -q '^module Hydra.Dsl.Lib.Functions where$' "$TARGET"; then
    echo "patch-void-import.sh: expected module header not found in $TARGET; refusing to patch blind." >&2
    exit 1
fi

python3 - "$TARGET" <<'PY'
import sys
path = sys.argv[1]
with open(path) as f:
    text = f.read()
marker = "module Hydra.Dsl.Lib.Functions where\n"
patch = (
    "\n"
    "-- #703 bootstrap patch (Step 8): the published hydra-haskell coder predates #684's\n"
    "-- \"always import Data.Void\" rule; this import unblocks a local Haskell host build.\n"
    "-- The next GENERATOR_HOST=haskell regeneration overwrites this file, import included.\n"
    "import Data.Void (Void)\n"
)
idx = text.index(marker) + len(marker)
text = text[:idx] + patch + text[idx:]
with open(path, 'w') as f:
    f.write(text)
PY

echo "patch-void-import.sh: patched $TARGET"
