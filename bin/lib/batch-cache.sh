#!/usr/bin/env bash
# Shared warm-cache check for Layer 2 batch assemblers.
#
# Usage: batch_cache_fresh <dist_root> <json_root>
#   <dist_root>  e.g. "$HYDRA_ROOT_DIR/dist/haskell"
#   <json_root>  e.g. "$HYDRA_ROOT_DIR/dist/json"
#
# Returns 0 (fresh, skip work) if every per-package
# <dist_root>/<pkg>/digest.json's recorded inputs match
# <json_root>/<pkg>/digest.json's hashes. Returns 1 otherwise.
#
# Pure Python, no stack invocation. Runs in <1s on a cold cache check.
batch_cache_fresh() {
    local dist_root="$1"
    local json_root="$2"
    python3 - "$dist_root" "$json_root" <<'PYEOF'
import json, os, sys
dist_root, json_root = sys.argv[1], sys.argv[2]
if not os.path.isdir(dist_root):
    sys.exit(1)
any_pkg = False
for entry in sorted(os.listdir(dist_root)):
    pkg_dir = os.path.join(dist_root, entry)
    if not os.path.isdir(pkg_dir):
        continue
    out_digest_path = os.path.join(pkg_dir, "digest.json")
    in_digest_path = os.path.join(json_root, entry, "digest.json")
    if not os.path.isfile(out_digest_path) or not os.path.isfile(in_digest_path):
        sys.exit(1)
    try:
        out_d = json.load(open(out_digest_path))
        in_d = json.load(open(in_digest_path))
    except Exception:
        sys.exit(1)
    recorded = {k: v.get("hash") if isinstance(v, dict) else v
                for k, v in out_d.get("inputs", {}).items()}
    current = in_d.get("hashes", in_d)
    if recorded != current:
        sys.exit(1)
    any_pkg = True
sys.exit(0 if any_pkg else 1)
PYEOF
}
