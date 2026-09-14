#!/usr/bin/env bash
#
# install-git-hooks.sh — install Hydra's shared git hooks into the bare repo.
#
# Installs bin/pre-push-bootstrap-gate.sh as .git/hooks/pre-push. Because Hydra
# uses a shared bare repo (hydra.git/), the hook lives once in hydra.git/hooks/
# and fires for every worktree's pushes. It only gates pushes to origin/main
# (the bootstrap land gate); all other pushes pass through.
#
# Idempotent: safe to re-run. Run from any worktree root.
#
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
gate="$script_dir/pre-push-bootstrap-gate.sh"
[ -f "$gate" ] || { echo "error: $gate not found" >&2; exit 1; }
chmod +x "$gate"

hooks_dir="$(git rev-parse --git-path hooks)"
mkdir -p "$hooks_dir"
target="$hooks_dir/pre-push"

# A tiny dispatcher, so the versioned gate script stays the single source of
# truth (edits to bin/ take effect without re-installing). The dispatcher
# resolves the gate relative to THIS worktree's bin/ — but since the hook is
# shared across worktrees, we point it at an absolute path captured at install.
cat > "$target" <<EOF
#!/usr/bin/env bash
# Auto-installed by bin/install-git-hooks.sh — do not edit.
# Dispatches to the versioned bootstrap land gate.
exec "$gate" "\$@"
EOF
chmod +x "$target"

echo "installed pre-push bootstrap gate:"
echo "  hook:    $target"
echo "  → gate:  $gate"
echo
echo "This gates pushes to origin/main across ALL worktrees sharing this bare repo."
echo "Verify:  git rev-parse --git-path hooks  →  $hooks_dir"
