#!/usr/bin/env bash
# setup-external-worktree.sh — per-machine: create the local-only `external`
# worktree and populate the three submodules there. This is the ONE populated
# tree; every other worktree stays unpopulated (see sweep-worktree-optout.sh).
#
# Run once per machine, AFTER the .gitmodules + gitlinks have landed on main
# (so `main` carries the submodule structure this checks out).
#
# The `external` branch is LOCAL-ONLY and NEVER pushed — it exists only to be the
# worktree where `submodule update` runs. Its dir name `external` is FIXED: the
# hook paths in every worktree are `../../external/agents/...`; renaming this dir
# silently breaks them (see 583-cutover-runbook.md).
#
# Idempotent: if the external worktree already exists, re-runs submodule update.
#
# Usage:
#   bin/setup-external-worktree.sh --dry-run
#   bin/setup-external-worktree.sh

set -euo pipefail

DRY=""
[ "${1:-}" = "--dry-run" ] && DRY=1

THIS_ROOT="$(git rev-parse --show-toplevel)"
WORKTREES_DIR="$(dirname "$THIS_ROOT")"          # .../hydra/worktrees
HYDRA_ROOT="$(dirname "$WORKTREES_DIR")"         # .../hydra
COMMON_DIR="$(git rev-parse --git-common-dir)"   # .../hydra/hydra.git
EXTERNAL_DIR="$HYDRA_ROOT/external"

run() { if [ -n "$DRY" ]; then printf '  DRY: %s\n' "$*"; else echo "+ $*"; "$@"; fi; }

echo "Hydra root:      $HYDRA_ROOT"
echo "External worktree: $EXTERNAL_DIR (branch 'external', local-only)"
[ -n "$DRY" ] && echo "(dry-run)"
echo

# 0. If my earlier ad-hoc loose clone is sitting at external/agents (pre-submodule
#    era), it must go — the real populated copy comes from the submodule. Only
#    remove it if external/ is NOT yet a worktree (i.e. it's the stray clone).
if [ -d "$EXTERNAL_DIR" ] && ! git -C "$EXTERNAL_DIR" rev-parse --is-inside-work-tree >/dev/null 2>&1; then
    echo "external/ exists but is not a git worktree — treating as the pre-submodule stray clone; removing."
    run rm -rf "$EXTERNAL_DIR"
fi

# 1. Create the external worktree off main (local-only branch 'external'), unless present.
if git worktree list --porcelain | awk '/^worktree /{print $2}' | grep -qx "$EXTERNAL_DIR"; then
    echo "external worktree already registered — will refresh submodules."
else
    # Branch 'external' off origin/main; never pushed.
    if git show-ref --verify --quiet refs/heads/external; then
        run git -C "$COMMON_DIR" worktree add "$EXTERNAL_DIR" external
    else
        run git -C "$COMMON_DIR" worktree add -b external "$EXTERNAL_DIR" origin/main
    fi
fi

# 1b. core.bare=false on the external worktree — REQUIRED. This is a bare repo
# (hydra.git shared config core.bare=true); without a per-worktree override the
# external worktree is treated as bare and `git submodule update` below fails with
# "git-submodule cannot be used without a working tree". Must set worktreeConfig +
# core.bare=false here, before the submodule update. (Incident 2026-09-15.)
run git -C "$EXTERNAL_DIR" config extensions.worktreeConfig true
run git -C "$EXTERNAL_DIR" config --worktree core.bare false

# 2. Populate the three submodules IN the external worktree.
echo "Populating submodules in the external worktree..."
run git -C "$EXTERNAL_DIR" submodule update --init agents wiki coordination

# 3. Report.
echo
echo "Populated:"
for name in agents wiki coordination; do
    if [ -e "$EXTERNAL_DIR/$name/.git" ]; then
        echo "  $EXTERNAL_DIR/$name  ($(git -C "$EXTERNAL_DIR/$name" rev-parse --short HEAD 2>/dev/null || echo '?'))"
    else
        echo "  $EXTERNAL_DIR/$name  — NOT populated (check submodule update output)"
    fi
done
echo
echo "Reach from any worktree via ../../external/{agents,wiki,coordination}."
echo "Do NOT rename the external/ directory (hook paths encode it)."
