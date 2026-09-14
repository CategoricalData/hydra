#!/usr/bin/env bash
# sweep-worktree-optout.sh — opt EVERY non-`external` worktree OUT of populating
# the three submodules (agents, wiki, coordination), so only the `external`
# worktree ever materializes them. Run once per machine after the .gitmodules +
# gitlinks land on main (so every worktree carries the gitlinks).
#
# WHY: a submodule gitlink exists in every worktree's tree, but whether a worktree
# POPULATES it is governed by that worktree's out-of-band `config.worktree`
# (submodule.<name>.update = none). Without the opt-out, a stray `git submodule
# update` in any worktree clones a full copy of the submodule store into a tree
# whose layout/permissions assume it isn't there (see 583-cutover-runbook.md
# Caution 2). So we opt out EVERY submodule in EVERY non-external worktree.
#
# THREE CAUTIONS baked in:
#  1. `extensions.worktreeConfig true` is set FIRST (per worktree), before any
#     `--worktree` write — else the write leaks into the shared config.
#  2. Opt out ALL THREE submodules, including agents — not just the heavy ones.
#  3. (sparse-checkout is not used here; if ever added, use non-cone mode.)
#
# Idempotent: re-running just re-asserts the same config values.
#
# Usage:
#   bin/sweep-worktree-optout.sh --dry-run
#   bin/sweep-worktree-optout.sh              # apply to all non-external worktrees

set -euo pipefail

DRY=""
[ "${1:-}" = "--dry-run" ] && DRY=1

SUBMODULES=(agents wiki coordination)

# Repo root of THIS worktree, then the worktrees/ dir (sibling under hydra/).
THIS_ROOT="$(git rev-parse --show-toplevel)"
WORKTREES_DIR="$(dirname "$THIS_ROOT")"          # .../hydra/worktrees
HYDRA_ROOT="$(dirname "$WORKTREES_DIR")"         # .../hydra

run() { if [ -n "$DRY" ]; then printf '    DRY: %s\n' "$*"; else echo "    + $*"; "$@"; fi; }

echo "Opt-out sweep — worktrees under: $WORKTREES_DIR"
echo "Submodules: ${SUBMODULES[*]}"
[ -n "$DRY" ] && echo "(dry-run)"
echo

# Every registered worktree except the `external` one (which IS the populated
# tree and must keep default update behavior). Use `git worktree list` so we
# catch all of them, not just a dir glob.
mapfile -t WT_PATHS < <(git worktree list --porcelain | awk '/^worktree /{print $2}')

for wt in "${WT_PATHS[@]}"; do
    base="$(basename "$wt")"
    # Skip the bare repo dir and the external worktree.
    case "$base" in
        hydra.git|external) echo "  skip $base (bare repo or the populated 'external' tree)"; continue ;;
    esac
    # Only operate on real worktrees that carry the gitlinks (post-land).
    [ -d "$wt" ] || { echo "  skip $base (path absent)"; continue; }

    echo "  worktree: $base"
    # (1) worktreeConfig TRUE first — MUST precede any --worktree write.
    run git -C "$wt" config extensions.worktreeConfig true
    # (1b) core.bare=false per worktree — REQUIRED. This is a bare repo
    # (hydra.git shared config has core.bare=true). Turning on
    # extensions.worktreeConfig makes every worktree read core.bare from the
    # SHARED config unless overridden per-worktree, so without this line every
    # worktree becomes "bare" and `git status`/`rev-parse --is-inside-work-tree`
    # fail with "must be run in a work tree". Must be written to config.worktree,
    # right after enabling the extension. (Incident 2026-09-15: the omission of
    # this line broke all ~69 alpha worktrees fleet-wide.)
    run git -C "$wt" config --worktree core.bare false
    # (2) opt out every submodule.
    for name in "${SUBMODULES[@]}"; do
        run git -C "$wt" config --worktree "submodule.$name.update" none
    done
done

echo
echo "Done. Verify a sample with:"
echo "  git -C <worktree> config --worktree --get-regexp 'submodule\\..*\\.update'"
