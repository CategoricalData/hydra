#!/usr/bin/env bash
# add-external-submodules.sh — author the three "external" git submodules
# (agents, wiki, coordination) as FLOATING gitlinks at the Hydra repo root.
#
# WHAT THIS DOES (once, on the authoring branch — feature_583):
#   - `git submodule add` for agents / wiki / coordination, committing
#     .gitmodules + 3 gitlinks with root-relative paths `agents`/`wiki`/
#     `coordination`. (Those materialize under hydra/external/ ONLY in the
#     `external` worktree, because that worktree's root dir is named `external`;
#     the committed path string is NOT `external/agents`.)
#   - marks all three FLOATING: branch = main/master/coordination, ignore = all
#     (a floated submodule never churns Hydra's main; we always take latest).
#   - DEINITs them in THIS worktree so the authoring worktree is left correctly
#     UNPOPULATED (the gitlinks stay committed; only the working copies go away).
#   - removes the defensive `wiki/` line from .gitignore (an ignored path can't
#     carry a tracked gitlink).
#
# WHAT THIS DOES NOT DO: it does not create the `external` worktree, does not
# populate anything, does not touch other worktrees' config, and does not touch
# the live hydra/wiki or hydra/coordination loose checkouts. Those are separate
# steps (per-machine external setup; the fleet-wide opt-out sweep). See
# 583-cutover-runbook.md.
#
# Usage:
#   bin/add-external-submodules.sh --dry-run   # print what it would do
#   bin/add-external-submodules.sh             # do it (must be run from the
#                                              # feature_583 worktree root)

set -euo pipefail

DRY=""
[ "${1:-}" = "--dry-run" ] && DRY=1

AGENTS_URL="git@github-CategoricalData-hydra-agents:CategoricalData/hydra-agents.git"
WIKI_URL="git@github-CategoricalData-hydra:CategoricalData/hydra.wiki.git"

run() {
    if [ -n "$DRY" ]; then printf '  DRY: %s\n' "$*"; else echo "+ $*"; "$@"; fi
}

ROOT="$(git rev-parse --show-toplevel)"
cd "$ROOT"
echo "Authoring external submodules at repo root: $ROOT"
[ -n "$DRY" ] && echo "(dry-run — no changes will be made)"

# 0. Guard: only from a worktree, and refuse if .gitmodules already has entries.
if [ -f .gitmodules ] && grep -q 'submodule "agents"' .gitmodules 2>/dev/null; then
    echo "error: .gitmodules already defines 'agents' — already authored. Aborting." >&2
    exit 1
fi

# 1. Remove the defensive `wiki/` ignore (blocks a tracked wiki gitlink).
if grep -qxF 'wiki/' .gitignore 2>/dev/null; then
    echo "Removing 'wiki/' from .gitignore (would block the wiki gitlink)..."
    run sed -i '/^wiki\/$/d' .gitignore
    # also drop the now-orphaned '# Hydra wiki' comment if it's immediately above
    run sed -i '/^# Hydra wiki$/d' .gitignore
fi

# 2. Add the three submodules. Root-relative paths agents/wiki/coordination.
#    wiki + coordination share the hydra.wiki.git URL, differing only by branch.
echo "Adding submodule: agents (hydra-agents / main)"
run git submodule add --name agents -b main "$AGENTS_URL" agents

echo "Adding submodule: wiki (hydra.wiki / master)"
run git submodule add --name wiki -b master "$WIKI_URL" wiki

echo "Adding submodule: coordination (hydra.wiki / coordination)"
run git submodule add --name coordination -b coordination "$WIKI_URL" coordination

# 3. Mark all three FLOATING: ignore = all (branch already set via -b above).
for name in agents wiki coordination; do
    echo "Setting submodule.$name.ignore = all (floating; never churn main)"
    run git config -f .gitmodules "submodule.$name.ignore" all
done
run git add .gitmodules

# 4. DEINIT in THIS worktree — leave the authoring worktree UNPOPULATED.
#    The gitlinks remain committed; the working copies are removed here.
echo "De-initializing the three submodules in THIS (authoring) worktree..."
run git submodule deinit -f agents wiki coordination

echo
echo "Done. Review with: git status && git diff --cached .gitmodules"
echo "Then commit .gitmodules + the 3 gitlinks (+ the .gitignore edit) together"
echo "with the doc-subset. staging lands the commit on origin/main."
