#!/usr/bin/env bash
# Spawn a worktree + tmux session + Claude for a specific GitHub issue.
#
# Usage:
#   COORDINATOR=<coord-worktree> bin/spawn-issue-worktree.sh <issue-number> <slug> [<issue-title>] [--force-respawn]
# Example:
#   COORDINATOR=staging-gce bin/spawn-issue-worktree.sh 425 clojure_json_decode "Clojure host's JSON decoder rejects kernel JSON"
#   COORDINATOR=release_508_hydra_0_17 TYPE=task PARENT=508 \
#     bin/spawn-issue-worktree.sh 557 worker_hierarchy "Establish a principled worker + issue hierarchy"
#
# $COORDINATOR names the worktree whose inbox the new agent will address —
# the entity that owns the ready-to-stage / question-answering relationship.
# It varies per machine and per epoch (which session is currently coordinating);
# there is no sensible hardcoded default, so the operator must set it.
#
# $TYPE selects the branch prefix so the branch name carries the issue type,
# mirroring the issue tree (see claude/agent-hierarchy.md). One of
# bug|feature|task|release; default bug. The branch, worktree, and tmux session
# are all named ${TYPE}_${NUM}_${SLUG}.
#
# $PARENT is the parent issue number this agent's issue is a child of. It is
# recorded in the seeded briefing so the agent/coordinator relationship is
# derivable from the issue parent (every non-release issue should declare one;
# see the mandatory-parent rule in claude/agent-hierarchy.md). Optional but
# strongly recommended for non-release agents; omit for a release_* root.
#
# $SPAWN_MODEL selects the model (default opusplan for bug/task agents). The
# per-class model policy lives in claude/agent-hierarchy.md — release and
# high-stakes coordinators run fable, staging runs opus, simple agents opusplan.
#
# Refuses to spawn if worktrees/closed/ already contains a plan-doc for this
# issue (a prior agent addressed it; the GitHub issue may need a status
# comment / close action rather than a new agent). Override with
# --force-respawn if the prior fix was genuinely incomplete.
#
# Creates (${TYPE} defaults to bug):
#   - branch:  <type>_<NNN>_<slug>  (off origin/main)
#   - worktree: ../<type>_<NNN>_<slug>/
#   - .claude/ seeded with hooks + permissions
#   - claude-hydra-messages/inbox/ seeded with a briefing (addressed to $COORDINATOR)
#   - tmux session: <type>_<NNN>_<slug>   (detached, claude running)
#
# After spawn, attach with:
#   tmux attach -t <type>_<NNN>_<slug>
# or just watch the central coordinator inbox:
#   ls ~/.cache/claude-attention/

set -euo pipefail

if [ -z "${COORDINATOR:-}" ]; then
    echo "error: \$COORDINATOR must be set (name the coordinator worktree, e.g. COORDINATOR=staging-gce)" >&2
    echo "       See the file header for context — coordinator identity varies per machine and per epoch." >&2
    exit 2
fi

if [ $# -lt 2 ]; then
    echo "usage: COORDINATOR=<coord-worktree> [TYPE=bug|feature|task|release] [PARENT=<n>] [SPAWN_MODEL=<m>] \\" >&2
    echo "       $0 <issue-number> <slug> [<title>] [--force-respawn]" >&2
    exit 2
fi

NUM="$1"
SLUG="$2"
TITLE="${3:-(issue title not given — fetch with: gh issue view $NUM)}"

# Issue type drives the branch prefix so the branch name mirrors the issue tree.
TYPE="${TYPE:-bug}"
case "$TYPE" in
    bug|feature|task|release) ;;
    *)
        echo "error: TYPE must be one of bug|feature|task|release (got '$TYPE')" >&2
        exit 2
        ;;
esac

# Parent issue number (the issue this one is a child of). Recorded in the
# briefing; every non-release issue should declare one. Empty for release roots.
PARENT="${PARENT:-}"

# Build the parent paragraph for the briefing as a PLAIN variable, never as an
# inline ${PARENT:+...} brace-expansion inside the heredoc. Rationale (this bit
# them once, #557 review C1): an apostrophe inside a ${var:+...} alternate value
# in a heredoc breaks bash's brace-scan at RUNTIME ("bad substitution: no
# closing }"), which `bash -n` cannot catch — it aborted every spawn. Assembling
# the text here, apostrophe-free and with no nested brace-expansion, keeps the
# heredoc a simple variable interpolation.
if [ -n "$PARENT" ]; then
    PARENT_PARAGRAPH="**Parent issue:** #${PARENT} — your coordinator (${COORDINATOR}) manages this
agent as part of the #${PARENT} subtree. Any issue you file must itself declare a
parent (see the mandatory-parent rule in claude/agent-hierarchy.md)."
else
    PARENT_PARAGRAPH="**Parent issue:** (none given) — if this is not a release root, ask your
coordinator (${COORDINATOR}) which issue this is a child of. Every non-release
issue must declare a parent (claude/agent-hierarchy.md)."
fi

ROOT="${CLAUDE_PROJECT_DIR:-$(git rev-parse --show-toplevel 2>/dev/null || pwd)}"
WORKTREES_DIR="$(cd "$ROOT/.." && pwd)"
BRANCH="${TYPE}_${NUM}_${SLUG}"
WT="$WORKTREES_DIR/$BRANCH"

if [ -e "$WT" ]; then
    echo "error: $WT already exists" >&2
    exit 1
fi

# Don't re-spawn an agent for an issue we've already closed. Closed worktrees
# leave their plan-doc behind in worktrees/closed/ as `<type>_<NNN>_*-plan.md`
# (any of bug|feature|task|release); if any match this issue number, surface
# them and bail. Coordinator should review the archived plan and the GitHub
# issue state before deciding to override with --force.
CLOSED_DIR="$WORKTREES_DIR/closed"
if [ -d "$CLOSED_DIR" ]; then
    # Match any type prefix for this issue number, not just bug_.
    PRIOR=$(ls "$CLOSED_DIR" 2>/dev/null | grep -E "^(bug|feature|task|release)_${NUM}_" || true)
    if [ -n "$PRIOR" ]; then
        echo "error: issue #${NUM} already has an archived plan-doc in worktrees/closed/:" >&2
        echo "$PRIOR" | sed 's/^/  /' >&2
        echo "" >&2
        echo "If this issue is still open on GitHub, the prior agent likely landed a partial fix" >&2
        echo "and the issue needs a status comment or close action, not a new agent." >&2
        echo "Check with: gh issue view ${NUM}" >&2
        echo "Read the archive: less '$CLOSED_DIR'/${PRIOR%%$'\n'*}" >&2
        echo "" >&2
        echo "To override and re-spawn anyway, pass --force-respawn as the 4th argument." >&2
        if [ "${4:-}" != "--force-respawn" ]; then
            exit 1
        fi
        echo "(--force-respawn given; proceeding)" >&2
    fi
fi

echo "Creating worktree $WT (branch $BRANCH off origin/main)..."
git worktree add -b "$BRANCH" "$WT" origin/main

# From here on, any abort must undo BOTH the worktree and the branch that
# `git worktree add -b` created — otherwise a retry fails with "$WT already
# exists" (worktree left) or "branch already exists" (branch left). A single
# ERR/EXIT trap cleans up until we clear it on success (just before tmux spawn).
cleanup_partial_spawn() {
    local st=$?
    [ "$st" -eq 0 ] && return 0
    echo "spawn aborted (exit $st); cleaning up partial worktree + branch..." >&2
    git worktree remove --force "$WT" 2>/dev/null || true
    git branch -D "$BRANCH" 2>/dev/null || true
}
trap cleanup_partial_spawn EXIT

echo "Seeding .claude/settings.json (hooks + permissions)..."
mkdir -p "$WT/.claude"
# Hooks themselves live in bin/claude-hooks/ in the tracked tree — the
# worktree checkout already includes them, so we just need to drop in
# the settings.json that references them.
#
# Guard: verify the referenced hook scripts actually exist and are executable
# in the new worktree *before* launching Claude. A missing/non-exec hook fails
# silently at the first Stop/Notification/UserPromptSubmit event — the stop
# hook's absence in particular is implicated in an idle agent never
# self-checking its inbox (a multi-hour approval-unread stall). Catch a
# provisioning regression here, at spawn, instead of at first turn-end.
# (Cleanup on abort is handled by the EXIT trap installed above.)
#
# CORE_HOOKS have long been on origin/main, so their absence is a real
# regression → abort. proposals-hook.sh and autonomy-hook.sh are newer (added
# on the #557 branch); until that branch lands on main, a worktree cut from
# origin/main won't have them, so treat them as warn-not-abort to avoid
# regressing every spawn in that window. Once they are on main, they will be
# present and the warnings stop firing.
for hook in inbox-hook.sh notification-hook.sh stop-hook.sh; do
    HOOK_PATH="$WT/bin/claude-hooks/$hook"
    if [ ! -x "$HOOK_PATH" ]; then
        echo "error: hook script missing or not executable: $HOOK_PATH" >&2
        echo "       The spawned agent's Stop/inbox hooks would fail silently." >&2
        echo "       Ensure bin/claude-hooks/*.sh are committed with +x and present" >&2
        echo "       in origin/main (the worktree base). Aborting spawn." >&2
        exit 1
    fi
done
for hook in proposals-hook.sh autonomy-hook.sh decisions-hook.sh; do
    if [ ! -x "$WT/bin/claude-hooks/$hook" ]; then
        echo "warning: bin/claude-hooks/$hook not present in this worktree." >&2
        echo "         It is expected until #557 lands on origin/main. The corresponding" >&2
        echo "         banner will be inactive for this agent until the file is on main." >&2
    fi
done
cp "$WT/bin/claude-hooks/template-settings.json" "$WT/.claude/settings.json"

# Sandbox permission bypass (see claude/sandbox-permissions.md). The checked-in
# template deliberately does NOT carry defaultMode: bypassPermissions — that would
# leak bypass to every machine that clones the repo, including non-sandbox laptops.
# Instead, inject it here ONLY when this machine declares itself a sandbox via the
# ~/.hydra-sandbox marker. On a non-sandbox machine the marker is absent, the
# injection is skipped, and the spawned agent prompts normally.
if [ -f "$HOME/.hydra-sandbox" ]; then
    echo "Sandbox marker present — enabling permission bypass for this agent..."
    python3 - "$WT/.claude/settings.json" <<'PYEOF'
import json, sys
p = sys.argv[1]
with open(p) as f:
    cfg = json.load(f)
cfg.setdefault("permissions", {})["defaultMode"] = "bypassPermissions"
with open(p, "w") as f:
    json.dump(cfg, f, indent=2)
    f.write("\n")
PYEOF
fi

echo "Seeding claude-hydra-messages/inbox/ with briefing..."
mkdir -p "$WT/claude-hydra-messages/inbox/archive"
mkdir -p "$WT/claude-hydra-messages/outbox/archive"
# Issue-proposal queue (see claude/agent-hierarchy.md). The proposals hook
# surfaces a standing banner while pending/ is non-empty; pre-creating the dirs
# means the hook is a silent no-op until a proposal actually lands here.
#   pending/    — proposals awaiting the USER's disposition. This is the queue
#                 the banner watches. It is authoritative ONLY in the worktree
#                 that actually holds the disposition point (the top coordinator).
#   forwarded/  — a leaf/non-top agent moves its copy HERE the instant it sends
#                 the proposal up the chain, so its own pending/ never nags for a
#                 proposal it does not own the disposition of (#557 review M2).
#   approved/ / declined/ — post-disposition, verdict recorded.
mkdir -p "$WT/claude-hydra-messages/proposals/pending"
mkdir -p "$WT/claude-hydra-messages/proposals/forwarded"
mkdir -p "$WT/claude-hydra-messages/proposals/approved"
mkdir -p "$WT/claude-hydra-messages/proposals/declined"
# Design-decision queue (decisions-hook.sh; agent-hierarchy.md § Design decisions):
# pending/ (awaiting answer; banner watches), forwarded/ (sent up-chain),
# answered/ (answer recorded + routed down to the asker).
mkdir -p "$WT/claude-hydra-messages/decisions/pending"
mkdir -p "$WT/claude-hydra-messages/decisions/forwarded"
mkdir -p "$WT/claude-hydra-messages/decisions/answered"
TS="$(date -u +%Y-%m-%dT%H-%M-%SZ)"
cat > "$WT/claude-hydra-messages/inbox/${TS}-coordinator-assignment.md" <<EOF
# Assignment: GitHub issue #${NUM}

**From:** coordinator (${COORDINATOR})
**Date:** $(date -u +%Y-%m-%d)
**Issue:** https://github.com/CategoricalData/hydra/issues/${NUM}
**Title:** ${TITLE}
**Type:** ${TYPE}
${PARENT_PARAGRAPH}

## What

Read the GitHub issue (\`gh issue view ${NUM}\`) for the full problem statement and
context. This branch is dedicated to investigating and addressing it.

## How to work

1. Run the CLAUDE.md startup procedure (you should be doing that anyway).
2. Read the issue body via \`gh issue view ${NUM}\`. Investigate the symptom
   and propose a root cause.
3. Write your plan to \`${BRANCH}-plan.md\` at the worktree root.
4. Iterate: commit small, push often, draft a PR when ready for review.

### Scoped reading (keep your context lean)

Your startup reading is scoped to your role (see
claude/agent-hierarchy.md § Context minimization). As an issue agent you need:
the CLAUDE.md startup checklist, this briefing, the new-issue proposal
lifecycle + dependency test in claude/agent-hierarchy.md, and the \`docs/\`
recipes relevant to YOUR issue. You do NOT need the staging promotion-loop
internals (branch-flow.md) or the coordinator lifecycle
(coordinator-workflow.md) unless/until your issue takes on children. Read what
your job needs, and no more.

## Coordination

- The inbox hook (\`bin/claude-hooks/inbox-hook.sh\`, wired via
  \`.claude/settings.json\`) auto-surfaces new messages in this directory on
  each prompt. You don't need to remember to poll.
- The coordinator monitors \`~/.cache/claude-attention/\` for blocked
  sessions. If you hit a permission prompt and pause, the coordinator
  sees a marker file with your worktree name and can act.

### IMPORTANT: never call AskUserQuestion. Use the coordinator inbox instead.

The human is supervising N parallel sub-sessions and cannot watch each pane.
**Your settings.json denies the \`AskUserQuestion\` tool by default** — calling
it will fail. This is intentional: every multiple-choice prompt blocks your
turn while the human has to cycle through every pane to answer, which doesn't
scale across sessions.

The same principle applies to any other form of pause-and-wait-for-the-user:
don't end your turn with "Should I do X or Y?" expecting the human to answer.
The human will not see it in time. Instead, route the question through the
coordinator:

1. Write a message to the coordinator's inbox at
   \`../${COORDINATOR}/claude-hydra-messages/inbox/\`
   (filename format per claude/cross-worktree-messages.md). Include enough
   context for the coordinator to answer on the user's behalf.
2. Then either (a) **make a best-guess choice, document it in the plan-doc,
   and continue working** while waiting for the coordinator's reply, or
   (b) if the question is truly blocking, end your turn with a brief
   "blocked — waiting on coordinator's answer" status; your next inbox
   pickup will include the reply.
3. **Default to (a).** Making a judgment call and writing it in the plan-doc
   is almost always better than blocking. The coordinator can redirect you
   later if the choice was wrong; if it was right, you've saved a round-trip.
4. The coordinator may answer directly (your inbox surfaces it on the next
   prompt) or escalate to the human and relay the answer back.

Permission prompts (tool-use approvals) are different — those will be
handled at the harness level by the human; you do not need to message
the coordinator about them.

## Scope guard

This branch is for issue #${NUM} **only**. Out-of-scope discoveries should
go in the plan-doc's "Findings" section for future filing, not this PR.

Good luck. Ping back when you've got a plan or hit a blocker.
EOF

# Worktree is fully provisioned; from here a failure should NOT tear it down
# (the branch/worktree are the valuable artifact even if tmux/claude hiccups).
# Clear the cleanup trap so a normal or error EXIT leaves the worktree intact.
trap - EXIT

echo "Starting tmux session '${BRANCH}'..."
tmux new-session -d -s "${BRANCH}" -c "$WT"

# Pin the window/pane title to the session name so the iTerm2/terminal tab
# always shows the worktree identity. Without this, Claude's TUI emits OSC
# escape sequences that retitle the window with the current task, making it
# hard to find a specific session among many tabs.
tmux set-window-option -t "${BRANCH}" automatic-rename off
tmux set-window-option -t "${BRANCH}" allow-rename off
tmux rename-window   -t "${BRANCH}" "${BRANCH}"
# Also set the pane title via OSC; tmux relays this to terminals that
# respect pane-titles (iTerm2 with set -g set-titles on).
tmux select-pane     -t "${BRANCH}" -T "${BRANCH}"

# Default to opusplan for bug_/task_ agents — they spend most of their wall
# time on mechanical sync/regen/verify, which Sonnet handles fine. The
# per-class model policy (release/high-stakes → fable, staging → opus, simple
# agent → opusplan) lives in claude/agent-hierarchy.md; SPAWN_MODEL is the
# mechanism the spawning parent sets accordingly.
SPAWN_MODEL="${SPAWN_MODEL:-opusplan}"
tmux send-keys -t "${BRANCH}" "claude-remote -b -m ${SPAWN_MODEL}" Enter

# Give claude time to spin up its TUI before sending the trigger prompt.
# 8s is comfortable headroom for Claude Code v2.1's startup. We then send
# the prompt, pause, and send a follow-up Enter — Claude Code occasionally
# leaves the prompt sitting in the input box (the initial Enter arrives
# while the TUI is still settling and gets eaten), forcing the human to
# hit Enter manually to actually submit it. The second Enter is insurance:
# a no-op if the prompt was already submitted, the missing submit otherwise.
sleep 8
tmux send-keys -t "${BRANCH}" "Please complete the CLAUDE.md startup procedure and address any pending inbox messages." Enter
sleep 3
tmux send-keys -t "${BRANCH}" Enter

echo ""
echo "============================================================"
echo "Spawned ${BRANCH}:"
echo "  worktree: $WT"
echo "  branch:   $BRANCH"
echo "  tmux:     tmux attach -t ${BRANCH}"
echo ""
echo "Watch attention markers from coordinator:"
echo "  ls -la ~/.cache/claude-attention/"
echo "============================================================"
