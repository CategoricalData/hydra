#!/usr/bin/env bash
# Spawn a worktree + tmux session + Claude for a specific GitHub issue.
#
# Usage:
#   bin/spawn-issue-worktree.sh <issue-number> <slug> [<issue-title>]
# Example:
#   bin/spawn-issue-worktree.sh 425 clojure_json_decode "Clojure host's JSON decoder rejects kernel JSON"
#
# Creates:
#   - branch:  bug_<NNN>_<slug>  (off origin/main)
#   - worktree: ../bug_<NNN>_<slug>/
#   - .claude/ seeded with hooks + permissions
#   - claude-hydra-messages/inbox/ seeded with a briefing
#   - tmux session: bug_<NNN>_<slug>   (detached, claude running)
#
# After spawn, attach with:
#   tmux attach -t bug_<NNN>_<slug>
# or just watch the central coordinator inbox:
#   ls ~/.cache/claude-attention/

set -euo pipefail

if [ $# -lt 2 ]; then
    echo "usage: $0 <issue-number> <slug> [<title>]" >&2
    exit 2
fi

NUM="$1"
SLUG="$2"
TITLE="${3:-(issue title not given — fetch with: gh issue view $NUM)}"

ROOT="${CLAUDE_PROJECT_DIR:-$(git rev-parse --show-toplevel 2>/dev/null || pwd)}"
WORKTREES_DIR="$(cd "$ROOT/.." && pwd)"
BRANCH="bug_${NUM}_${SLUG}"
WT="$WORKTREES_DIR/$BRANCH"

if [ -e "$WT" ]; then
    echo "error: $WT already exists" >&2
    exit 1
fi

echo "Creating worktree $WT (branch $BRANCH off HEAD)..."
# Branch off the coordinator's current HEAD so the new worktree inherits
# bin/claude-hooks/ and any other tooling needed for the spawn to succeed.
# (Branching off origin/main would miss the parallel-Claude framework if it
# hasn't been pushed yet.)
git worktree add -b "$BRANCH" "$WT" HEAD

echo "Seeding .claude/settings.json (hooks + permissions)..."
mkdir -p "$WT/.claude"
# Hooks themselves live in bin/claude-hooks/ in the tracked tree — the
# worktree checkout already includes them, so we just need to drop in
# the settings.json that references them.
cp "$WT/bin/claude-hooks/template-settings.json" "$WT/.claude/settings.json"

echo "Seeding claude-hydra-messages/inbox/ with briefing..."
mkdir -p "$WT/claude-hydra-messages/inbox/archive"
mkdir -p "$WT/claude-hydra-messages/outbox/archive"
TS="$(date -u +%Y-%m-%dT%H-%M-%SZ)"
cat > "$WT/claude-hydra-messages/inbox/${TS}-coordinator-assignment.md" <<EOF
# Assignment: GitHub issue #${NUM}

**From:** coordinator (feature_409_bootstrap_everything)
**Date:** $(date -u +%Y-%m-%d)
**Issue:** https://github.com/CategoricalData/hydra/issues/${NUM}
**Title:** ${TITLE}

## What

Read the GitHub issue (\`gh issue view ${NUM}\`) for the full problem statement and
context. The issue was filed from the #409 matrix run; this branch is dedicated
to investigating and fixing it.

## How to work

1. Run the CLAUDE.md startup procedure (you should be doing that anyway).
2. Read the issue body via \`gh issue view ${NUM}\`. Investigate the symptom
   and propose a root cause.
3. Write your plan to \`bug_${NUM}_${SLUG}-plan.md\` at the worktree root.
4. Iterate: commit small, push often, draft a PR when ready for review.

## Coordination

- The inbox hook (\`.claude/inbox-hook.sh\`) auto-surfaces new messages in
  this directory on each prompt. You don't need to remember to poll.
- The coordinator monitors \`~/.cache/claude-attention/\` for blocked
  sessions. If you hit a permission prompt and pause, the coordinator
  sees a marker file with your worktree name and can act.

### IMPORTANT: never ask the user free-form questions interactively

The human is supervising N parallel sub-sessions and cannot watch each pane.
If you would normally pause your turn to ask the user a clarifying question,
**don't**. Instead, route the question through the coordinator:

1. Write a message to the coordinator's inbox at
   \`../feature_409_bootstrap_everything/claude-hydra-messages/inbox/\`
   (filename format per claude/cross-worktree-messages.md). Include enough
   context for the coordinator to answer on the user's behalf.
2. Then either (a) **make a best-guess choice, document it in the plan-doc,
   and continue working** while waiting for the coordinator's reply, or
   (b) if the question is truly blocking, end your turn with a brief
   "blocked — waiting on coordinator's answer" status; your next inbox
   pickup will include the reply.
3. The coordinator may answer directly (your inbox surfaces it on the next
   prompt) or escalate to the human and relay the answer back.

Permission prompts (tool-use approvals) are different — those will be
handled at the harness level by the human; you do not need to message
the coordinator about them.

## Scope guard

This branch is for issue #${NUM} **only**. Out-of-scope discoveries should
go in the plan-doc's "Findings" section for future filing, not this PR.

## When your work is complete

Follow \`../../sub-claude-handoff.md\` — that file is authoritative. Summary:

1. **Verify** the fix end-to-end (every relevant test suite green; cross-host
   implications checked). Do not begin handoff with red tests.
2. **Squash** WIP commits per CLAUDE.md "Commit workflow" + \`.claude/commands/squash.md\`.
   Reset target: \`git merge-base HEAD feature_409_bootstrap_everything\`
   (the fork point — NOT main, NOT the coordinator's current tip).
3. **\`/save\`** to write final state into your \`bug_${NUM}_${SLUG}-plan.md\`.
4. **Tint the tab blue** as the explicit "I am done" signal to the human:
   \`~/projects/github/joshsh/egodev/common/bash/claude-tab-color.sh b\`

Blue specifically means "verified + squashed + saved; ready for the human
to terminate me, review my commits in this tmux session, and ask the
coordinator to finalize." Do not tint blue prematurely. Green is the
default auto-applied idle color and carries no completion semantic — only
explicit blue stops the auto-hooks from overriding the tab.

Good luck. Ping back when you've got a plan or hit a blocker.
EOF

echo "Starting tmux session 'bug_${NUM}_${SLUG}'..."
tmux new-session -d -s "bug_${NUM}_${SLUG}" -c "$WT"

# Pin the window/pane title to the session name so the iTerm2/terminal tab
# always shows the worktree identity. Without this, Claude's TUI emits OSC
# escape sequences that retitle the window with the current task, making it
# hard to find a specific session among many tabs.
tmux set-window-option -t "bug_${NUM}_${SLUG}" automatic-rename off
tmux set-window-option -t "bug_${NUM}_${SLUG}" allow-rename off
tmux rename-window   -t "bug_${NUM}_${SLUG}" "bug_${NUM}_${SLUG}"
# Also set the pane title via OSC; tmux relays this to terminals that
# respect pane-titles (iTerm2 with set -g set-titles on).
tmux select-pane     -t "bug_${NUM}_${SLUG}" -T "bug_${NUM}_${SLUG}"

# --dangerously-skip-permissions: sub-Claudes work autonomously, no
# prompting. Worktree-bounded blast radius (deploy keys, no PAT in the
# sub-session). User reviews resulting PRs before merge.
tmux send-keys -t "bug_${NUM}_${SLUG}" "CLAUDE_LABEL='bug_${NUM}_${SLUG}' claude --remote-control 'bug_${NUM}_${SLUG}' --dangerously-skip-permissions" Enter

# --dangerously-skip-permissions shows a "Bypass Permissions" warning on
# first launch (per machine/project) with options "1. No, exit" /
# "2. Yes, I accept". Default-highlighted option is "1", so if we send
# our trigger prompt without answering, it lands on "1" and Claude exits.
sleep 4
if tmux capture-pane -t "bug_${NUM}_${SLUG}:0" -p | grep -q 'Bypass Permissions'; then
    echo "Accepting Bypass Permissions warning..."
    tmux send-keys -t "bug_${NUM}_${SLUG}" '2' Enter
    sleep 2
fi

# Give claude time to spin up its TUI before sending the trigger prompt;
# 8s is comfortable headroom for Claude Code v2.1's startup.
sleep 8
tmux send-keys -t "bug_${NUM}_${SLUG}" "Please complete the CLAUDE.md startup procedure and address any pending inbox messages." Enter

echo ""
echo "============================================================"
echo "Spawned bug_${NUM}_${SLUG}:"
echo "  worktree: $WT"
echo "  branch:   $BRANCH"
echo "  tmux:     tmux attach -t bug_${NUM}_${SLUG}"
echo ""
echo "Watch attention markers from coordinator:"
echo "  ls -la ~/.cache/claude-attention/"
echo "============================================================"
