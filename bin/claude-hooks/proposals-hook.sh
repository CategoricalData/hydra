#!/usr/bin/env bash
# UserPromptSubmit hook: surface a STANDING banner for undispositioned issue
# proposals, so none is ever lost in the noise of an agent's output.
#
# See claude/agent-hierarchy.md § "New-issue proposals" and § "No proposal is
# ever lost in the noise". A proposal is a file the discovering agent drafts into
#   claude-hydra-messages/proposals/pending/<id>.md
# It leaves pending/ only when the USER dispositions it (moved to approved/ or
# declined/). Until then it must keep reminding the user it needs attention.
#
# UNLIKE inbox-hook.sh, this hook does NOT track a .seen set: the whole point is
# to re-surface the pending queue on EVERY turn until it drains. The banner is
# tied to queue depth, not to "have I shown this once". It shows the count and
# the age of the oldest pending proposal so a stale proposal nags louder, and it
# escalates the wording past a threshold (the age/urgency escalation ladder — the
# out-of-band attention-marker is written by the coordinator, not here).
#
# WHOSE pending/ nags: only the worktree that holds the disposition point (the
# top coordinator) has proposals in pending/. A leaf/non-top agent moves its copy
# to proposals/forwarded/ the instant it sends the proposal UP the chain (see
# spawn-issue-worktree.sh and agent-hierarchy.md), so its own pending/ stays
# empty and this hook stays silent there. That resolves the M2 contradiction: a
# leaf never nags for a proposal it does not own the disposition of.
#
# Portability: avoids GNU-only `find -printf` and `stat -c` so the hook also runs
# on the macOS/BSD laptop in the fleet (where those flags differ and the hook
# would otherwise go silently mute).

set -euo pipefail

ROOT="${CLAUDE_PROJECT_DIR:-$(git rev-parse --show-toplevel 2>/dev/null || pwd)}"
PENDING="$ROOT/claude-hydra-messages/proposals/pending"

# Silently exit if this worktree has no proposal queue.
[ -d "$PENDING" ] || exit 0

# Pending proposal files at the top level (portable: -maxdepth is supported on
# both GNU and BSD find; strip the dir with basename rather than GNU -printf).
PROPOSALS=()
while IFS= read -r p; do
    [ -n "$p" ] && PROPOSALS+=("$(basename "$p")")
done < <(find "$PENDING" -maxdepth 1 -name '*.md' -type f | sort)

# Empty queue → silent. (This is what makes the banner "clear when drained".)
[ "${#PROPOSALS[@]}" -eq 0 ] && exit 0

# Age of the oldest pending proposal, in whole days, for the nag wording.
# mtime lookup is portable: GNU `stat -c %Y` vs BSD/macOS `stat -f %m`. Try GNU
# first, fall back to BSD; if neither works, treat as "now" (age 0) rather than
# breaking the banner.
file_mtime() {
    stat -c %Y "$1" 2>/dev/null || stat -f %m "$1" 2>/dev/null || echo "$2"
}
now=$(date +%s)
oldest_epoch=$now
for f in "${PROPOSALS[@]}"; do
    m=$(file_mtime "$PENDING/$f" "$now")
    [ "$m" -lt "$oldest_epoch" ] && oldest_epoch=$m
done
age_days=$(( (now - oldest_epoch) / 86400 ))

# Escalate the wording past 1 day so a stale queue reads as more urgent. The
# marker-writing escalation (to ~/.cache/claude-attention/) is a coordinator
# duty documented in agent-hierarchy.md; this hook owns the standing banner.
if [ "$age_days" -ge 1 ]; then
    echo "## ⚠️  ${#PROPOSALS[@]} issue proposal(s) awaiting the user's approval — OLDEST IS ${age_days}d OLD"
    echo ""
    echo "These have sat undispositioned. Surface them to the user at the next"
    echo "check-in, and consider escalating via an attention-marker (age threshold"
    echo "reached) — see claude/agent-hierarchy.md § age/urgency escalation."
else
    echo "## ⚠️  ${#PROPOSALS[@]} issue proposal(s) awaiting the user's approval"
    echo ""
    echo "Surface these to the user at the next check-in (periodic digest)."
fi
echo ""
echo "Pending in \`claude-hydra-messages/proposals/pending/\` (must NOT be filed"
echo "to GitHub without explicit user approval — CLAUDE.md hard rule 5):"
echo ""
for f in "${PROPOSALS[@]}"; do
    # First markdown heading of each proposal, as a one-line summary.
    title=$(grep -m1 '^#' "$PENDING/$f" 2>/dev/null | sed 's/^#\+[[:space:]]*//' || true)
    echo "- \`$f\`${title:+ — $title}"
done
echo ""
echo "On the user's verdict, move each file to \`proposals/approved/\` or"
echo "\`proposals/declined/\` (recording the verdict) so this banner clears."
