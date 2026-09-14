#!/usr/bin/env bash
# UserPromptSubmit hook: surface new fleet-coordination entries automatically.
#
# The `coordination` branch of the wiki repo is the fleet's cross-machine
# channel (claims, red-CI ownership, stand-down orders, release handoffs —
# see claude/fleet-coordination.md). It is an INBOUND channel: another machine
# can post a claim or a question at any time, unprompted. Polling it therefore
# cannot be keyed to actions this agent initiates.
#
# That was the actual failure mode this hook exists to prevent. The documented
# rule (fleet-coordination.md) listed only self-initiated triggers — fetch
# before *your* push, before *your* claim, inside *your* watcher loop — so an
# unprompted message from another machine had no trigger at all. On 2026-09-14
# both staging agents lapsed simultaneously: alpha posted a #740 stand-down
# that went unread on marvin7, and alpha itself acknowledged having stopped
# polling. A stand-down that arrives late is worse than useless — it is read
# after the duplicated work is already done.
#
# State: .coordination-seen (gitignored, in the worktree root) records the last
# surfaced commit SHA. Only entries newer than that are shown.
#
# This hook NEVER blocks a turn: the fetch is time-bounded and every failure
# path exits 0 silently. A missed poll is recoverable (the next turn retries);
# a hung prompt is not.

set -euo pipefail

ROOT="${CLAUDE_PROJECT_DIR:-$(git rev-parse --show-toplevel 2>/dev/null || pwd)}"
WIKI="$(cd "$ROOT/../../wiki" 2>/dev/null && pwd || true)"
SEEN="$ROOT/.coordination-seen"

# Silently exit unless the wiki clone is present with a coordination branch.
[ -n "$WIKI" ] && [ -d "$WIKI/.git" ] || exit 0

# Bounded fetch. Without a timeout a network stall would hang the user's
# prompt; 15s is far above a normal fetch and far below anything noticeable
# as a hang. `timeout` is GNU/coreutils and absent on stock macOS, so fall
# back to a background-kill guard. Any failure → silent exit 0.
fetch_bounded() {
    if command -v timeout >/dev/null 2>&1; then
        timeout 15 git -C "$WIKI" fetch origin coordination --quiet 2>/dev/null
    elif command -v gtimeout >/dev/null 2>&1; then
        gtimeout 15 git -C "$WIKI" fetch origin coordination --quiet 2>/dev/null
    else
        git -C "$WIKI" fetch origin coordination --quiet 2>/dev/null &
        local pid=$!
        local waited=0
        while kill -0 "$pid" 2>/dev/null && [ "$waited" -lt 15 ]; do
            sleep 1
            waited=$((waited + 1))
        done
        if kill -0 "$pid" 2>/dev/null; then
            kill -TERM "$pid" 2>/dev/null || true
            return 1
        fi
        wait "$pid" 2>/dev/null
    fi
}
fetch_bounded || exit 0

REMOTE_SHA="$(git -C "$WIKI" rev-parse origin/coordination 2>/dev/null || true)"
[ -n "$REMOTE_SHA" ] || exit 0

# First run in a worktree: record the current head and stay silent rather than
# dumping the entire backlog into the turn.
if [ ! -f "$SEEN" ]; then
    printf '%s\n' "$REMOTE_SHA" > "$SEEN"
    exit 0
fi

LAST_SHA="$(cat "$SEEN" 2>/dev/null || true)"
[ "$LAST_SHA" = "$REMOTE_SHA" ] && exit 0

# The recorded SHA may no longer exist: the coordination branch is periodically
# deleted and recreated (fleet-coordination.md), which rewrites history. Treat
# an unknown baseline as a fresh start.
if ! git -C "$WIKI" cat-file -e "${LAST_SHA}^{commit}" 2>/dev/null; then
    printf '%s\n' "$REMOTE_SHA" > "$SEEN"
    exit 0
fi

COMMITS="$(git -C "$WIKI" log --oneline "${LAST_SHA}..origin/coordination" 2>/dev/null || true)"
[ -n "$COMMITS" ] || { printf '%s\n' "$REMOTE_SHA" > "$SEEN"; exit 0; }

# Record BEFORE emitting: if the consumer truncates our output, a trailing
# write would be lost to SIGPIPE and every later turn would re-surface the
# same entries. (Same reasoning as inbox-hook.sh.)
printf '%s\n' "$REMOTE_SHA" > "$SEEN"

echo "## New fleet-coordination entries (auto-surfaced by coordination hook)"
echo ""
echo "New commits on the wiki \`coordination\` branch since this worktree last looked:"
echo ""
echo '```'
echo "$COMMITS"
echo '```'
echo ""
echo "These may include claims, stand-down orders, or questions addressed to this"
echo "machine. Read the new LOG.md entries and respond if any concern this session."
echo ""
echo "---"
echo ""
echo "### Newest LOG.md content"
echo ""
git -C "$WIKI" show origin/coordination:LOG.md 2>/dev/null | tail -c 3000 || true
echo ""
