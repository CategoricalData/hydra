#!/usr/bin/env bash
# Notification hook: write a marker when Claude needs attention.
#
# Claude Code fires `Notification` when waiting on user input — permission
# prompts, idle "ready for next instruction," etc. This hook records the
# event so a central watcher can see which sub-sessions are blocked without
# attaching to each tmux pane.
#
# Marker dir: ~/.cache/claude-attention/<worktree-basename>.txt
# Contents: timestamp + (truncated) notification message piped on stdin.
#
# Coordinator pattern:
#   ls -la ~/.cache/claude-attention/         # which sessions are waiting
#   tail ~/.cache/claude-attention/foo.txt   # what's the wait about
#
# A companion Stop hook clears the marker when the session resumes work
# (see stop-hook.sh).

set -euo pipefail

ROOT="${CLAUDE_PROJECT_DIR:-$(git rev-parse --show-toplevel 2>/dev/null || pwd)}"
NAME="$(basename "$ROOT")"
DIR="$HOME/.cache/claude-attention"
mkdir -p "$DIR"

# Notification hook receives a JSON payload on stdin (per Claude Code's
# hook IO contract). Capture up to 2KB.
MSG="$(head -c 2048 || true)"

# Pull out notification_type so the marker leads with it. jq if available;
# fall back to a grep-based extract that's tolerant of pretty-printed JSON.
TYPE=""
if command -v jq >/dev/null 2>&1 && [ -n "$MSG" ]; then
    TYPE="$(printf '%s' "$MSG" | jq -r '.notification_type // ""' 2>/dev/null || true)"
fi
if [ -z "$TYPE" ] && [ -n "$MSG" ]; then
    TYPE="$(printf '%s' "$MSG" | grep -oE '"notification_type"[[:space:]]*:[[:space:]]*"[^"]+"' | head -1 | sed -E 's/.*"([^"]+)"$/\1/' || true)"
fi
[ -z "$TYPE" ] && TYPE="unknown"

# Write marker with type up front so a quick `ls` + `head` shows urgency.
{
    echo "type: $TYPE"
    echo "ts:   $(date -u +%Y-%m-%dT%H:%M:%SZ)"
    echo "wt:   $ROOT"
    echo ""
    [ -n "$MSG" ] && echo "$MSG"
} > "$DIR/$NAME.txt"
