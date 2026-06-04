#!/usr/bin/env bash
# Stop hook: clear the attention marker when Claude finishes a turn.
#
# Pairs with notification-hook.sh. When Claude wraps up a turn (Stop event),
# we remove the marker so the central watcher reflects the current state.
# If Claude immediately needs another prompt, Notification will re-create
# the marker on the next blocking event.

set -euo pipefail

ROOT="${CLAUDE_PROJECT_DIR:-$(git rev-parse --show-toplevel 2>/dev/null || pwd)}"
NAME="$(basename "$ROOT")"
rm -f "$HOME/.cache/claude-attention/$NAME.txt"
