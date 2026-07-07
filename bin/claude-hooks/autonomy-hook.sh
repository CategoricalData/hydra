#!/usr/bin/env bash
# UserPromptSubmit hook: surface the machine's AUTONOMY DIAL to the agent on
# every turn, so a change to the dial takes effect fleet-wide without
# re-prompting N sessions.
#
# See claude/agent-hierarchy.md § "The autonomy dial". The dial is a single
# per-machine file the user edits:
#   <worktrees-dir>/autonomy.md        (i.e. ../autonomy.md from a worktree root)
# It lives OUTSIDE any git checkout (the worktrees parent dir), so it is shared
# by every worktree on the machine and never committed. Recognized fields, one
# per line, anywhere in the file (first match wins; everything else is free-form
# notes for humans):
#   level: low | medium | high
#   available-until: 2026-07-07T06:00Z      (optional; UTC, ISO-8601-ish)
#   auto-file-rules: <text>                 (optional; narrow pre-declared rules)
#
# Defaults and noise budget: a MISSING file means level=medium (the documented
# default) and this hook stays SILENT. It also stays silent when the file says
# medium with no availability window and no auto-file rules — the banner only
# appears when there is something non-default to know. Like proposals-hook.sh,
# it re-reads the file every turn (no .seen state), which is what makes an edit
# to autonomy.md propagate to the whole fleet on each session's next turn.
#
# Portability: no GNU-only flags; date parsing tries GNU `date -d` then BSD
# `date -j -f` (two accepted formats), and degrades to showing the raw string
# (no expiry check) if neither parses — the macOS laptop must not go mute.

set -euo pipefail

ROOT="${CLAUDE_PROJECT_DIR:-$(git rev-parse --show-toplevel 2>/dev/null || pwd)}"
DIAL_FILE="$(dirname "$ROOT")/autonomy.md"

# No dial file → default (medium) → silent.
[ -f "$DIAL_FILE" ] || exit 0

field() {
    # First "key: value" line for the given key, value only; empty if absent.
    grep -m1 -iE "^${1}:" "$DIAL_FILE" 2>/dev/null \
        | sed -E "s/^[^:]*:[[:space:]]*//" || true
}

LEVEL=$(field 'level' | tr '[:upper:]' '[:lower:]' | tr -d '[:space:]')
UNTIL=$(field 'available-until')
RULES=$(field 'auto-file-rules')

# Normalize "no rules" spellings to empty.
case "$(printf '%s' "$RULES" | tr '[:upper:]' '[:lower:]')" in
    none|n/a|-) RULES="" ;;
esac

INVALID=""
case "$LEVEL" in
    low|medium|high) ;;
    "") LEVEL="medium" ;;                # file exists but no level line
    *)  INVALID="$LEVEL"; LEVEL="medium" ;;
esac

# Nothing non-default to report → silent.
if [ "$LEVEL" = "medium" ] && [ -z "$UNTIL" ] && [ -z "$RULES" ] && [ -z "$INVALID" ]; then
    exit 0
fi

# Best-effort expiry check on available-until (GNU date, then BSD date with the
# two documented formats). Unparseable → no check, show the raw string.
EXPIRED=""
if [ -n "$UNTIL" ]; then
    until_epoch=$(date -u -d "$UNTIL" +%s 2>/dev/null \
        || date -j -u -f '%Y-%m-%dT%H:%MZ' "$UNTIL" +%s 2>/dev/null \
        || date -j -u -f '%Y-%m-%dT%H:%M:%SZ' "$UNTIL" +%s 2>/dev/null \
        || true)
    if [ -n "$until_epoch" ] && [ "$until_epoch" -lt "$(date +%s)" ]; then
        EXPIRED="yes"
    fi
fi

echo "## Autonomy dial: $(printf '%s' "$LEVEL" | tr '[:lower:]' '[:upper:]') (machine-wide; from \`$DIAL_FILE\`)"
echo ""
HAD_BULLETS=""
if [ -n "$INVALID" ]; then
    echo "- ⚠ unrecognized \`level: $INVALID\` — treating as **medium**; fix the file (low|medium|high)."
    HAD_BULLETS="yes"
fi
if [ -n "$UNTIL" ]; then
    if [ -n "$EXPIRED" ]; then
        echo "- ⚠ \`available-until: $UNTIL\` has **passed** — the user may be back; confirm the"
        echo "  dial at next contact and treat ambiguous grants conservatively until then."
    else
        echo "- User availability: afk until $UNTIL — prefer queue-and-proceed over blocking."
    fi
    HAD_BULLETS="yes"
fi
if [ -n "$RULES" ]; then
    echo "- Pre-declared auto-file rules: $RULES"
    HAD_BULLETS="yes"
fi
if [ -n "$HAD_BULLETS" ]; then
    echo ""
fi
echo "What each level permits is defined in claude/agent-hierarchy.md § The"
echo "autonomy dial. Invariants that never ride the dial: issue filing is never"
echo "automatic beyond narrow pre-declared rules (draft-and-queue at high), and"
echo "the pre-push gates (WIP scan, [CI]-by-name, fetch-merge) always hold."
