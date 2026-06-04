#!/usr/bin/env bash
# UserPromptSubmit hook: surface new cross-worktree messages automatically.
#
# Consumes the file-based protocol described in
# claude/cross-worktree-messages.md, but does the polling at hook-time
# instead of relying on Claude to remember. Output gets injected into the
# next Claude turn as user-prompt-submit-hook content.
#
# State: .seen file in the inbox tracks which messages have already been
# surfaced (by filename). Survives across sessions. If you ever want to
# re-surface everything, `rm claude-hydra-messages/inbox/.seen`.
#
# Idempotency: never re-surfaces the same filename twice. If a sender
# re-copies (per the protocol's crash-recovery clause), the duplicate is
# silently suppressed.

set -euo pipefail

ROOT="${CLAUDE_PROJECT_DIR:-$(git rev-parse --show-toplevel 2>/dev/null || pwd)}"
INBOX="$ROOT/claude-hydra-messages/inbox"
SEEN="$INBOX/.seen"

# Silently exit if no inbox directory in this worktree.
[ -d "$INBOX" ] || exit 0

touch "$SEEN"

# Current message files at the inbox top level (skip archive/, skip hidden).
mapfile -t CURRENT < <(find "$INBOX" -maxdepth 1 -name '*.md' -type f -printf '%f\n' | sort)

# Build the "seen" set.
declare -A SEEN_SET=()
while IFS= read -r f; do
    [ -n "$f" ] && SEEN_SET["$f"]=1
done < "$SEEN"

# Collect new messages (current minus seen).
NEW=()
for f in "${CURRENT[@]}"; do
    [ -z "${SEEN_SET[$f]:-}" ] && NEW+=("$f")
done

# Nothing new → silent.
[ "${#NEW[@]}" -eq 0 ] && exit 0

# Surface them.
echo "## New cross-worktree messages (auto-surfaced by inbox hook)"
echo ""
echo "${#NEW[@]} unread message(s) in \`claude-hydra-messages/inbox/\`."
echo "After addressing each, \`mv\` the file to \`inbox/archive/\` per the protocol."
echo ""
for f in "${NEW[@]}"; do
    echo "---"
    echo ""
    echo "### Message: \`$f\`"
    echo ""
    cat "$INBOX/$f"
    echo ""
    echo "$f" >> "$SEEN"
done
