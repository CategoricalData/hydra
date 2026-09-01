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
#
# Portability: macOS ships bash 3.2, which has neither `mapfile`/`readarray`
# nor associative arrays (`declare -A`), and BSD `find` has no `-printf` (it
# accepts the flag silently on some builds while emitting nothing). Use a
# plain while-read loop, `basename`, and a newline-delimited string as the
# "seen" set instead, so this runs unchanged on macOS and Linux.
CURRENT=()
while IFS= read -r path; do
    [ -n "$path" ] && CURRENT+=("$(basename "$path")")
done < <(find "$INBOX" -maxdepth 1 -name '*.md' -type f | sort)

# Read the "seen" set as newline-delimited text; membership is a fixed-string,
# whole-line grep, so filenames containing regex metacharacters are safe.
SEEN_TEXT="$(cat "$SEEN" 2>/dev/null || true)"

# Collect new messages (current minus seen).
NEW=()
for f in ${CURRENT[@]+"${CURRENT[@]}"}; do
    printf '%s\n' "$SEEN_TEXT" | grep -qxF -- "$f" || NEW+=("$f")
done

# Nothing new → silent. (Guard the expansion: under `set -u`, bash 3.2 treats
# "${arr[@]}" on an empty array as an unbound variable.)
[ "${#NEW[@]:-0}" -eq 0 ] && exit 0

# Mark them seen BEFORE emitting. If the consumer closes the pipe early (a
# truncated read sends SIGPIPE mid-loop), a write that trailed the output would
# never land and every future invocation would re-surface the same messages.
# Recording first makes the hook idempotent regardless of how its output is
# consumed; the cost of the opposite failure — a message surfaced once and
# missed — is bounded, since the file itself remains in the inbox.
for f in ${NEW[@]+"${NEW[@]}"}; do
    echo "$f" >> "$SEEN"
done

# Surface them.
echo "## New cross-worktree messages (auto-surfaced by inbox hook)"
echo ""
echo "${#NEW[@]} unread message(s) in \`claude-hydra-messages/inbox/\`."
echo "After addressing each, \`mv\` the file to \`inbox/archive/\` per the protocol."
echo ""
for f in ${NEW[@]+"${NEW[@]}"}; do
    echo "---"
    echo ""
    echo "### Message: \`$f\`"
    echo ""
    cat "$INBOX/$f"
    echo ""
done
