#!/usr/bin/env bash
# UserPromptSubmit hook: surface a STANDING banner for unanswered design
# decisions, so an agent blocked on a decision is never invisible.
#
# See claude/agent-hierarchy.md § "Design decisions: a distinct channel".
# A decision is a file the asking agent drafts into
#   claude-hydra-messages/decisions/pending/<id>.md
# with a `blocking: yes|no` line. It bubbles UP the coordinator chain like a
# proposal: each coordinator answers what is within its authority (moving the
# file to answered/ and routing the answer DOWN to the asker), and forwards
# only genuinely user-level questions further up. Only the top coordinator's
# pending/ accumulates user-level decisions — and only there does this banner
# fire. A non-top agent moves its copy to decisions/forwarded/ on sending up,
# so its own pending/ stays empty and this hook stays silent there.
#
# UNLIKE proposals, a decision is often BLOCKING live work: the banner sorts
# blocking decisions first and counts them separately, and the disposition is
# not complete until the answer has been routed back DOWN to the asking agent
# (answered/ file records both the answer and the routed-to confirmation).
#
# Like proposals-hook.sh: no .seen state — re-surfaces every turn until the
# queue drains. Portability: no GNU-only find/stat flags (BSD laptop safe).

set -euo pipefail

ROOT="${CLAUDE_PROJECT_DIR:-$(git rev-parse --show-toplevel 2>/dev/null || pwd)}"
PENDING="$ROOT/claude-hydra-messages/decisions/pending"

[ -d "$PENDING" ] || exit 0

FILES=()
while IFS= read -r p; do
    [ -n "$p" ] && FILES+=("$p")
done < <(find "$PENDING" -maxdepth 1 -name '*.md' -type f | sort)

[ "${#FILES[@]}" -eq 0 ] && exit 0

BLOCKING=()
NONBLOCKING=()
for f in "${FILES[@]}"; do
    if grep -qiE '^blocking:[[:space:]]*(yes|true)' "$f" 2>/dev/null; then
        BLOCKING+=("$(basename "$f")")
    else
        NONBLOCKING+=("$(basename "$f")")
    fi
done

echo "## DESIGN DECISIONS awaiting disposition: ${#FILES[@]} (${#BLOCKING[@]} BLOCKING live work)"
echo ""
if [ "${#BLOCKING[@]}" -gt 0 ]; then
    echo "BLOCKING (an agent is stopped on each of these):"
    for b in "${BLOCKING[@]}"; do echo "- claude-hydra-messages/decisions/pending/$b"; done
    echo ""
fi
if [ "${#NONBLOCKING[@]}" -gt 0 ]; then
    echo "Non-blocking:"
    for n in "${NONBLOCKING[@]}"; do echo "- claude-hydra-messages/decisions/pending/$n"; done
    echo ""
fi
echo "Disposition: answer (or relay to the user), record the answer in the file,"
echo "move it to decisions/answered/, and ROUTE THE ANSWER DOWN to the asking"
echo "agent (verify-after-copy). A decision is not done until the asker has it."
