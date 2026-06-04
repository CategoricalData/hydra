#!/usr/bin/env bash
# Unified Claude session status board.
#
# For every tmux session named bug_*  (or feature_409_*), shows:
#   - state:  blocked / idle / working / unknown
#   - focus:  current in-progress TodoWrite task (scraped from pane)
#   - age:    how long the state has been the same
#
# Use as one-shot or in a watch loop:
#   bin/claude-board.sh
#   watch -n 5 bin/claude-board.sh

set -uo pipefail

ATTN="$HOME/.cache/claude-attention"

# ANSI colors when stdout is a TTY.
if [ -t 1 ]; then
    R=$'\033[31m'; Y=$'\033[33m'; G=$'\033[32m'; D=$'\033[2m'; B=$'\033[1m'; N=$'\033[0m'
else
    R=""; Y=""; G=""; D=""; B=""; N=""
fi

# Gather list of relevant tmux sessions.
mapfile -t SESSIONS < <(tmux ls -F '#S' 2>/dev/null | grep -E '^(bug_|feature_409_)' || true)

if [ "${#SESSIONS[@]}" -eq 0 ]; then
    echo "No Claude sessions running (no tmux sessions match bug_*  or feature_409_*)."
    exit 0
fi

# State extraction per session.
declare -A STATE TYPE FOCUS AGE_S
for s in "${SESSIONS[@]}"; do
    marker="$ATTN/$s.txt"

    # Default state if no marker.
    STATE[$s]="working"
    TYPE[$s]="-"
    AGE_S[$s]="-"

    # Live pane check: is there a "Do you want to proceed?" prompt visible
    # right now? That's a more reliable signal than the marker, which can
    # be stale if Stop hasn't fired yet in a long autonomous turn.
    pane="$(tmux capture-pane -t "$s:0" -p 2>/dev/null || true)"
    if echo "$pane" | grep -qE 'Do you want to proceed\?'; then
        STATE[$s]="blocked"
        TYPE[$s]="permission_prompt"
        # Use the marker's mtime if available for age; otherwise unknown.
        if [ -f "$marker" ]; then
            mt="$(stat -c %Y "$marker" 2>/dev/null || echo 0)"
            now="$(date +%s)"
            AGE_S[$s]=$((now - mt))
        fi
    elif [ -f "$marker" ]; then
        # No live prompt visible; rely on marker file for last-known state.
        t="$(sed -n 's/^type:[[:space:]]*//p' "$marker" | head -1)"
        if [ -z "$t" ]; then
            t="$(grep -oE '"notification_type"[[:space:]]*:[[:space:]]*"[^"]+"' "$marker" | head -1 | sed -E 's/.*"([^"]+)"$/\1/')"
        fi
        [ -z "$t" ] && t="unknown"
        TYPE[$s]="$t"

        mt="$(stat -c %Y "$marker" 2>/dev/null || echo 0)"
        now="$(date +%s)"
        AGE_S[$s]=$((now - mt))

        case "$t" in
            permission_prompt) STATE[$s]="working" ;;  # marker stale; pane says no live prompt
            idle_prompt)       STATE[$s]="idle" ;;
            *)                 STATE[$s]="attn" ;;
        esac
    fi

    # Scrape current focus from the pane: look for the in_progress TodoWrite
    # marker (◼) and grab its description.
    focus="$(tmux capture-pane -t "$s" -p 2>/dev/null | grep -oE '◼ .{0,80}' | tail -1 | sed 's/^◼ //' || true)"
    if [ -z "$focus" ]; then
        # Fallback: latest tool-call line (lines starting with ● Bash, ● Read, etc.).
        focus="$(tmux capture-pane -t "$s" -p 2>/dev/null | grep -E '^● ' | tail -1 | sed -E 's/^● //; s/ *⎿.*//' | cut -c1-80 || true)"
    fi
    [ -z "$focus" ] && focus="-"
    FOCUS[$s]="$focus"
done

# Sort key: blocked first, then attn, then idle, then working.
order_for() {
    case "$1" in
        blocked) echo "0" ;;
        attn)    echo "1" ;;
        idle)    echo "2" ;;
        working) echo "3" ;;
        *)       echo "4" ;;
    esac
}

# Build sortable lines.
LINES=()
for s in "${SESSIONS[@]}"; do
    o="$(order_for "${STATE[$s]}")"
    LINES+=("$o|$s")
done
IFS=$'\n' SORTED=($(printf '%s\n' "${LINES[@]}" | sort -t'|' -k1,1n -k2,2))

# Pretty-print.
fmt_age() {
    local s="$1"
    [ "$s" = "-" ] && { echo "-"; return; }
    if   [ "$s" -lt 60 ];   then echo "${s}s"
    elif [ "$s" -lt 3600 ]; then echo "$((s/60))m"
    elif [ "$s" -lt 86400 ]; then echo "$((s/3600))h"
    else                         echo "$((s/86400))d"
    fi
}

state_color() {
    case "$1" in
        blocked) echo "$R" ;;
        attn)    echo "$Y" ;;
        idle)    echo "$D" ;;
        working) echo "$G" ;;
        *)       echo "" ;;
    esac
}

state_glyph() {
    case "$1" in
        blocked) echo "⛔" ;;
        attn)    echo "⚠ " ;;
        idle)    echo "⏸ " ;;
        working) echo "🟢" ;;
        *)       echo "? " ;;
    esac
}

printf "%b   %-9s %-36s %-6s %-50s%b\n" "$B" "STATE" "SESSION" "AGE" "FOCUS" "$N"
printf "   %-9s %-36s %-6s %-50s\n"     "─────" "───────" "───" "─────"

for line in "${SORTED[@]}"; do
    s="${line#*|}"
    st="${STATE[$s]}"
    c="$(state_color "$st")"
    g="$(state_glyph "$st")"
    age="$(fmt_age "${AGE_S[$s]}")"
    printf "%b%-3s %-9s %-36s %-6s %-50s%b\n" "$c" "$g" "$st" "$s" "$age" "${FOCUS[$s]}" "$N"
done
