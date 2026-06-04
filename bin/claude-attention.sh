#!/usr/bin/env bash
# At-a-glance view of which Claude sessions are blocked.
#
# Reads markers written by .claude/notification-hook.sh and prints a
# sorted summary. Color-coded: permission_prompt = urgent, anything
# else = informational. Run as a one-shot or via watch:
#
#   bin/claude-attention.sh
#   watch -n 5 bin/claude-attention.sh

set -euo pipefail

DIR="$HOME/.cache/claude-attention"

if [ ! -d "$DIR" ] || ! ls -1 "$DIR"/*.txt >/dev/null 2>&1; then
    echo "No active Claude attention markers."
    exit 0
fi

# ANSI colors when stdout is a TTY.
if [ -t 1 ]; then
    R=$'\033[31m'; Y=$'\033[33m'; G=$'\033[32m'; D=$'\033[2m'; N=$'\033[0m'
else
    R=""; Y=""; G=""; D=""; N=""
fi

printf "%-22s %-30s %s\n" "TYPE" "SESSION" "AGE"
printf "%-22s %-30s %s\n" "------" "-------" "---"

for f in "$DIR"/*.txt; do
    name="$(basename "$f" .txt)"
    type="$(sed -n 's/^type:[[:space:]]*//p' "$f" | head -1 || true)"
    ts="$(sed -n 's/^ts:[[:space:]]*//p' "$f" | head -1 || true)"
    # Fallback for legacy markers (old hook format): pull from JSON payload.
    if [ -z "$type" ] && command -v jq >/dev/null 2>&1; then
        type="$(grep -E '^[{]' "$f" | head -1 | jq -r '.notification_type // ""' 2>/dev/null || true)"
    fi
    if [ -z "$type" ]; then
        type="$(grep -oE '"notification_type"[[:space:]]*:[[:space:]]*"[^"]+"' "$f" | head -1 | sed -E 's/.*"([^"]+)"$/\1/' || true)"
    fi
    [ -z "$type" ] && type="unknown"
    # Fallback for ts: use file mtime.
    if [ -z "$ts" ]; then
        ts="$(date -u -d "@$(stat -c %Y "$f")" +%Y-%m-%dT%H:%M:%SZ 2>/dev/null || true)"
    fi

    # Age in human form.
    if [ -n "$ts" ]; then
        ts_epoch="$(date -d "$ts" +%s 2>/dev/null || echo 0)"
        now="$(date +%s)"
        age_s=$((now - ts_epoch))
        if [ "$age_s" -lt 60 ]; then age="${age_s}s ago"
        elif [ "$age_s" -lt 3600 ]; then age="$((age_s/60))m ago"
        else age="$((age_s/3600))h ago"; fi
    else
        age="?"
    fi

    case "$type" in
        permission_prompt) color="$R" ;;
        idle_prompt)       color="$D" ;;
        *)                 color="$Y" ;;
    esac
    printf "%b%-22s%b %-30s %s\n" "$color" "$type" "$N" "$name" "$age"
done | sort
