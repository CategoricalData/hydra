#!/usr/bin/env bash
# Anti-regression guard for #416 Phase-A build host-independence.
#
# #416's near-term sub-goal: no stage downstream of the DSL→JSON boundary drags
# a specific host toolchain. The digest/freshness stages were promoted to pure
# bash (bin/digest.sh) — fresh / refresh / refresh-input. This test fails if any
# PRODUCTION build script re-introduces a `stack exec digest-check` invocation
# (the promoted-away host-call class), so host-dependence can't creep back in
# silently after the promotion lands.
#
# What is ALLOWED (not flagged):
#   * The DSL→JSON boundary + the JSON→target coder (update-json-*, bootstrap-from-json,
#     transform-json-to-target) — legit host use above/at the boundary or the
#     host-selectable codegen engine. This guard only polices `digest-check`.
#   * Test-oracle harnesses (bin/test-*.sh) that run digest-check as their
#     comparison oracle — not part of the shipped downstream build.
#
# Scope: bin/ + heads/*/bin/ production scripts. Comments and the conformance
# oracle are excluded. Hermetic, stack-free, fast. Wired into test-regressions.sh.

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT_DIR="${HYDRA_ROOT_DIR:-$( cd "$SCRIPT_DIR/.." && pwd )}"

# Test-oracle harnesses legitimately invoke digest-check as their comparison
# oracle; they are not part of the shipped build. Exclude by basename.
ORACLE_HARNESSES='
bin/test-digest-conformance.sh
bin/test-orphan-reconcile.sh
bin/test-json-content-invalidates-render.sh
'

is_oracle_harness() {
    local f="$1" h
    for h in $ORACLE_HARNESSES; do
        [ "$f" = "$h" ] && return 0
    done
    return 1
}

cd "$HYDRA_ROOT_DIR"

# Every line invoking digest-check as a subcommand (fresh/refresh/refresh-input),
# excluding comment lines (leading # after indent) and the `stack exec digest-check`
# mentions that appear inside comments/strings. We match an actual invocation:
# `... digest-check -- <subcommand>` not preceded by a `#`.
violations=0
while IFS= read -r hit; do
    [ -n "$hit" ] || continue
    file="${hit%%:*}"
    rest="${hit#*:}"          # line-number:content
    content="${rest#*:}"
    # Skip comment lines.
    trimmed="$(printf '%s' "$content" | sed 's/^[[:space:]]*//')"
    case "$trimmed" in \#*) continue;; esac
    # Skip the allowed oracle harnesses.
    if is_oracle_harness "$file"; then continue; fi
    echo "REGRESSION [$file]: production script invokes digest-check (promoted to bin/digest.sh):" >&2
    echo "  $hit" >&2
    violations=$((violations + 1))
done < <(grep -rnE 'digest-check -- (fresh|refresh|refresh-input)' bin heads --include='*.sh' 2>/dev/null || true)

if [ "$violations" -ne 0 ]; then
    echo "test-build-host-independence: FAILED — $violations production digest-check invocation(s) found." >&2
    echo "  The digest stages are promoted to pure bash (bin/digest.sh); call that, not \`stack exec digest-check\`." >&2
    exit 1
fi

echo "test-build-host-independence: OK — no production digest-check invocation in the downstream build path."
