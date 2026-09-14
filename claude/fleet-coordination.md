# Fleet coordination

Cross-machine staging coordination (see [agent-hierarchy.md](agent-hierarchy.md),
§ Cross-machine staging coordination) runs over a dedicated **`coordination` branch
of the wiki repository** — not over the rendered wiki, and not over a pinned issue.

- [fleet-state.md](fleet-state.md) mirrors `STATE.md` on that branch: durable current
  truth (machines, standing arrangements, acting-coordinator records). Edited in place
  on the `coordination` branch; never grows.
- [fleet-log.md](fleet-log.md) mirrors `LOG.md`: append-only event stream (red-CI
  claims, contested-issue claim-checks, unusual-batch announcements). Claims are
  commits: a successful push acquires the claim; a rejected push means someone else
  got there first — pull, read, defer.
- Routine pushes to `origin/main` are NOT announced (git's fetch-merge-before-push +
  non-fast-forward rejection already serializes them safely).
- The `coordination` branch is periodically deleted and recreated (STATE.md carried
  forward) so its churn never bloats the wiki's history. It is deliberately unrendered.
- **Why the wiki repo and not the main hydra repo or an issue:** main-repo events
  (commits, issue comments) are echoed to the community Discord — coordination
  churn there would spam human developers. Wiki changes are not echoed. Do NOT
  migrate this channel into the main repo or onto a GitHub issue.

Agents: fetch the `coordination` branch before any push to main, before claiming a
red-CI fix or a contested issue, and every ~10-15 minutes inside long watcher loops.

**`coordination` is an INBOUND channel — keep a standing listener, not an
action-triggered one.** The three triggers above are all things *you* initiate
(your push, your claim, your watcher loop). Another machine can post a claim, a
stand-down, or a question at any moment without you having done anything to invite
it, so a listener keyed to your own actions misses exactly the traffic that matters
most. A staging session polls `coordination` for its whole lifetime — armed at
session start, re-armed whenever the poll expires — whether or not it is expecting
a reply. Note that background monitors on some machines die after ~30 minutes, so
"armed once" is not "armed"; re-arming is part of the duty.

This is not hypothetical. On 2026-09-14 both staging agents lapsed at once: alpha
posted a #740 stand-down that sat unread on marvin7 while marvin7 prepared to
investigate the same issue, and alpha separately acknowledged having stopped
polling. A stand-down read late is worse than useless — it arrives after the
duplicated work is done. `bin/claude-hooks/coordination-hook.sh` now surfaces new
entries on every turn so this does not depend on an agent remembering; the standing
poll remains the backstop for long gaps between turns.

Note: this page and [fleet-state.md](fleet-state.md) / [fleet-log.md](fleet-log.md)
were moved here from the public wiki (where they were orphaned, agent-operational
pages) — see `claude/` in the main repo, not the wiki, for the current copies. The
live, authoritative `STATE.md`/`LOG.md` remain on the wiki's `coordination` branch;
these are point-in-time snapshots taken at the move.
