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
red-CI fix or a contested issue, and on the cadence below.

**Polling cadence: exponential backoff with reset — the same rule as the inbox.** This branch is a
message channel, so it follows the cadence defined canonically for cross-worktree messages
(`../../external/agents/docs/cross-worktree-messages.md`, "Polling cadence"):

- Poll after **1 minute**. If nothing new arrived, poll again after **2**, then **4**, then **8** —
  doubling each idle round.
- **Cap at 15 minutes.** Once there, keep polling every 15 minutes indefinitely; do not back off
  further.
- **Reset to 1 minute whenever you SEND or RECEIVE** anything on the channel. Traffic in *either*
  direction signals an active conversation — receiving a reply tightens the cadence just as sending
  does. It decays again only through consecutive idle polls.

A flat idle interval is wrong right after traffic: that is exactly when the next message is most
likely, and a 15-minute poll wastes the whole probable window. Being otherwise busy is not an
exemption — a long build does not excuse missing a stand-down.

When waiting on a named external event instead (a CI run, a long build), match that event's own
timescale rather than this schedule.

The **inbox** (`claude-hydra-messages/`) needs no schedule: `bin/claude-hooks/inbox-hook.sh` surfaces
new messages on every turn, and `bin/claude-hooks/coordination-hook.sh` now does the same for this
branch. The cadence above governs explicit polling *between* turns — a long autonomous stretch where
no turn boundary occurs to fire the hooks.

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
