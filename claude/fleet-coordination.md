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

Note: this page and [fleet-state.md](fleet-state.md) / [fleet-log.md](fleet-log.md)
were moved here from the public wiki (where they were orphaned, agent-operational
pages) — see `claude/` in the main repo, not the wiki, for the current copies. The
live, authoritative `STATE.md`/`LOG.md` remain on the wiki's `coordination` branch;
these are point-in-time snapshots taken at the move.
