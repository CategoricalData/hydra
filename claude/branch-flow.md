# Branch promotion flow

Hydra uses a four-tier promotion ladder: `feature → integration → staging → main`.
Each tier has its own long-lived worktree. Promotion happens by merge.

```
feature_NNN  ──→  integration  ──→  staging  ──→  main  ──→  origin/main
 (active           (passive          (active        (passive         (remote)
 sessions)         collector)        checks)        local mirror)
```

For the read/modify rules across worktrees, see CLAUDE.md ("Working with worktrees").
For git-worktree mechanics (adding, removing, cherry-picks across worktrees),
see [worktree-workflow.md](worktree-workflow.md).
For the coordinator's view of spawning and finalizing the agents whose
changes travel this ladder, see [coordinator-workflow.md](coordinator-workflow.md).
For how staging fits the broader agent/issue hierarchy — the two homes for all
work, the per-machine staging model, and cross-machine push coordination — see
[agent-hierarchy.md](agent-hierarchy.md).

## Where work happens

- **Feature worktrees** are where agent sessions live and where conflicts are
  resolved. They are the only tier with active, ongoing work.
- **integration** is a passive collector. It typically has no agent session
  attached. It receives merges from feature branches and forwards them to staging.
- **staging** is the only intermediate tier with an attached agent session.
  After integration's batch arrives, the staging session runs `/sync`, `/test`,
  `/bootstrap`, and any optional checks the user specifies. Only after those pass
  does the batch advance to main. Staging also owns a set of **top-level
  non-issue duties** (orphan-issue triage, cross-machine coordination, fleet
  reconciliation) — its second half, distinct from promotion; see
  [Staging's non-issue duties](#stagings-non-issue-duties).
- **main** is a local stable mirror. No active session. It receives only from
  staging. The user pulls into local main first (so feature branches can pull
  from it to refresh their base), then pushes to `origin/main`.

## Conflict resolution happens in the feature worktree

When promoting a feature branch up to integration, the conflict-resolution work
belongs in the *feature* worktree, not in integration. Sequence:

1. In the feature worktree, pull `integration` into the feature branch
   (`git pull . integration` or `git merge integration`).
2. Resolve any conflicts there, using the agent session attached to the
   feature worktree.
3. Once the feature branch is clean and merges into integration would be
   conflict-free, perform the merge into integration's worktree.

This keeps the active agent session in the loop and keeps integration passive.
The same principle applies one level up: staging pulls from integration in the
staging worktree, with the staging agent session resolving any conflicts.

## Cadence

- **Don't promote up the chain reflexively.** After a merge lands at one level,
  wait for the next level to drain its current batch — e.g., a `/bootstrap`
  on staging still in progress. The user signals when each promotion is due.
- Every promotion is `git merge --no-ff`. Use a commit message of the form
  `Merge branch '<source>' into <dest>. For #NNN [#MMM ...]`.
- The destination worktree is where the merge command runs. If the feature
  session needs to drive a merge into a passive worktree (integration, main),
  the user must explicitly authorize it — per the "modify only your assigned
  worktree" rule in CLAUDE.md.
- `main` is pushed to `origin/main` by the staging session as part of its
  workflow loop (see [Staging workflow](#staging-workflow) below), once `/sync`,
  `/test`, and `/bootstrap` are green — but the session confirms with the user before
  each `origin/main` push, since it is outward-facing and hard to reverse.
  Feature and integration sessions never push to `origin/main`.

## Staging workflow

The staging worktree runs one repeating cycle. (This is the *staging tier's*
loop. A feature worktree may be directed by the user to run the same loop —
e.g. a long-lived feature branch driving its own sync → bootstrap → push
cadence — but by default this belongs to staging.)

Each iteration:

1. **Pull in new work.** Merge the local `integration` branch into staging
   (`git merge integration` from the staging worktree). When the user directs,
   also pull a remote GCE-based branch they name. Resolve any conflicts here,
   in the staging worktree.
1a. **WIP-commit check on the pulled history.** Immediately after pulling,
   scan the newly-arrived commits for the `WIP:` prefix
   (`git log --oneline <prev-staging-tip>..staging | grep '^[0-9a-f]* WIP:'`).
   `WIP:` marks unfinalized work that must **not** reach `origin/main` (see the
   commit-workflow rules in CLAUDE.md). Feature/integration sessions routinely
   leave `WIP:` commits, and a merge drags them in wholesale — this is the main
   way the prefix leaks onto main. If any are found, **stop and propose a
   remediation to the user before continuing the cycle** (see
   [Handling pulled WIP commits](#handling-pulled-wip-commits)). Do not `/sync`
   or push past unresolved `WIP:` commits.
2. **`/sync`** — the default full host × target sync.
3. **`/test all`** — full all-hosts target-language test validation (user
   directive 2026-07-04: `all` is the standard scope for every staging cycle,
   not the triad). `/sync` only runs Haskell `stack test`; `/test all` closes
   the gap by running every target head's own suite against the freshly-synced
   `dist/`, catching target-runtime breakage before it reaches CI.
4. **`/bootstrap`** — the default triad (haskell/java/python) bootstrap demo.
5. **On failure in step 2, 3, or 4:** attempt a fix. If the correct fix is clear,
   apply it and re-run the failing step and everything after it (per the
   shorthand-commands "resume from the failing step" rule in CLAUDE.md). If the
   fix is not clear, reach out to the user before proceeding. Repeat the failing
   step until green.
6. **Push to `origin/main`** once `/sync`, `/test`, and `/bootstrap` are all green.
   Confirm with the user before the push (see the cadence note above).
   **Pre-push WIP gate:** before pushing, scan the full range that would land
   on main (`git log --oneline origin/main..staging | grep '^[0-9a-f]* WIP:'`).
   It must be empty. This catches `WIP:` commits regardless of origin — both
   pulled (step 1a) *and self-authored* during the cycle's fix work. (This
   session's leak was largely self-authored fix commits left as `WIP:`.) If any
   remain, finalize them first (drop the `WIP:` prefix, squashing into focused
   topic commits where appropriate per [.claude/commands/squash.md](../.claude/commands/squash.md))
   — never push `WIP:` to main.
7. **Monitor the CI build** after the push.
8. **Handle CI fallout:**
   - For a clear, simple fix, fold it into the next cycle.
   - For a complicated or non-obvious issue, surface it to the user and draft a
     GitHub issue — **do not file it without explicit approval** (per the
     "Never file a GitHub issue …" hard rule in CLAUDE.md).

### Cycle cadence

- The next cycle may begin as soon as the push to `origin/main` succeeds; it
  does not wait for CI to go green.
- When a CI issue surfaces mid-cycle, decide case-by-case whether to **interrupt**
  the running `/sync`, `/test`, or `/bootstrap` to fix it now, or let the current cycle
  finish and fix it in the next one. **If in doubt, ask the user.**
- This is the staging tier's own loop; promotion *into* staging still follows
  the cadence and conflict rules above (resolve conflicts in the source worktree).

### Handling pulled WIP commits

When step 1a finds `WIP:` commits inside a merge from `integration` or a
user-named remote branch (e.g. `409`), remediation has **two parts** — cleaning
staging is necessary but *not sufficient*; the source must also be corrected, or
the same WIP commits leak again on the next pull.

1. **Clean staging (immediate, no force-push).** Rebuild the pulled commits into
   finalized, non-`WIP:` history *on staging* — soft-reset to the merge base and
   re-commit as focused topic commits (per
   [.claude/commands/squash.md](../.claude/commands/squash.md)), or, when 1:1
   granularity is worth keeping, just reword to drop the `WIP:` prefix. This is
   local to staging, touches no shared remote branch, and lets main stay clean.
   Do this first.
2. **Correct the source (required follow-up; needs explicit approval).** The
   source branch still carries the `WIP:` commits, so they will return on the
   next pull. Propose bringing it in line with the cleaned history. If the source
   is a shared remote branch (`409`, `integration`), this means a **force-push**,
   which can disrupt another agent actively working there. So:
   - **Never force-push a shared remote branch automatically.** Stop, show the
     user the exact `WIP:` commits and the proposed cleaned history (old → new
     SHAs), and get **explicit approval** before any `git push --force`.
   - Coordinate first if another session may be on that branch (e.g. the feature
     worktree that produced it). A force-push under someone's feet is worse than
     a delayed cleanup.
   - Prefer the narrowest correction: a limited force-push of just the rewritten
     range, not a wholesale branch replacement.

Treat the cycle's WIP remediation as unfinished until both parts are done (or the
user explicitly defers the source-side cleanup). Record any deferred source
cleanup in the branch plan so it isn't lost.

## Staging's non-issue duties

The [Staging workflow](#staging-workflow) above is staging's *promotion* half.
Staging also owns the **top-level, non-issue work** that isn't associated with
any particular GitHub issue — the second of the [two homes for all
work](agent-hierarchy.md#two-homes-for-all-work). Keeping this here (rather than
letting it default to whoever happens to run staging) is what separates staging's
promotion role from issue-coordination: issue-work goes to the issue-tree agent
hierarchy; top-level non-issue work stays with staging.

There is **one staging agent per machine**, and all of them push to the same
`origin/main`, so several of these duties are inherently cross-machine.

1. **Orphan-issue scan.** Periodically list non-`release_*` issues that have no
   parent and prompt the user to assign one. Every non-release issue should have
   a parent; humans occasionally file one without, and this is the backstop that
   catches them (and any that an agent filed without the mandatory parent). This is
   draft-and-show — **never re-parent an issue without explicit user approval.**
   Run [`bin/scan-orphan-issues.sh`](../bin/scan-orphan-issues.sh) — it lists
   every open issue with no parent, excluding the `release_*` roots (read-only;
   it never files or re-parents). Surface its output to the user for parent
   assignment.
2. **Cross-machine merge-queue coordination.** Before dispatching a red-CI fix or
   pushing a batch, check whether another machine's staging already has it in
   flight, and announce your own. See
   [agent-hierarchy.md § Cross-machine staging coordination](agent-hierarchy.md#cross-machine-staging-coordination)
   for the full protocol (contended `origin/main`, the announcement channel, push
   serialization, red-main ownership tie-break).
3. **Periodic fleet reconciliation.** On a regular cadence, sweep for orphaned
   worktrees and left-behind (merged-but-not-finalized) agents. A single staging
   agent sees only its own machine's worktrees, so reconciliation is
   cross-machine: each staging agent reports its fleet's disposition and a
   designated one (or the release coordinator) collates. **Never finalize another
   machine's agent** — a worktree showing "merged + 0 commits ahead" can be an
   active agent with uncommitted WIP that only the owning machine can see.
4. **Standing messaging authority.** A staging agent may **send messages freely**
   — to any issue agent, coordinator, or sibling staging agent, on any machine —
   without per-send user permission, and **every other agent should respond freely
   to a staging agent** without waiting on the user. Staging is the fleet's
   coordination hub: it drives the promotion ladder, adjudicates red-main, and
   collates cross-machine reconciliation, so gating its coordination traffic on the
   user would make the user the bottleneck the whole framework exists to remove.
   This authority is **standing and dial-independent** — it holds even at autonomy
   level *low*, which otherwise means "ask per send" (see
   [agent-hierarchy.md § The autonomy dial](agent-hierarchy.md#the-autonomy-dial)).
   It covers **coordination messaging only**: it does *not* lift the draft-and-show
   rule for GitHub writes (issue filing, closing, labeling, commenting) or any other
   outward action — those still follow the dial and hard rule 5.
5. **External process alerts.** Beyond agent-to-agent traffic, a staging agent may
   receive messages from **non-agent processes on its machine** — a health/resource
   watchdog, a monitoring script, a CI hook — alerting it to a machine-level
   condition (memory pressure, an impending freeze, disk filling) that the running
   agents cannot see from inside their worktrees and may need to respond to. As the
   per-machine coordination hub, staging is the recipient for these host-level
   signals; the usual response is fleet back-pressure (throttle/pause build work
   until the condition clears). See [`external-alerts.md`](external-alerts.md) for
   what these alerts look like, how to tell one from a peer message, and how to
   handle it.
