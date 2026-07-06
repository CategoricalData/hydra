# Coordinator workflow

A coordinator session spawns workers, reviews their work, shepherds it
through the staging cycle to `main`, and finalizes each worker once its
change is fully landed. This is the coordinator-side companion to
[`sub-claude-handoff.md`](../sub-claude-handoff.md) (the *worker's* view of
handoff) and [`branch-flow.md`](branch-flow.md) (the promotion ladder a
change travels: feature → integration → staging → main).

## Spawning a worker

Spawn via
`COORDINATOR=<coord-worktree> bin/spawn-issue-worktree.sh <issue-number> <slug>`.
The `COORDINATOR` env var is required and names the worktree the new worker
addresses for ready-to-stage handoffs and coordination questions; it varies
per machine and per epoch (the coordinator role shifts as sessions come and
go), so there is no sensible hardcoded default — the operator sets it at
spawn time. The script creates a worktree, seeds `.claude/`, opens a
detached `tmux` session, and starts Claude inside it via `claude-remote`.

Seed the assignment (via the worker's `claude-hydra-messages/inbox/`) with
the issue, the authoring pattern to follow, the standing rules, and any
sibling-collision / moving-`main` notes relevant to the current fleet.

### Model selection

`claude-remote` accepts an optional `-m <model>` / `--model <model>` flag
(`opus`, `sonnet`, `opusplan`) and defaults to `opus`. Policy:

- **`bug_NNN_*` and `task_NNN_*` workers** — default to **`opusplan`**. Most
  spend the bulk of their wall time on mechanical sync/regen/verify loops,
  which Sonnet handles fine; Opus stays in the loop for the planning/triage
  phase via opusplan's mode-switching.
- **`feature_NNN_*` workers** — stay on **`opus`**. Feature work is
  planning-heavy across the whole branch lifetime (matrix decisions, worker
  triage, plan-doc curation), so the cost is justified.
- **Override for tricky bugs**: `SPAWN_MODEL=opus bin/spawn-issue-worktree.sh …`
  bumps a bug worker to plain Opus when the issue is genuinely
  architecture-level or under-specified.

Manual spawns (no script) follow the same defaults:
`claude-remote bug_NNN_slug -m opusplan` for bug/task workers,
`claude-remote feature_NNN_slug` (or `-m opus`) for feature workers.

## Worker lifecycle

Spawning is step 1 of a lifecycle the coordinator owns end to end.

1. **Spawn** — above.
2. **Implement + review** — the worker builds; the coordinator reviews the
   diff *independently* (do not rubber-stamp a self-report) and returns a
   verdict, iterating until correct.
3. **Rebase + squash + verify** — on the coordinator's signal the worker
   rebases onto the current `main` tip (`git fetch` first; `main` moves
   without notice), squashes to clean topic commits (no `WIP:` prefix; the
   issue-closing commit ends `Resolves #<issue>`, others `For #<issue>`),
   and re-validates. **Validate every host the change generates into, not
   just the triad** (haskell/java/python): flat-namespace Lisp dialects
   (Common Lisp, Emacs Lisp, Scheme) need generated files *and* loader-list
   entries the triad never exercises, and a triad-only handoff for a
   host-generating module has reddened `main` after landing.
4. **Pause to save context** — once the change is rebased, squashed,
   verified, and the issue is considered resolved (with a `Resolves #`
   commit), ask the worker to `/save` its branch plan, then **end the
   worker's Claude session** so it stops consuming context and polling
   while it waits on the staging cycle (which the coordinator, not the
   worker, drives). Do this **only after the coordinator has pulled the
   worker's commits into the coordinating worktree** — never end a session
   whose only copy of the work is its own worktree state. A paused worker
   is idle, not gone.
5. **Reopen if bugs surface** — if a defect appears (e.g. a red CI job
   traced to the change), start a **new** Claude session for that worker
   (usually `opusplan`; plain `opus` for architecture-level fixes) and give
   it instructions. Then re-pause or finalize as appropriate.
6. **Finalize** — once the staging cycle completes, the change lands on
   `main`, **and CI on the landing tip is green** (not merely "pushed" — a
   push with red CI is not finalized): `/save` the branch plan again if it
   changed since the pause, shut down the Claude process, and copy the
   branch plan to `worktrees/closed/<branch>-plan.md` (the archive
   `spawn-issue-worktree.sh` checks to avoid re-spawning a closed issue).
   Archive the plan **before** `git worktree remove` (removing first loses
   the plan). The worktree may then be removed. Do **not** finalize on a red
   or in-progress CI run — reopen (step 5) instead.

## Context hygiene (an ongoing duty, not a lifecycle step)

The lifecycle above deals with a worker whose *work* is done (pause/finalize).
Context hygiene is different: it applies to a worker whose *work continues*
but whose **context window has grown disproportionate to the value of what it
is still doing**. A worker dragged through several rebases, multiple
validation rounds, a crash recovery, or a re-engagement can accumulate a huge
transcript while its *remaining* task is small — burning tokens (and slowing
itself) far out of proportion. Left unattended this is a silent, steady cost
across a long-lived fleet.

**Make it a periodic sweep.** On a regular cadence (e.g. whenever you survey
the fleet, or when a milestone lands), check each live worker's context size —
the TUI status bar shows a `save NNNk tokens` hint, and the session transcript
size is a proxy. Flag any worker whose context is large (rule of thumb:
hundreds of k tokens) **relative to the work it has left**. A big context on a
worker about to finish is fine — let it finish; a big context on a worker with
hours of work ahead is worth resetting.

**The intervention is a well-timed reset, not a pause.** For a flagged
still-active worker:

1. **Wait for a safe boundary.** Never reset mid-operation (mid-sync, mid-
   rebase, mid-`stack test`). Reset only at a natural seam: after a commit,
   between validation rounds, after a delivery. The worker's uncommitted /
   in-flight state must be durable first — a commit, or captured in the branch
   plan — because a reset re-reads from disk, not from the lost context.
2. **`/save` with compaction.** Have the worker `/save` its branch plan, and
   as part of that **prune no-longer-relevant material** — superseded
   approaches, resolved dead-ends, stale intermediate SHAs, verbose logs from
   earlier rounds. The plan should end up as a tight, current
   resume-from-cold brief: what shipped, what's verified, what's left, and any
   live constraints — not a running diary. A bloated plan defeats the reset.
3. **`/compact` or restart.** Then either `/compact` the session, or restart
   it (`claude-remote <slug> -m <model> --continue`, which re-reads the saved
   plan fresh). Restart gives the cleanest context; `/compact` is lighter-
   weight when the worker is mid-task-sequence and you only need headroom.
   Re-orient the restarted session with a short inbox message stating exactly
   where it is and what's next (it has the plan, but a one-line "you are here"
   saves a re-derivation).

The test for whether to intervene: *if this worker crashed right now and had
to resume purely from its branch plan, would it lose important state?* If yes,
its plan isn't current enough — fix that (step 2) regardless of whether you
then reset. If no, and its context is large with real work remaining, reset
it. The branch plan being a faithful cold-resume brief is the invariant that
makes both context hygiene and the pause/reopen steps safe.

## Periodic fleet reconciliation (no orphans)

Workers get left behind — most often a worker whose change *landed* but whose
finalize (archive plan → end session → remove worktree) never ran, because
the landing happened during a busy stretch. Left unattended, these accumulate
as live Claude sessions burning resources for work that shipped days ago, and
as untracked worktrees/branches. Finalizing recent workers promptly is not
enough; you must also **periodically sweep for the ones that slipped**.

**The sweep** — for every worktree you might own:
1. Is its branch an ancestor of `origin/main` (merged)? `git merge-base
   --is-ancestor <branch> origin/main`.
2. Does it still have a live tmux/Claude session? `tmux has-session`.
3. How many commits ahead of main? `git rev-list --count origin/main..<branch>`.

A worktree that is **merged + still has a live session + 0 commits ahead** is
the classic left-behind-finalize case → finalize it. But two traps:
- **0-commits-ahead does NOT mean orphaned.** An *active* worker with
  uncommitted in-tree WIP also shows 0 ahead. Check last session activity
  before concluding a merged/0-ahead worktree is finished — a recently-active
  one is working, not done.
- **Only finalize workers you own.** A coordinator must not finalize another
  coordinator's workers, intentionally-parked worktrees, or release/detached
  worktrees.

### Cross-coordinator reconciliation

When more than one coordinator (or a coordinator + the staging session) owns
workers, a one-sided sweep can't guarantee zero orphans — you can't finalize
what you don't own, and you can misread another owner's active worker as an
orphan. So reconcile *cooperatively*:

1. **Build the authoritative table** from the sweep above across ALL
   worktrees (merged? live? commits-ahead?), tagged by your best guess of
   owner and lifecycle state.
2. **Finalize your own** left-behinds first.
3. **Send the other coordinator ONE reconciliation message** — not a bare
   list. Structure it: (a) *your* fleet, all accounted for; (b) *their*
   fleet with your read in brackets, split into ACTIVE (do-not-touch),
   FINALIZE-CANDIDATES (merged + session up + issue resolved), and
   OTHER/non-worker (release, detached). Explicitly name the active ones so a
   sweep can't kill a live worker.
4. **Ask them to reply with dispositions** — what they finalize, what they
   *intentionally keep* (with the reason: a paused-pending-user-review
   worktree, a deferred-work branch, a dormant collector), and what is
   criterion-gated (landed but waiting on CI-green). The reply, not your
   list, is what closes the reconciliation.

The goal state: every worktree is provably one of — an active worker, a
deliberately-parked worker (with a documented reason), a finalize queued on a
stated criterion, or already finalized. Nothing is merely "still there."
Expect to correct each other: the owner of a worker always knows its true
state better than another coordinator's scan (0-ahead misreads active WIP as
done; a "leftover"-looking worktree may be a load-bearing review artifact).

## Why the coordinator holds the human-gated actions

Workers never push to `main`, never create/close/comment on GitHub issues,
and never merge — those stay with the coordinator (and, for the actual push
to `origin/main`, the designated staging session; see
[`branch-flow.md`](branch-flow.md)). Concentrating the human-in-the-loop
friction (auth, CI decisions, revert judgment) in one place — rather than
scattering it across every worker — lets workers run fully autonomously up
to handoff, and is why a well-run worker rarely needs the human directly
while staging does.
