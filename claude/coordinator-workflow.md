# Coordinator workflow

A coordinating agent spawns child agents, reviews their work, shepherds it
through the staging cycle to `main`, and finalizes each child once its
change is fully landed. This is the coordinator-side companion to
[`agent-handoff.md`](agent-handoff.md) (the *assigned agent's* view of
the same lifecycle) and [`branch-flow.md`](branch-flow.md) (the promotion
ladder a change travels: feature → integration → staging → main).

"Coordinator" is a *responsibility an issue agent takes on*, not a separate
kind of agent: every issue agent whose issue has actively-worked children, by
that fact, manages those children in addition to its own issue. For the full
model — the agent/issue-tree correspondence, the two agent kinds (issue vs.
staging), the two homes for all work, the new-issue proposal lifecycle,
role-scoped context, model-selection, and cross-machine staging coordination —
see [`agent-hierarchy.md`](agent-hierarchy.md). This doc is the mechanics an
issue agent uses to run its children's lifecycle.

## Spawning an agent

Spawn via
`COORDINATOR=<coord-worktree> bin/spawn-issue-worktree.sh <issue-number> <slug>`.
The `COORDINATOR` env var is required and names the worktree the new agent
addresses for ready-to-stage handoffs and coordination questions; it varies
per machine and per epoch (the coordinator role shifts as sessions come and
go), so there is no sensible hardcoded default — the operator sets it at
spawn time. The script creates a worktree, seeds `.claude/`, opens a
detached `tmux` session, and starts the agent session inside it (currently
Claude Code via `claude-remote`; the lifecycle below is framework-neutral).

Seed the assignment (via the agent's `claude-hydra-messages/inbox/`) with
the issue, the authoring pattern to follow, the standing rules, and any
sibling-collision / moving-`main` notes relevant to the current fleet.

### Model selection

The **per-role model policy is authoritative in
[`agent-hierarchy.md § Model selection`](agent-hierarchy.md#model-selection-by-agent-role)**
— release/high-stakes coordinators run **Fable**, staging runs **Opus**, simple
issue agents run **`opusplan`**, and an issue agent that takes on children may be
escalated. Consult that table when choosing; don't duplicate the policy here.

Mechanically: `claude-remote` takes `-m <model>` / `--model <model>` (e.g.
`opus`, `sonnet`, `opusplan`, `fable`) and defaults to `opus`;
`spawn-issue-worktree.sh`'s `SPAWN_MODEL` (default `opusplan`) is how the
spawning parent applies the policy. Override at spawn, e.g.
`SPAWN_MODEL=opus bin/spawn-issue-worktree.sh …` for an architecture-level issue,
or set it to `fable` for a high-stakes coordinator. Manual spawns follow the
same table: `claude-remote <type>_NNN_slug -m <model>`.

## Agent lifecycle

Spawning is step 1 of a lifecycle the coordinator owns end to end.

1. **Spawn** — above.
2. **Implement + review** — the agent builds; the coordinator reviews the
   diff *independently* (do not rubber-stamp a self-report) and returns a
   verdict, iterating until correct. When the coordinator runs a more capable
   model than the agent, this review is also **mentoring**: the agent owns and
   *solves* its hard design calls and brings worked solutions (with reasoning
   and rejected alternatives) for review — never a blank "which way?" — while
   still surfacing the calls that genuinely benefit from the smarter look, and
   respecting the parent's context budget. See
   [capability-tiered mentoring](agent-hierarchy.md#capability-tiered-mentoring-across-a-parentchild-model-gap).
3. **Rebase + squash + verify** — on the coordinator's signal the agent
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
   commit), ask the agent to `/save` its branch plan, then **end the
   agent's session** so it stops consuming context and polling
   while it waits on the staging cycle (which the coordinator, not the
   child agent, drives). Do this **only after the coordinator has pulled the
   agent's commits into the coordinating worktree** — never end a session
   whose only copy of the work is its own worktree state. A paused agent
   is idle, not gone.
5. **Reopen if bugs surface** — if a defect appears (e.g. a red CI job
   traced to the change), start a **new** session for that agent
   (usually `opusplan`; plain `opus` for architecture-level fixes) and give
   it instructions. Then re-pause or finalize as appropriate.
6. **Finalize** — once the staging cycle completes, the change lands on
   `main`, **CI on the landing tip is green**, **and every child issue is
   closed**. Three gates, all required:
   - **Landed + green.** Not merely "pushed" — a push with red CI is not
     finalized. Key the CI check on the workflow *named* `[CI]`, not "the latest
     run": a fast auxiliary workflow (Dependency-Graph, JavaDocs) can show green
     while `[CI]` still runs, and mistaking it for `[CI]` green has caused a
     premature finalize.
   - **All children closed.** The parent/child link is a blocking dependency
     (see [`agent-hierarchy.md`](agent-hierarchy.md#finalizing-a-parent-waits-for-its-children)):
     a parent issue cannot finalize while any child issue is open. An agent that
     took on children mid-flight extended its own finalize gate by exactly that
     much.

   Then: `/save` the branch plan again if it changed since the pause, shut down
   the agent's session, and copy the branch plan to
   `worktrees/closed/<branch>-plan.md` (the archive `spawn-issue-worktree.sh`
   checks to avoid re-spawning a closed issue). Archive the plan **before**
   `git worktree remove` (removing first loses the plan). The worktree may then
   be removed. Do **not** finalize on a red or in-progress CI run, or with an
   open child — reopen (step 5) instead.

## Context hygiene (an ongoing duty, not a lifecycle step)

The lifecycle above deals with an agent whose *work* is done (pause/finalize).
Context hygiene is different: it applies to an agent whose *work continues*
but whose **context window has grown disproportionate to the value of what it
is still doing**. An agent dragged through several rebases, multiple
validation rounds, a crash recovery, or a re-engagement can accumulate a huge
transcript while its *remaining* task is small — burning tokens (and slowing
itself) far out of proportion. Left unattended this is a silent, steady cost
across a long-lived fleet.

**Make it a periodic sweep.** On a regular cadence (e.g. whenever you survey
the fleet, or when a milestone lands), check each live agent's context size —
the TUI status bar shows a `save NNNk tokens` hint, and the session transcript
size is a proxy. Flag any agent whose context is large (rule of thumb:
hundreds of k tokens) **relative to the work it has left**. A big context on an
agent about to finish is fine — let it finish; a big context on an agent with
hours of work ahead is worth resetting.

**The intervention is a well-timed reset, not a pause.** For a flagged
still-active agent:

1. **Wait for a safe boundary.** Never reset mid-operation (mid-sync, mid-
   rebase, mid-`stack test`). Reset only at a natural seam: after a commit,
   between validation rounds, after a delivery. The agent's uncommitted /
   in-flight state must be durable first — a commit, or captured in the branch
   plan — because a reset re-reads from disk, not from the lost context.
2. **`/save` with compaction.** Have the agent `/save` its branch plan, and
   as part of that **prune no-longer-relevant material** — superseded
   approaches, resolved dead-ends, stale intermediate SHAs, verbose logs from
   earlier rounds. The plan should end up as a tight, current
   resume-from-cold brief: what shipped, what's verified, what's left, and any
   live constraints — not a running diary. A bloated plan defeats the reset.
3. **`/compact` or restart.** Then either `/compact` the session, or restart
   it (`claude-remote <slug> -m <model> --continue`, which re-reads the saved
   plan fresh). Restart gives the cleanest context; `/compact` is lighter-
   weight when the agent is mid-task-sequence and you only need headroom.
   Re-orient the restarted session with a short inbox message stating exactly
   where it is and what's next (it has the plan, but a one-line "you are here"
   saves a re-derivation).

The test for whether to intervene: *if this agent crashed right now and had
to resume purely from its branch plan, would it lose important state?* If yes,
its plan isn't current enough — fix that (step 2) regardless of whether you
then reset. If no, and its context is large with real work remaining, reset
it. The branch plan being a faithful cold-resume brief is the invariant that
makes both context hygiene and the pause/reopen steps safe.

## Periodic fleet reconciliation (no orphans)

Agents get left behind — most often an agent whose change *landed* but whose
finalize (archive plan → end session → remove worktree) never ran, because
the landing happened during a busy stretch. Left unattended, these accumulate
as live agent sessions burning resources for work that shipped days ago, and
as untracked worktrees/branches. Finalizing recent agents promptly is not
enough; you must also **periodically sweep for the ones that slipped**.

**The sweep** — for every worktree you might own:
1. Is its branch an ancestor of `origin/main` (merged)? `git merge-base
   --is-ancestor <branch> origin/main`.
2. Does it still have a live tmux/agent session? `tmux has-session`.
3. How many commits ahead of main? `git rev-list --count origin/main..<branch>`.

A worktree that is **merged + still has a live session + 0 commits ahead** is
the classic left-behind-finalize case → finalize it. But two traps:
- **0-commits-ahead does NOT mean orphaned.** An *active* agent with
  uncommitted in-tree WIP also shows 0 ahead. Check last session activity
  before concluding a merged/0-ahead worktree is finished — a recently-active
  one is working, not done.
- **Only finalize agents you own.** A coordinator must not finalize another
  coordinator's agents, intentionally-parked worktrees, or release/detached
  worktrees.

### Cross-coordinator reconciliation

When more than one coordinator (or a coordinator + the staging agent) owns
child agents, a one-sided sweep can't guarantee zero orphans — you can't
finalize what you don't own, and you can misread another owner's active agent
as an orphan. So reconcile *cooperatively*:

1. **Build the authoritative table** from the sweep above across ALL
   worktrees (merged? live? commits-ahead?), tagged by your best guess of
   owner and lifecycle state.
2. **Finalize your own** left-behinds first.
3. **Send the other coordinator ONE reconciliation message** — not a bare
   list. Structure it: (a) *your* fleet, all accounted for; (b) *their*
   fleet with your read in brackets, split into ACTIVE (do-not-touch),
   FINALIZE-CANDIDATES (merged + session up + issue resolved), and
   OTHER/non-agent (release, detached). Explicitly name the active ones so a
   sweep can't kill a live agent.
4. **Ask them to reply with dispositions** — what they finalize, what they
   *intentionally keep* (with the reason: a paused-pending-user-review
   worktree, a deferred-work branch, a dormant collector), and what is
   criterion-gated (landed but waiting on CI-green). The reply, not your
   list, is what closes the reconciliation.

The goal state: every worktree is provably one of — an active agent, a
deliberately-parked agent (with a documented reason), a finalize queued on a
stated criterion, or already finalized. Nothing is merely "still there."
Expect to correct each other: the owner of an agent always knows its true
state better than another coordinator's scan (0-ahead misreads active WIP as
done; a "leftover"-looking worktree may be a load-bearing review artifact).

## Why the coordinator holds the human-gated actions

Child agents never push to `main`, never create/close/comment on GitHub
issues, and never merge — those stay with the coordinator (and, for the actual
push to `origin/main`, the designated staging agent; see
[`branch-flow.md`](branch-flow.md)). Concentrating the human-in-the-loop
friction (auth, CI decisions, revert judgment) in one place — rather than
scattering it across every agent — lets agents run fully autonomously up
to handoff, and is why a well-run issue agent rarely needs the human directly
while staging does.

## Filing issues: mandatory parent, always user-gated

When any agent (child or coordinator) needs to file a new GitHub issue:

- **Every issue must declare a parent, except `release_*` roots.** The parent
  is a required field of the draft, alongside title and body. Answer "which
  issue is this a child of?" *before* showing the draft. This is the
  enforceable rule that prevents a repeat of the #416 subtree (filed entirely
  parentless). See [`agent-hierarchy.md`](agent-hierarchy.md#mandatory-parent-the-enforceable-rule).
- **Draft-and-show is the terminal step.** Prepare the full text including the
  parent link, surface it through the coordinator chain to the user, and file
  only on explicit approval — per CLAUDE.md hard rule 5. Never file, close,
  comment, label, or re-parent on your own initiative. Make "show the draft"
  the last step of the drafting task, not one that can be skipped when intent
  seems clear.
