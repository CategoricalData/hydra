# Agent + issue hierarchy

Hydra's autonomous development runs as a fleet of LLM agent sessions ("agents")
across multiple machines. This document defines how those agents are organized,
how that organization mirrors the GitHub issue tree, how new-issue proposals
reach the user, how each agent's initial context is scoped to its role, and the
operational realities an agent arriving cold would otherwise re-learn the
expensive way.

It is the structural companion to
[`coordinator-workflow.md`](coordinator-workflow.md) (the mechanics an issue
agent uses when it takes on the coordinator responsibility — spawning,
reviewing, finalizing children), [`branch-flow.md`](branch-flow.md) (the
promotion ladder a change travels),
[`cross-worktree-messages.md`](cross-worktree-messages.md) (the message-passing
transport every workflow here rides on — inbox/outbox, the proposal queue,
routing by hierarchy), and [`agent-handoff.md`](agent-handoff.md)
(the agent's-eye view).

> **Terminology.** We call the sessions **agents**, not "workers." There are
> two kinds: **issue agents** (assigned to one GitHub issue) and **staging
> agents** (per-machine promotion + top-level duties). "Coordinator" is **not a
> third kind** — it is a *responsibility* an issue agent takes on when its issue
> has actively-worked children: it manages those child agents in addition to its
> own issue. An issue agent with no children is just an issue agent; a
> `release_*` agent is an issue agent whose issue is a release root and which
> (almost always) coordinates children.
>
> **Framework-agnostic.** The current fleet runs Claude Code (and the artifact
> names reflect that history: `CLAUDE.md`, the `claude/` directory,
> `claude-hydra-messages/`, `claude-remote`), but nothing in this model assumes
> it. Other agent frameworks — OpenAI Codex has been used effectively with
> Hydra — fill the same roles: the issue tree, the briefing, the plan-doc, the
> inbox protocol, and the proposal queue are all plain files and git state any
> agent can read and write. Where a mechanism below is substrate-specific
> (hooks, `tmux` quirks, model names), treat it as the current implementation
> of a framework-neutral contract, not as part of the model.

## The core principle

**The agent parent-child hierarchy mirrors the GitHub issue parent-child
hierarchy.** A coordinator's scope is derivable from the issue tree, not from
tribal knowledge. If issue N has children M1, M2, then the agent for N is the
coordinator for the agents on M1 and M2 — by that fact alone, not by any
separate designation.

Crucially, **the parent/child link is a blocking-dependency edge, not an
org-chart line.** A child issue is a *dependency* of its parent: the parent
cannot finalize until the child does. This is what makes the tree shape
meaningful — it is a faithful picture of the real dependency graph — and it is
what decides parentage (see [The dependency test](#the-dependency-test)).

## Two homes for all work

Partition **all** work into exactly two categories:

1. **Issue-associated work** → handled by the agent hierarchy that mirrors the
   issue tree (below).
2. **Top-level, non-issue work** (promotion to `origin/main`, cross-machine
   coordination, orphan-issue triage, fleet reconciliation) → handled by
   **staging agents** (below).

Nothing falls outside these two homes. Historically "homeless" work (a bug with
no obvious owner) defaulted to whoever ran staging, blurring the two; the
partition gives every kind of work a principled home.

**Worked example (live, 2026-07-06).** The #416 subtree ("promote the build
system into Hydra") has 15 children, coordinated on the `feature_416` side
(e.g. `bug_551`). Everything else under the #508 (Hydra 0.17) release — issues
like `bug_548/549/550/552` and `task_557` — sits on the #508 side, currently
staging-owned with an ownership transfer to `release_508_hydra_0_17` in progress.
That transfer *is* the two-homes partition resolving in real time: issue-work
migrating from staging's ad-hoc ownership into the issue-tree hierarchy. See
[Coordination handoff](#coordination-handoff).

## The issue tree

- **`release` issue type.** Top-level release umbrellas are typed `Release` and
  their agents are named `release_NNN_*` (e.g. `release_508_hydra_0_17`), so
  top-level status is legible from the branch name exactly like
  `bug_`/`feature_`/`task_`.
- **Every issue declares a parent, EXCEPT `release_*` issues** — the roots.
- **Agent-created issues MUST declare a parent at creation.** This is the
  enforceable rule that prevents a repeat of the #416 subtree (filed entirely
  parentless). See [Mandatory parent](#mandatory-parent-the-enforceable-rule).
- Humans may occasionally create a non-release issue without a parent. A staging
  duty (the [orphan-issue scan](#staging-agents)) finds these and prompts the
  user to assign a parent. **There should be no parentless issues other than
  `release_*`.**

The mechanism is GitHub's native sub-issue (parent/child) links, not a
convention in issue bodies.

**One issue → one active agent branch.** A given issue number has at most one
*active* agent branch at a time. Branch names encode the type
(`<type>_<NNN>_<slug>`), so a single issue must not sprout two live branches with
different type prefixes or slugs (observed in the wild: an issue carrying both a
`feature_NNN_*` and a `task_NNN_*` branch — one is stale). When re-scoping an
issue's work, rename/retire the old branch rather than spawning a second; the
spawn guard's closed-plan check (keyed on `<type>_<NNN>_`) assumes one branch per
issue.

## Agent kinds and the coordinator responsibility

There are exactly **two kinds of agent**, keyed to the [two homes for all
work](#two-homes-for-all-work):

### Issue agent
Assigned to exactly one GitHub issue. Implements its issue, responds to review,
rebases/squashes/verifies, and hands off. Every agent in the issue tree is an
issue agent — including release agents.

**The coordinator responsibility.** An issue agent whose issue has
actively-worked children *additionally* manages those children — this is what we
mean by "coordinator." It is a **responsibility layered on top of being an issue
agent, not a separate kind**:

- An issue agent with no children is just an issue agent.
- The moment its issue spawns active sub-issues, it *takes on* the coordinator
  responsibility for exactly those direct children: spawn → review →
  rebase/squash/verify → pause → (reopen if needed) → finalize (full lifecycle in
  [`coordinator-workflow.md`](coordinator-workflow.md)). It still owns its own
  issue's implementation the whole time.
- When those children finalize, the responsibility lapses and it is a plain issue
  agent again.

Because the responsibility attaches to the issue *position* (does my issue have
active children?), a coordinator's scope is derivable from the issue tree — no
separate designation, no session class to provision.

A **`release_NNN_*` agent** is an issue agent whose issue is a release root (no
parent). It almost always carries the coordinator responsibility for its
subtree, runs the most capable model, and holds the longest-horizon context.

### Staging agent
Per machine; not an issue agent and never carries the coordinator
responsibility. Owns promotion + top-level non-issue duties. See
[Staging agents](#staging-agents) below.

### Coordination handoff
Coordination for an issue subtree can start out held by a staging agent — the
default home for anything without a principled owner — and later transfer to a
dedicated coordinator once one is spawned. This is exactly the transition #557
codifies: staging sheds its ad-hoc issue-coordination as the issue-tree
hierarchy fills in. When it happens:

- **The new coordinator is not instantly up to speed.** A freshly-spawned
  coordinator inherits the *position* immediately but not the *context* — the
  outgoing staging agent holds the live state (which children are in flight,
  what's verified, what's blocked). Treat handoff as a briefing, not a switch:
  the incoming coordinator reads the staging agent's plan-doc and inbox, and
  until it has absorbed that, **the staging agent remains the authoritative
  source of current state** and questions route to it.
- **The children's parent pointer moves with the coordination.** Once the
  coordinator owns the subtree, its children report to the coordinator's inbox,
  not staging's — but staging still owns their eventual landing to `main` (the
  promotion half). A child under handoff may legitimately have *two*
  correspondents: its coordinator for review/lifecycle, staging for landing.
- **Record the handoff in both plan-docs** so a cold resume on either side knows
  who currently owns what. An un-recorded handoff is how "who owns bug_NNN?"
  becomes ambiguous.

## New-issue proposals: how they bubble up to the user

The agent that discovers issue-worthy work has the most context to draft it, but
is often *not* the one who should file it or spawn its agent — and the user must
see and approve every proposal without becoming a bottleneck. The lifecycle
below separates the three actions (draft / file / spawn) and guarantees no
proposal is lost.

### First decide: file an issue, or fix it here?

Before anything else, decide whether the discovered work is even a *new issue* at
all, or something to just fix inline in the current worktree. The discriminator
is **scope, size, and coupling** — not dependency (dependency decides parentage
*after* you've decided to file).

**Fix it here, no new issue, when it is all of:**
- **In scope** — a reasonable reviewer would expect this fix as part of the
  current change, not be surprised to see it.
- **Small and self-contained** — it doesn't balloon the diff or the finalize
  timeline.
- **Coupled to your change** — your work caused it, or can't be correct without
  it; splitting it out would create an artificial two-PR dependency.
  (E.g. you rename a function and a caller two files over needs updating — just
  fix it.)

**File a new issue when any of:**
- **Out of scope** — a reviewer would be surprised to see it in this PR. This is
  the strongest signal, and it is the existing spawn-briefing scope guard
  ("out-of-scope discoveries go to Findings, not this PR").
- **Large or open-ended** — needs its own design, review, and validation; folding
  it in pushes the diff past reviewability and drags your finalize.
- **Independently valuable / separable** — it stands alone and someone else could
  own it; an issue makes it visible and assignable rather than buried.
- **Touches shared surface** others depend on (a kernel type, a primitive, a
  build invariant) — worth its own issue for traceability even if small.
  (E.g. mid-docs-fix you notice the Java coder emits the wrong Coder shape — out
  of scope, needs its own validation → file it. This is #552's origin story.)

**When genuinely unsure, the default is: inline only if it is *both* clearly
in-scope *and* clearly small; otherwise file.** The costs are asymmetric —
wrongly filing a trivial fix is mild overhead; wrongly inlining an out-of-scope
or large fix yields an unreviewable, scope-creeping PR with the fix buried where
nobody can track it.

**The two are not mutually exclusive in time.** A legitimate pattern is to inline
a *minimal, clearly-labelled* stopgap to unblock yourself now **and** file an
issue for the proper fix (e.g. a migration shim inline, with a follow-up issue
for the real solution). Don't let filing an issue block a safe stopgap; don't let
an inline stopgap leave the deeper problem unrecorded.

Once you've decided it *is* a new issue, the dependency test sets its parent.

### The dependency test

This is what decides parentage. When an agent discovers issue-worthy work, it
applies one factual test:
**does closing this new issue block *my* issue from closing?**

- **Yes → it is my child.** The dependency is real; I become its parent (and its
  coordinator), and my own finalize gate now includes it. I draft it with
  `parent = me`.
- **No → it is not my child.** I still draft it (I have the most context), but I
  propose the parent whose closure it actually gates — the relevant issue, or a
  `release_*` root if it's free-floating — and hand the draft up for that parent
  to own.

**Default to non-blocking when unsure.** Under-claiming is cheaply reversible
(re-parent later — the [orphan scan](#mandatory-parent-the-enforceable-rule)
surfaces mis-parented work); over-claiming inflates your finalize gate and can
stall a release on a tangential issue. The tree's shape is therefore as flat as
the dependencies are and as deep as they must be — never deep for structure's
own sake.

### The lifecycle (draft → bubble → digest → disposition → file)

1. **Draft at the leaf.** The discovering agent writes the full issue text
   (title, body, `parent` per the dependency test) as a **file**, not a
   throwaway message: `claude-hydra-messages/proposals/pending/<id>.md`. A file
   survives context loss, session death, and check-in gaps; a chat line does
   not. It leaves `pending/` only when the user dispositions it.
2. **Bubble up the coordinator chain, verbatim.** The draft text travels
   *unchanged*. Each coordinator **annotates a recommendation** — file / decline
   / merge-into-#N / defer — with its reasoning, and forwards it up. **A
   coordinator never drops or rewrites a proposal.** It filters by *recommending*,
   not by deleting: a probable duplicate is forwarded flagged "probable dup of
   #N, recommend decline," so the user still sees it. The user sees **every
   distinct proposal**.
3. **Digest to the user (periodic, with urgent override).** Proposals are
   batched into a digest presented at a coordinator↔user check-in — the user
   checks in with coordinators frequently, so the proposal travels to where the
   user is already looking. **Urgent / release-blocking / red-main** proposals
   bypass the batch and interrupt out-of-band immediately (attention-marker).
4. **User dispositions.** Approve / decline / re-parent. The proposal file moves
   to `proposals/approved/` or `proposals/declined/` with the verdict recorded.
5. **File on approval only.** Filing happens only after explicit user approval
   (CLAUDE.md hard rule 5). The **filer** is whoever holds the conversation
   (usually the top coordinator) — it need not be the drafter; the draft is a
   complete, parent-stamped artifact any agent can file faithfully, so a paused
   or finished leaf never blocks filing.
6. **Spawn the child agent = the issue-tree parent.** Spawning is a coordination
   act, owned by the coordinator for the new issue's parent. If the proposer
   claimed the issue as its own child (dependency test = yes), the proposer *is*
   that parent and spawns it. If not, the proposer is usually a *sibling* of the
   new issue and does **not** spawn it — the real parent does.

### No proposal is ever lost in the noise (hard requirement)

A proposal is not "handled" until the user has dispositioned it. Three
mechanisms guarantee it is impossible to miss:

- **It is a file in `proposals/pending/`**, not a transcript line. State machine:
  `pending → approved | declined`. Nothing else clears it.
- **A standing per-prompt banner.** [`bin/claude-hooks/proposals-hook.sh`](../bin/claude-hooks/proposals-hook.sh)
  runs on every `UserPromptSubmit` (like the inbox hook) and surfaces
  *"⚠ N issue proposal(s) awaiting approval (oldest: Nd)"* whenever
  `proposals/pending/` is non-empty. It re-surfaces **every turn until the queue
  drains** — a standing banner tied to queue depth, not a one-shot notification.
- **Age/urgency escalation.** A proposal sitting undispositioned past a threshold
  (N check-ins / M hours) is promoted from the digest to an attention-marker in
  `~/.cache/claude-attention/` (the channel the user already watches); urgent
  ones write the marker immediately. The reminder gets *louder* with age and
  never silently expires.

**The pending queue is single-truth: it lives only in the worktree that holds
the disposition point — the top coordinator** (where the user checks in). A
leaf/non-top agent that drafts a proposal:

1. writes it into its *own* `proposals/pending/<id>.md`,
2. sends it up the coordinator chain (ordinary message transport, copying into
   the coordinator's `proposals/pending/`), and
3. **immediately moves its own copy to `proposals/forwarded/`** — so its own
   pending queue does not nag for a proposal whose disposition it does not own.

Only the top coordinator's `pending/` accumulates awaiting-user proposals, and
only there does the banner fire. A coordinator that is *itself* a child of a
higher coordinator forwards likewise, until the proposal reaches the top of the
subtree the user checks in with. See
[`cross-worktree-messages.md § Issue proposals`](cross-worktree-messages.md#issue-proposals-a-distinct-channel)
for the transport mechanics (filenames, copy-before-archive, crash recovery).
The banner always shows on the holding coordinator's surface; the
attention-marker fires on age/urgency escalation.

## Model selection by agent role

The model an agent runs is part of its role definition, not an incidental spawn
parameter. `spawn-issue-worktree.sh`'s `SPAWN_MODEL` is the mechanism; this
table governs how the spawning parent sets it. (The names below are the current
Claude lineup; the durable content is the role → capability-tier mapping. An
agent on a different framework maps the same roles onto that framework's
strongest / balanced / plan-then-execute tiers.)

| Agent role | Model | Why |
|---|---|---|
| **Release agent** (`release_NNN_*`) | **Fable** (most capable available) | Root of an issue subtree; longest-horizon context; every downstream agent inherits the consequences of its coordination quality. |
| **High-stakes coordinator** (large / long-running / architecturally consequential subtree, e.g. the #416-class effort) | **Fable** | "High-stakes" is decided by the spawning parent or the user. |
| **Simple issue agent** (single well-scoped bug/task, implement-and-hand-off) | **`opusplan`** | Bulk of wall time is mechanical sync/regen/verify; Opus stays in the loop for planning/triage via mode-switching. |
| **Staging agent** (per-machine promotion + top-level duties) | **Opus** | Sustained cross-machine reasoning and merge judgment. |
| **Agent needing sustained planning/analysis** beyond the simple profile | **Opus** | At the discretion of the spawning parent or the user. |

### Model escalation when an issue agent takes on children

An issue agent's responsibilities can grow mid-life: one running `opusplan` that
discovers a *true dependency* (dependency test = yes) has just taken on the
coordinator responsibility, and coordination is exactly the sustained-planning
work `opusplan` is under-provisioned for. Handle it as an explicit lifecycle
event, riding the
[context-hygiene reset](coordinator-workflow.md#context-hygiene-an-ongoing-duty-not-a-lifecycle-step)
that already exists:

- **Trigger:** the agent's proposal is approved *as its own child* (it becomes a
  parent). That approval is a clean, safe boundary — not mid-operation.
- **Action:** the coordinator relaying the approval has the agent `/save` its
  plan-doc (now also a coordination brief for the new child) and restart
  (`claude-remote <slug> -m <model> --continue`) at the class-appropriate model.
- **Target model:** default **`opusplan` → Opus** on becoming a parent; escalate
  to **Fable** only if the new subtree is high-stakes — the same "high-stakes is
  the parent's or user's call" rule as the spawn-time table. One small dependency
  makes an Opus coordinator, not a Fable one.

## Context minimization: role-appropriate initial context

Each agent role should start with **only the context its task needs** — not all
of CLAUDE.md. Loading everything into every agent bloats context, slows the
agent, and costs tokens across a long-lived fleet. The startup reading is scoped
by role:

| Role | Needs at startup | Does *not* need |
|---|---|---|
| **Issue agent** (no children yet) | Deep Hydra context for *its* issue (the relevant `docs/` recipes, lexicon, package READMEs); the agent lifecycle ([`agent-handoff.md`](agent-handoff.md)); the proposal lifecycle + dependency test from this doc | Staging internals — the promotion loop mechanics, cross-machine push protocol, WIP-gate details |
| **Issue agent with the coordinator responsibility** (has children) | The above, plus the hierarchy + coordinator lifecycle ([`coordinator-workflow.md`](coordinator-workflow.md)) and this doc's proposal/finalize model. This reading is layered on when it takes on children — not front-loaded on a childless issue agent | Deep Hydra internals beyond what its own issue demands |
| **Staging agent** | The promotion loop + branch-flow ([`branch-flow.md`](branch-flow.md)), cross-machine coordination (below), staging's non-issue duties, and *awareness* of the coordinator workflow (enough to hand off correctly) | The full depth of Hydra kernel/DSL knowledge an issue agent needs — it promotes and coordinates, it does not implement kernel changes |

Practically, this means **the spawn briefing points each role at its scoped
reading list, not at "read all of CLAUDE.md."** CLAUDE.md remains the always-loaded
orientation for a human or a general session; a role-spawned agent gets a
narrower manifest seeded in its briefing (see the seeded assignment in
[`spawn-issue-worktree.sh`](../bin/spawn-issue-worktree.sh)). The governing
principle: *an agent reads what it must to do its job well, and no more.*

## Mandatory parent (the enforceable rule)

No agent — leaf, coordinator, release, or staging — creates a GitHub issue
without declaring a parent (except a `release_*` root). Because issue filing is
itself user-gated (below), the parent is part of the draft the agent shows the
user; the [dependency test](#the-dependency-test) answers "which issue is this a
child of?" as a required field of every issue draft, alongside title and body.

The staging orphan-issue scan is the backstop:
[`bin/scan-orphan-issues.sh`](../bin/scan-orphan-issues.sh) periodically lists
parentless non-release open issues so any that slipped through (or were filed by
a human) get surfaced to the user for parenting. It is read-only — it never
re-parents on its own.

## Issue filing is always user-gated

No agent — at any layer of the hierarchy — files, closes, comments on, labels,
or re-parents GitHub issues on its own initiative. The invariant is
**draft-and-show**: prepare the full text (including the parent link), surface it
through the coordinator chain to the user, and act only on explicit approval.
This inherits directly from CLAUDE.md hard rule 5 and applies without exception
to every layer. "Draft the issue" means *show me*, not *file it*.

To make the slip structurally harder, make "show the draft" the **terminal step**
of any issue-drafting task — not a step that can be skipped when intent seems
clear. (This issue, #557, was itself filed one approval short of that standard;
the rule exists to prevent that class of slip.)

## The autonomy dial

The default agent baseline is conservative — ask before most outward actions.
That is safe but makes the user a bottleneck: they end up re-typing grants like
"send messages freely until this lands" or "work autonomously, I'm afk 6h" every
session. The **autonomy dial** replaces those ad-hoc prompts with a standing
setting the user sets once and changes cheaply. Three levels — **low / medium /
high** — deliberately few, so it is effortless to reason about ("how much
control do I want right now?").

| | **Low** (supervised) | **Medium** (coordinated — default) | **High** (autonomous — "I'm afk") |
|---|---|---|---|
| **Messaging** | ask per send | send freely within task scope | full |
| **Pause / reset** | ask | self-manage | self-manage + auto-reset idle/wasteful agents |
| **Issue filing** | draft-and-show, ask | draft-and-show, ask | draft-and-**queue** (don't ask, don't file) |
| **`Resolves #` squash** (= close) | ask always | **provenance rule** (below) | provenance rule, relaxed for user-owned per a pre-declared rule |
| **Push to `origin/main`** (staging only) | autonomous¹ | autonomous¹ | autonomous¹ |

¹ **Push is autonomous at every level.** Only staging agents push (one per
machine), and they do so frequently as normal operation — the push does **not**
block on the user unless the user has given the *specific* standing direction
"confirm pushes with me." That toggle is orthogonal to the dial. The mandatory
pre-push invariants always hold regardless of level: WIP-scan clean,
`[CI]`-by-name gate, fetch-merge-before-push, tree-consistency.

**Staging is exempt from the messaging row.** A staging agent sends coordination
messages freely — and every agent responds to it freely — at *every* dial level,
including *low*. This is a standing, dial-independent authority (staging is the
fleet's coordination hub); see
[branch-flow.md § Staging's non-issue duties](branch-flow.md#stagings-non-issue-duties).
It covers coordination traffic only, not GitHub writes or other outward actions.

Two asymmetries are load-bearing:

- **Filing never rides the dial up to "just do it."** Even High only relaxes
  filing to *draft-and-queue* (a proposal sitting in `proposals/pending/`, not a
  GitHub write). Auto-*filing* happens only under a narrow explicit rule the user
  pre-declares (e.g. "auto-file confirmed dup-closes"), never open-endedly. This
  is what protects against the recurring "an issue was filed I hadn't approved"
  surprise: the messaging/pausing knobs crank up aggressively, the
  GitHub-mutation knob barely moves.
- **Availability is a first-class input.** "afk until 06:00Z" is a declaration
  the whole fleet reads, so an agent knows whether the user is even reachable
  before choosing block-vs-queue-and-proceed. When the user is afk, "blocked on
  user" should become "queue it and keep working" — the generalization of the
  afk-instruction.

**Mechanism.** The user sets the level in one place per machine — a plain
`autonomy.md` at the **worktrees parent directory** (`../autonomy.md` from any
worktree root; outside every git checkout, so it is shared by all worktrees on
the machine and never committed) — instead of re-prompting N panes. Recognized
fields, one per line (everything else is free-form notes):

```
level: low | medium | high
available-until: 2026-07-07T06:00Z    (optional; UTC)
auto-file-rules: <narrow pre-declared rules, or none>   (optional)
```

[`bin/claude-hooks/autonomy-hook.sh`](../bin/claude-hooks/autonomy-hook.sh)
(wired as a `UserPromptSubmit` hook alongside the inbox and proposals hooks)
re-reads the file every turn, so an edit propagates to the whole fleet at each
session's next turn with no re-prompting. **A missing file means `medium`** —
the default — and the hook stays silent; the banner appears only when there is
something non-default to know (level low/high, an availability window, or
pre-declared rules). A stale `available-until` is flagged rather than silently
trusted. On a multi-machine fleet the file is per-machine: set it on each
machine you are dialing, or announce the change on the staging coordination
channel. The autonomy level *is* the generalization of the scoped "coordinate
freely until X lands" grant.

## Provenance and completion ownership

Who owns an issue's *completion* — who decides it is done — is determined by
**origination**: whose idea the issue was, not whose hand filed it.

- **User-originated** → **user-owned.** The user conceived it. This covers both
  the user filing it directly *and* the user discussing it with an agent and
  asking the agent to draft/file it — in both cases the user owns "done."
- **Agent-originated** → **agent-owned.** An agent discovered the work via the
  [dependency test](#the-dependency-test) and proposed it through the
  `proposals/` flow; the user's involvement was *approving* the proposal, not
  conceiving it.

**The marker lives on the GitHub issue, so provenance is durable fleet-wide** —
it does not depend on any machine's local proposal files or plan-docs surviving,
which matters because a proposal filed on one machine may be picked up by an
agent on another. The marker is the **`agent-proposed` label**, applied **if and
only if** the issue was filed as the disposition of an agent's
`proposals/approved/` entry:

| Path | Originator | Filed by | `agent-proposed` label | Completion |
|---|---|---|---|---|
| User files it directly | user | user | none | user-owned |
| User asks an agent to draft/file it | **user** | agent | **none** | **user-owned** |
| Agent proposes via `proposals/` → user approves → filed | agent | agent | **yes** | agent-owned |

The middle row is the gray area, resolved cleanly: a user-requested,
agent-drafted filing did **not** come up the proposal queue, so it gets **no
label** and is user-owned — identical to the user filing it by hand. The label is
triggered by the `proposals/approved/` path alone.

**Absence of the marker means user-owned** — the safe default, requiring no
action, and consistent with the "default to more user control when unsure" bias.
Because the label is applied only at approved-filing time (and applying a label
is itself a user-gated action), the marker is trustworthy: an agent cannot
self-apply it to a user-originated issue.

This drives the `Resolves #` squash gate (the [autonomy dial](#the-autonomy-dial)
row):

- **`agent-proposed` present → agent-owned:** the responsible agent may make the
  `Resolves #NNN` squash autonomously (subject to the autonomy level).
- **label absent → user-owned:** the `Resolves #NNN` squash is user-gated — the
  user owns the definition of done.

**Squash-with-`Resolves #` *is* the close; it is one act, not two.** The GitHub
issue does not actually go closed until the change lands on `main` — but that
land/push/auto-close tail runs **independently of user review** (staging pushes
autonomously; GitHub auto-closes on the merged `Resolves #`). So the only place a
human gate ever sits is on *producing the `Resolves #` squash*; everything
downstream is mechanical. Provenance decides whether that one gate is active.

## Finalizing: a parent waits for its children

Because the parent/child link is a blocking-dependency edge, **a parent issue
cannot be finalized until all of its child issues are closed.** This extends the
finalize gate in [`coordinator-workflow.md`](coordinator-workflow.md#agent-lifecycle):
finalize keys on landed-to-main + `[CI]` green **and** all children closed.

This makes the tree self-enforcing: an agent that discovers a true dependency
mid-flight is extending *its own* finalize gate, and cannot close itself while
pretending the dependency does not exist. **Worked example:**
`release_508_hydra_0_17` cannot finalize Hydra 0.17 until its release-blocking
children (#551, #552) close — the plan-doc's "0.17-blocking vs. deferrable"
triage is precisely this rule in action, with deferrable children re-parented to
#509 (0.18) rather than blocking the release.

## Staging agents

**One staging agent per machine.** Multiple machines work in parallel, so there
are in general multiple staging agents all promoting to the same `origin/main`.
This makes the fetch-merge-before-push protocol and cross-writer coordination
load-bearing, not optional (see
[Cross-machine staging coordination](#cross-machine-staging-coordination)).

A staging agent owns two halves:

1. **Promotion** — the pull → sync → test → bootstrap → push → monitor-CI loop.
   Documented in full in [`branch-flow.md § Staging workflow`](branch-flow.md#staging-workflow).
2. **Top-level non-issue duties** — everything not associated with a particular
   GitHub issue:
   - **Orphan-issue scan.** Find non-`release_*` issues with no parent and
     prompt the user to assign one. (Draft-and-show — never re-parent without
     approval.)
   - **Cross-machine merge-queue coordination** (see below).
   - **Periodic fleet reconciliation.** Orphan-agent and orphan-issue sweeps
     run per machine; results are merged across machines. A single staging
     agent sees only its own worktrees, so reconciliation is inherently
     cross-machine. Pitfall: a worktree showing "merged + 0 commits ahead" can
     be an *active* agent with uncommitted WIP — only the owning machine knows,
     so **never finalize another machine's agent.**

Staging is **not** an issue coordinator. Issue-work goes to the issue-tree
hierarchy; top-level non-issue work stays with staging. This separation is the
point of the [two-homes partition](#two-homes-for-all-work).

## Cross-machine staging coordination

Because there is one staging agent per machine and all push to the same
`origin/main`, staging coordination is a genuine distributed-systems problem.
The observed failure mode: two machines' staging both wrote to main and the
second write surfaced only by accident (a fast-forward onto an unfamiliar tip).
Design against "found out by accident."

1. **`origin/main` is the only shared truth; treat it as contended.** Mandatory
   before any push: `git fetch`, merge (or rebase) `origin/main` into the local
   staging tip, **re-validate the combined tree**, then push. Never push a tip
   computed against a stale fetch.
2. **A push-announcement channel.** Before dispatching a fix for a red-CI
   regression OR pushing a batch, drop a one-line "diagnosing / fixing /
   pushing X" note where the other staging agents reliably see it. Convergent
   double-work (two machines fixing the same red-CI bug) happened this week; this
   prevents it. **Channel (adopted 2026-07-06): the `coordination` branch of the wiki
   repository** — `STATE.md` (durable, edit-in-place) + `LOG.md` (append-only
   claims/events); a claim is a commit, so push-rejection gives atomic
   compare-and-swap semantics; the branch is periodically deleted/recreated so
   churn never bloats the wiki. See the rendered "Fleet coordination" wiki
   page. (A pinned coordination issue was considered and rejected: unbounded
   comment growth, weaker claim atomicity, and — decisive — main-repo events
   including issue comments echo to the community Discord, so coordination
   churn there would spam human developers. Wiki changes are not echoed;
   never migrate this channel to a Discord-echoed surface.) The mechanism matters less than the invariant:
   *no staging agent starts a red-CI fix or a push without first checking
   whether another machine already has it in flight.*
3. **Serialize pushes; parallelize everything else.** Validation (sync, test,
   bootstrap) is expensive and machine-local — let all machines do it
   concurrently. Only the *push* must be serialized against `origin/main`. A
   lightweight advisory "push lock" (claim via the announcement channel, hold
   only for fetch-merge-validate-push, release immediately) prevents two
   machines racing a merge onto main. The push window is minutes, not the hours
   of validation.
4. **Ownership of a red main is shared, with a tie-break.** Whichever staging
   agent notices a red `[CI]` first claims the fix via the announcement
   channel; others defer. Simultaneous claims resolve by a deterministic
   tie-break (lowest machine id / earliest claim timestamp). The non-owner still
   helps by reproducing/verifying (the multi-layer cross-check is valuable) but
   only one dispatches the fix.
5. **Reconciliation is cross-machine.** Each staging agent reports its fleet's
   disposition; a designated one (or the release coordinator) collates so
   nothing on any machine is left behind. **Worked example (2026-07-06):** a
   cross-coordinator ownership reconciliation between the `feature_416` and
   `#508` sides resolved to no orphans and no double-ownership, keyed on a
   **tree-consistency rule** — every active branch maps to exactly one issue and
   one owner, and the GitHub parent tree is made to match. See the cooperative
   reconciliation flow in [`coordinator-workflow.md`](coordinator-workflow.md).

## Operational realities a cold agent lacks

Hard-won, non-obvious, and not otherwise written down. An agent arriving cold
does not know these. (Scope by role per
[Context minimization](#context-minimization-role-appropriate-initial-context):
the substrate and coordination-hygiene notes apply to everyone; the DSL/build
notes are for issue agents doing host-generating work.)

### On the substrate (how sessions actually run)
- Agents currently run as `tmux` sessions hosting `claude-remote`, spawned by
  [`bin/spawn-issue-worktree.sh`](../bin/spawn-issue-worktree.sh). (A different
  agent framework would substitute its own launcher; the worktree + inbox
  contract is what matters.) You drive another session by writing to its
  `claude-hydra-messages/inbox/` **and/or** `tmux send-keys`.
- **`tmux send-keys` has real quirks.** Input can lag 10s+; verify a keystroke
  landed by watching the target's transcript mtime, not the pane. A draft
  sitting in an `/rc` session's input box (machine-generated, or a remote/mobile
  draft) will NOT submit via a local Enter — clear with `C-u`, retype with
  `send-keys -l`, then Enter. Treat suggestion-drafts as hints, never as
  authorization.
- **Attention markers** appear under `~/.cache/claude-attention/` (written by
  the notification hook, cleared by the stop hook). An agent can *stall* (idle,
  waiting on a shell-completion notification that already fired) — distinguish
  stall from work by checking for a live process in its worktree cwd, not by the
  "N shell" TUI indicator (which goes stale).

### On coordination hygiene
- **Always `git fetch` and re-read `origin/main` before quoting any base SHA or
  issuing a rebase signal.** Main moves without notice — from this machine's
  staging AND from other machines'. A remembered tip is a bug.
- **Cross-worktree message sends normally need per-send user permission**, but a
  scoped autonomous grant (e.g. "coordinate freely until #416 lands") lifts that
  within the stated scope. Know your scope. **Exception: a staging agent sends
  freely at all times, and every agent responds to a staging agent freely** — a
  standing, dial-independent authority for coordination traffic (see
  [branch-flow.md § Staging's non-issue duties](branch-flow.md#stagings-non-issue-duties)).
- **An agent's branch commits are safe as a git ref in the shared bare repo**
  independent of its tmux session — so pausing an agent (ending its session to
  save context) loses nothing *provided the coordinator has the commits*. The
  finalize precondition "pull the agent's commits first" exists for the case
  where the only copy is uncommitted worktree state.
- **The `[CI]` workflow is the gate, by name.** Each push triggers several
  workflows (Dependency-Graph, JavaDocs, `[CI]`); `gh run list -L 1` returns
  whichever sorts first — often a fast auxiliary one showing green while `[CI]`
  still runs. This has bitten twice: a **Dependency-Graph green mistaken for CI
  green**, causing a premature finalize. Key the gate on the workflow *named*
  `[CI]`, not on "the latest run." Main `[CI]` takes ~3h15m end-to-end (a single
  Haskell job gates 8 language jobs); do not flag a run "stuck" before ~3.5h.

### On why review is non-negotiable
- The worst defects are *invisible to every test suite by construction*: a
  `.gitignore` pattern silently untracked a module family's kernel JSON (on disk
  everywhere, absent from commits — only a fresh clone would fail); a per-package
  digest cache-hit read only one package's digest, so a *different* package's
  test change silently skipped regeneration. Neither is catchable by "did the
  tests pass"; both were caught by a coordinator diffing commit contents against
  the working tree. **Independent review is the safety net for the class of bug
  testing structurally cannot see.**
- **A good agent pushes back with evidence, and the coordinator must let it.**
  Twice this week an agent's counter-evidence overturned the coordinator's wrong
  hypothesis, ending at the true root cause. An agent challenging a coordinator's
  call with hard data is the system working, not insubordination.

### On DSL/build specifics that bite host-generating work (issue agents)
- **"Triad green" (haskell/java/python) is NOT sufficient for anything that
  generates into all hosts.** The flat-namespace Lisps (Common Lisp, Emacs Lisp,
  Scheme) need generated files AND hand-maintained loader-list entries the triad
  never exercises; a triad-only handoff for a host-generating module has reddened
  `main`. Require full-9-host validation for host-generating modules.
- **A new `hydra.lib` module must be registered in ~7 scattered places** (Haskell
  driver libSubs, per-host bootstraps, Java coder alias, each host's
  `Libraries.*`, the two Common-Lisp load lists, the Emacs loader lists). Miss
  one and it compiles but fails at that host's test runtime only. (#533 aims to
  make this translingual.)
- **The published-host seam:** a newly-generated kernel module/dependency is
  invisible to drivers running in *published* mode (they consume the last
  published artifact, which predates it) until a release ships — hence
  `hostOverrides: local` / `--local-host` for the dev cycle.

## What the past week proved (evidence for this model)

Kept because it justifies the rules above and is easy to forget under pressure.

- **Independent adversarial review before landing** caught real pre-merge
  defects (inverted characterization tests, the `.gitignore` untrack near-miss,
  the Scheme recursive-copy divergence) — several invisible to every test suite.
- **Multi-layer cross-checking** (leaf → coordinator → staging) corrected
  cascading errors, each layer fixing the one above; the bug got *more* correctly
  diagnosed at each hop.
- **Durable resume state (branch plans)** survived two context-exhaustion events
  with zero lost work — the plan-doc is a faithful cold-resume brief.
- **Agent lifecycle discipline** + **per-machine staging owning the push**
  concentrated human-in-the-loop friction in one place and let agents run
  autonomously to handoff.
- **Cooperative reconciliation** between coordinators cleared orphaned worktrees
  neither side could clear alone, keyed on the tree-consistency rule.
