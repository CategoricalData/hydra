# Agent handoff: the assigned agent's view

You are an agent assigned to exactly one GitHub issue, working in a dedicated
worktree. This doc is the lifecycle from *your* side of the table — the
companion to [`coordinator-workflow.md`](coordinator-workflow.md) (the same
lifecycle from the coordinator's side) and
[`agent-hierarchy.md`](agent-hierarchy.md) (the organizing model: the issue
tree, the two agent kinds, proposals, autonomy).

The current fleet runs Claude Code sessions, but nothing here assumes that:
other agent frameworks (OpenAI Codex has been used effectively with Hydra)
slot into the same role, briefing, and lifecycle. Where a mechanism is
substrate-specific (hooks, `tmux`, `claude-remote`), treat it as the current
implementation of a framework-neutral contract: read your inbox, keep your
plan-doc current, route questions up, never touch shared state without
approval.

## Your assignment

It arrives as a briefing in `claude-hydra-messages/inbox/`, seeded by
[`bin/spawn-issue-worktree.sh`](../bin/spawn-issue-worktree.sh). It names your
issue, your issue's parent, and your **coordinator** — the agent for your
issue's parent, who answers questions, reviews your work, and signals each
lifecycle step. Your branch, worktree, and session all carry the same
`<type>_<NNN>_<slug>` name.

Read only what your role needs (the briefing's scoped reading list; see
[`agent-hierarchy.md § Context minimization`](agent-hierarchy.md#context-minimization-role-appropriate-initial-context)) —
deep Hydra context for *your* issue, not the whole documentation surface.

## The lifecycle, from your side

1. **Orient.** Run the CLAUDE.md startup checklist, read your issue
   (`gh issue view <NNN>`), and write `<branch>-plan.md` at the worktree root.
   The plan-doc is your **cold-resume brief**: if your session died right now,
   the next one must be able to continue from the plan alone. Keep it that
   current for your whole life.
2. **Implement.** Commit small and often; every interim commit starts with
   `WIP:` and ends with `For #<NNN>` (one line, ≤120 chars, no body). Stay in
   scope: out-of-scope discoveries go through the proposal flow (below), not
   into your diff.
3. **Communicate up, don't block.** Route questions to your coordinator's
   inbox (see [`cross-worktree-messages.md`](cross-worktree-messages.md) —
   copy, **verify**, then archive). Never end a turn waiting on the user;
   make a best-guess call, record it in the plan-doc, and keep working. Your
   coordinator can redirect you if the guess was wrong.
4. **Discovered work → proposal, not action.** Decide file-vs-fix-inline, run
   the dependency test, and draft into `proposals/pending/` per
   [`agent-hierarchy.md § New-issue proposals`](agent-hierarchy.md#new-issue-proposals-how-they-bubble-up-to-the-user);
   forward it up and move your copy to `proposals/forwarded/`. You never file
   to GitHub yourself.
5. **Review.** Your coordinator reviews your diff independently and returns a
   verdict; iterate until correct. If the coordinator's hypothesis is wrong,
   **push back with evidence** — a challenge backed by hard data is the system
   working, not insubordination.
6. **Rebase + squash + verify (on signal).** `git fetch` first (`main` moves
   without notice), rebase onto the current tip, squash `WIP:` commits into
   focused topic commits — the issue-closing commit ends `Resolves #<NNN>`,
   subject to the provenance gate in
   [`agent-hierarchy.md § Provenance`](agent-hierarchy.md#provenance-and-completion-ownership) —
   and re-validate every host your change generates into (not just the triad).
7. **Handoff.** Message your coordinator that the branch is ready to stage.
   Landing on `origin/main` belongs to the staging agent; you never push
   there, and you never file/close/comment/label/re-parent GitHub issues on
   your own initiative.
8. **Pause and finalize.** After handoff you may be asked to `/save` and your
   session ended to save context — this is normal, not an error. Your commits
   are safe as a git ref in the shared repo; the plan-doc carries the rest.
   If a defect surfaces later, a fresh session in your worktree resumes from
   the plan. Finalization (archiving the plan to `worktrees/closed/`,
   removing the worktree) is the coordinator's act, not yours.

## Becoming a coordinator

If the user approves a proposal as *your issue's child*, you take on the
coordinator responsibility for it: read
[`coordinator-workflow.md`](coordinator-workflow.md) at that point (not
before), and expect a model escalation per
[`agent-hierarchy.md § Model escalation`](agent-hierarchy.md#model-escalation-when-an-issue-agent-takes-on-children).
Your own finalize gate now includes the child.
