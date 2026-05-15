---
description: Review user documentation and Claude-specific notes for staleness or gaps discovered during the current session, then update them concisely. End-of-session pass — and only when you've learned something this session that the docs don't yet reflect. Skip if the session was purely operational.
allowed-tools:
  - Read
  - Edit
  - Write
  - Grep
  - Glob
  - Bash(git diff *)
  - Bash(git log *)
  - Bash(git show *)
  - Bash(git status*)
  - Bash(ls *)
  - Bash(find *)
---

# End-of-session documentation pass

## When to run

The user invokes this. Don't trigger it autonomously.

Typical timing: after the substantive work of a session is committed and
about to be wrapped up, when the user wants any documentation
implications captured before the session ends.

## What counts as "user documentation"

Per the Hydra CLAUDE.md "Scope of user documentation" section:

- READMEs: top-level, demo, per-package, per-implementation
- Everything checked in under `docs/` (ignore untouched/unstaged files)
- The wiki (separate Git repo at `<hydra-root>/wiki/`)
- Code comments that document public APIs or non-obvious behavior

Also in scope for *this* command:

- `CLAUDE.md` itself
- `claude/*.md` — Claude-specific protocols and pitfalls

Out of scope: branch plan documents (`*-plan.md`), session-specific files,
generated `dist/`, tests, the lexicon (regenerated via `/lexicon`).

## Procedure

1. **Identify what was learned this session.** Look at this session's
   commits, the user's questions, and any blocked-and-resolved obstacles.
   The right kind of learning to capture is something a future Claude or
   contributor *would not have known* from reading the existing docs.

2. **Map learnings to docs.** For each learning, decide where it
   belongs:
   - **Project-wide concept or invariant** → `docs/implementation.md`
     or a relevant `docs/recipes/*.md`
   - **Claude-specific gotcha or workflow** → `claude/pitfalls.md` or
     `claude/<topic>.md`
   - **A new command or session protocol** → `CLAUDE.md` (sparingly —
     CLAUDE.md is loaded into every conversation; prefer linking out)
   - **Per-package operational detail** → that package's `README.md`

3. **Make minimal, additive edits.** Add a paragraph or a section; do
   not refactor existing prose. If a doc needs major restructure (e.g.
   splitting one doc into two), surface that to the user and let them
   decide.

4. **Stay concise.** A two-sentence pitfall entry is often the right
   size. Don't expand on background the existing docs already cover.

5. **Link new content.** If a learning lives in a recipe, add a link
   from CLAUDE.md's "Where to look up X" table when topical (rarely;
   usually the existing rows are enough).

6. **Mention major changes to the user.** Splitting a doc, removing a
   section, or changing a long-standing claim warrants a one-line
   heads-up before the edit.

## What to avoid

- Don't rewrite or "improve" docs prophylactically. The trigger is
  *something learned this session*. If nothing was learned, say so and
  stop.
- Don't add documentation about temporary state, branch-specific work,
  or in-progress investigations. Those go in the branch plan document.
- Don't expand CLAUDE.md aggressively. Every line lands in every future
  session's context.
- Don't touch the wiki without an explicit user instruction — it's a
  separate Git repo and committing requires its own confirmation.
