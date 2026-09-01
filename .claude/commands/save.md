---
description: Update the current branch's plan document with session status, pruning history that is no longer relevant so the plan stays a lean, complete handoff. The session may terminate after this completes — anything not captured is lost.
allowed-tools:
  - Read
  - Edit
  - Write
  - Bash(git status*)
  - Bash(git log *)
  - Bash(git branch *)
  - Bash(git rev-list *)
  - Bash(wc *)
  - Bash(ls *)
  - Bash(grep *)
---

# Save session state to the branch plan

## When to run

The user invokes this when they want session state written to the branch's
plan document for handoff. The session may terminate immediately after, so
the plan must be a **complete** handoff — anything not captured here is lost.

Saving is always **record then prune**. A plan that only ever grows becomes
context bloat: every future session pays to read resolved investigations,
superseded diagnoses, and work already landed on `main`. The plan should read
like a current briefing, not a lab notebook.

## Procedure

1. **Locate the plan document.** At the worktree root, named after the current
   branch — e.g. `staging-plan.md`, `feature_249_java_version-plan.md`. Create
   it if absent.

2. **Anchor to current reality** so what you write is not a stale snapshot:
   `git log -1 --oneline`, position vs `origin/main` (`git rev-list --count`),
   plus any in-flight work: running background tasks (with IDs), armed monitors
   (with task IDs), awaited user signals.

3. **Record everything from this session that isn't already in the plan.** Do
   this BEFORE cutting anything — you cannot safely prune what you have not yet
   written down:
   - **Completed work**: commit SHAs, one-line summary each, verification status
   - **Current state**: committed vs uncommitted, branch position, explicit pause
     point or awaited signal
   - **In-flight context**: background tasks + monitors with IDs, the next
     concrete step
   - **Open questions**: anything the user hasn't decided
   - **Cross-references**: issues, wiki pages, recent cross-worktree messages

4. **Preserve durable lessons ELSEWHERE before pruning.** For each resolved item
   you are about to delete, ask: did this produce a lesson that outlives this
   branch? If so, write it to a memory file
   (`/Users/josh/.claude/projects/.../memory/`) or the right doc — `claude/pitfalls.md`,
   `docs/troubleshooting.md` — **first**, then delete it here. Plan documents are
   not in Git; a lesson deleted from one and recorded nowhere else is simply lost.

5. **Prune to what a cold reader needs.** Keep:
   - **Purpose** — what this branch is for (one short paragraph)
   - **Current state** — HEAD/CI position, committed vs uncommitted, pause point,
     running tasks + monitors
   - **Next steps** — the exact sequence a fresh session should run
   - **Open questions** — genuinely undecided items
   - **Operational reminders** — gotchas worth having at hand; link to memory
     files and docs rather than re-explaining them

   Delete: resolved investigation logs, superseded diagnoses (including wrong
   turns that were later corrected), per-path bootstrap dumps, blow-by-blow
   change logs, and anything already landed on `main` and released. Keep a
   one-line pointer where the outcome still matters ("0.17.6 released; see
   wiki Releases"), not the narrative.

   Write the pruned plan in a **single `Write`**, not a sequence of Edits, so the
   plan is never left half-pruned if the session is interrupted.

6. **Report the reduction.** Tell the user the before/after size (e.g.
   "617 → 34 lines"), so the saving is visible and a prune that didn't actually
   prune is obvious.

7. **Do not commit the plan.** Plan documents are per-branch working notes and
   are not checked in.

8. **Surface anything that doesn't fit the plan.** Background tasks still
   running, stashed changes, cross-worktree messages awaiting reply — tell the
   user directly before the session ends.

## What the plan should NOT be

- A duplicate of the issue tracker — link out instead
- A change log of every tool call — only the durable result matters
- A narrative of how a problem was solved — that is a memory file or a doc
- A list of TODOs for hypothetical follow-up — only what is genuinely pending

## Related

- `/save-and-compact` — this, then `/compact` the conversation, for a
  context switch between major tasks.
