---
description: Save current session status into the branch plan, prune the plan down to only durable, currently-relevant state, then compact the conversation and re-read the trimmed plan. A space-saving context-switch between major tasks — like /save + /compact, but it also prevents a stale, bloated plan from polluting context going forward.
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

# Save status, prune the branch plan, then compact the conversation

## When to run

The user invokes this **between major tasks**, as a context-switching measure.
The conversation is about to be compacted, so anything important that lives only in
the conversation (and not in the plan) will be lost. At the same time, the branch
plan has accumulated resolved investigation logs, superseded diagnoses, and path
dumps that are no longer relevant — bloat that would re-enter context on every plan
read and crowd out useful signal.

This command does both halves in one pass: first **save** the current session status
into the plan (so nothing important is lost across the compaction), then **prune**
the plan to its durable core, then `/compact`, then re-read the trimmed plan — so the
fresh context window starts from a plan that is both complete and minimal.

This is `/save` + `/compact`, plus a guarantee that an outdated plan does not impact
context going forward.

## Procedure

Do these steps in order. The save (step 3) must happen BEFORE the prune (step 4) and
both BEFORE `/compact` (step 6). Do not skip the re-read at the end — that is the
whole point.

1. **Locate the plan document.** It lives at the worktree root, named after the
   current branch — e.g. `staging-plan.md`, `feature_249_java_version-plan.md`.
   If it does not exist, create it.

2. **Anchor to current reality.** Capture the *current* state so the saved/trimmed
   plan is accurate, not a stale snapshot:
   - `git log -1 --oneline` (current HEAD)
   - branch position vs `origin/main` (`git rev-list --count` ahead/behind)
   - any in-flight work, running background tasks, armed monitors, or awaited user
     signals

3. **Save current status into the plan FIRST (do not prune yet).** Before deleting
   anything, write down everything from *this* session that isn't already in the plan
   and would be lost when the conversation is compacted — following `/save`'s
   discipline (the plan is a complete handoff):
   - completed work this session: commit SHAs + one-line summary + verification status
   - current state: committed vs uncommitted, branch position, any explicit pause
     point or awaited signal
   - in-flight context: running background tasks (with IDs), armed monitors (with
     task IDs), the next concrete step
   - open questions the user hasn't decided yet
   The point of doing this first is that the compaction is lossy; the plan must hold
   everything important *before* you start cutting.

4. **Now prune the plan, keeping ONLY what's durable and currently actionable:**
   - **Purpose** — what this branch is for (one short paragraph).
   - **Current state** — HEAD/CI position; committed vs uncommitted; pause point /
     awaited signal; running tasks + monitors (so they survive the compaction).
   - **Next steps** — the exact sequence a fresh session should run next.
   - **Operational reminders** — gotchas worth keeping at hand; link to memory
     files / docs rather than re-explaining.

   **Delete:** resolved investigation logs, superseded diagnoses, per-path bootstrap
   dumps, blow-by-blow change logs, and anything already landed on `main`. If a
   resolved item produced a durable lesson, that lesson belongs in a memory file
   (`/Users/josh/.claude/projects/.../memory/`), not the plan — move it there if it
   isn't already captured, then drop it from the plan.

   Steps 3 and 4 are conceptually save-then-cut; in practice you write the final lean
   plan in one Write, having first made sure every still-relevant fact from the
   session is in it.

5. **Report the reduction, then do NOT commit the plan.** Tell the user the
   before/after size (e.g. "876 → 55 lines") so the space saving is visible. Plan
   documents are not checked in to Git.

6. **Compact the conversation.** Invoke `/compact` (the built-in command) so the
   conversation history is summarized into a fresh context window.

7. **Re-read the compacted plan.** After compaction completes, immediately `Read`
   the trimmed plan document back into context. This guarantees the post-compaction
   window holds the *current* lean plan, not whatever stale version the pre-compaction
   summary may have referenced.

## Notes

- This wraps `/save`'s discipline (capture a complete handoff) around a prune
  (delete what's stale) — `/save` only records, this records *then* trims. Saving
  first is essential: `/compact` is lossy, so the plan must be complete before you cut.
- If background tasks or monitors are still running when this is invoked, they MUST
  appear in the trimmed plan's "Current state" (with their IDs) so they survive the
  compaction and the next window knows to keep watching them.
