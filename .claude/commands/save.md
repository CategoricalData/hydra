---
description: Update the current branch's plan document with session status, completed work, and any open questions. The session may terminate after this completes — treat the plan doc as a complete handoff for the next session.
allowed-tools:
  - Read
  - Edit
  - Write
  - Bash(git status*)
  - Bash(git log *)
  - Bash(git branch *)
  - Bash(ls *)
---

# Save session state to branch plan

## When to run

The user invokes this when they want session state committed to the
branch's plan document for handoff. The session may terminate
immediately after, so the plan doc must be a complete handoff — anything
not captured here is lost.

## Procedure

1. **Locate the plan document.** It lives at the worktree root, named
   after the current branch — e.g. `staging-plan.md`,
   `feature_249_java_version-plan.md`. If it does not exist, create it.

2. **Update the plan.** Record:
   - **Completed work** this session: commit SHAs, brief summary of
     each, verification status
   - **Current state**: where the work sits, what's committed vs
     uncommitted, branch position relative to `origin/main`
   - **Open questions**: anything the user hasn't decided yet
   - **Next steps**: what a fresh session should do first
   - **Cross-references**: relevant issues, wiki pages, prior plan
     iterations, recent cross-worktree messages

3. **Keep it scannable.** The next session reads this cold. Lead with a
   "Status" line. Use short paragraphs and bullet lists. Link to the
   issue tracker rather than re-explaining context.

4. **Do not commit the plan.** Plan documents are not checked in to
   Git (they're per-branch working notes).

5. **Surface anything not in the plan.** If there's important state
   that doesn't fit naturally (e.g. background tasks still running,
   stashed changes, cross-worktree messages awaiting reply), tell the
   user directly before ending the session.

## What the plan should NOT be

- A duplicate of the issue tracker — link out instead
- A change log of every tool call — only the durable result matters
- A list of TODO items for hypothetical follow-up — capture only what's
  genuinely pending
