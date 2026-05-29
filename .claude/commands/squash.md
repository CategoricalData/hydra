---
description: Squash the branch's WIP commits into focused topic commits ahead of merge. Source changes first, generated files last. Targets the merge-base with main (the fork point), not main's tip.
allowed-tools:
  - Read
  - Edit
  - Bash(git status*)
  - Bash(git log *)
  - Bash(git diff *)
  - Bash(git reset *)
  - Bash(git add *)
  - Bash(git commit *)
  - Bash(git branch *)
  - Bash(git merge-base *)
---

# Squash WIP commits into shippable topic commits

## When to run

User-invoked, near the end of a feature branch's life when the WIP
checkpoints are ready to become a clean history before merge.

## Procedure

Per the CLAUDE.md "Commit workflow" section:

1. **Find the fork point.** Use `git merge-base HEAD main` — the
   commit where the current branch diverged from main. Squashing
   resets *to the fork point*, not to main's tip. (Resetting to main's
   tip silently drops any commits main has acquired since the branch
   diverged. Verified by the `feedback_squash_reset_target.md` memory.)

2. **Inspect the current commits.** `git log --oneline <fork-point>..HEAD`.
   Show the user. Confirm the squashing plan: which commits collapse
   into which topic groups, in what order.

3. **Soft-reset to the fork point.** `git reset --soft <fork-point>`.
   Everything since the fork is now staged.

4. **Re-commit as topic groups.**
   - **Source changes first** — Sources, kernel updates, helpers,
     hand-written code in `heads/`, configuration changes.
   - **Generated files last** — `dist/json/**` and `dist/haskell/**`
     in a separate commit, or grouped with related source if the
     coupling is tight.
   - **One topic per commit.** A "topic" is one focused change a
     reviewer would read as a unit (e.g. "migrate kernel inference",
     "delete dead stubs", "regenerate dist"). A whole feature branch
     squashed into a single commit is too aggressive — a typical
     branch lands as 3–10 topic commits.
   - Each commit message should describe the *why*, not just the
     *what* — pull from the prior WIP messages and the relevant issue.

5. **Commit message format** — per CLAUDE.md:
   - **No `WIP:` prefix** — its absence is the signal that this
     commit has been squashed and is shippable. WIP is *only* for
     pre-squash interim work.
   - **120 characters or less, single line, no body.** If one line
     isn't enough to explain the change, **split into more commits**.
     Never append a multi-line body to compensate — that defeats the
     purpose of focused topic commits.
   - No `Co-Authored-By` line.
   - End with `For #<issue>` (or `Resolves #<issue>` if this branch
     closes it).

6. **Verify.** `git log --oneline <fork-point>..HEAD` again. Then
   `git diff <fork-point> HEAD --stat` to confirm the net diff
   matches what was there before the squash.

## Hard rules

- **Never use `git rebase -i`** — interactive flags are unsupported in
  this environment. Soft-reset + re-commit is the workflow.
- **Never use `git reset --hard` to discard work** during squashing.
  Soft-reset only.
- **Never force-push to main** as part of squashing. After squashing,
  push the branch (`git push --force-with-lease origin <branch>`) but
  only after explicit user confirmation.
