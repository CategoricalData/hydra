# Worktree workflow

Day-to-day git operations work normally inside a worktree —
`git status`, `git commit`, `git push`, `git log` all behave as expected.
This page covers the mechanics that are specific to the bare-repo + worktrees layout.

For the read/modify rules, see CLAUDE.md ("Working with worktrees").
For the promotion ladder between long-lived branches (feature → integration →
staging → main), see [branch-flow.md](branch-flow.md).
For sibling messaging, see [cross-worktree-messages.md](cross-worktree-messages.md).

## One branch per worktree

Git refuses to check out the same branch in two worktrees simultaneously.
This is a feature: it prevents two Claude sessions from racing on the same branch.
If a branch is already checked out elsewhere, either work on it in that worktree
or add a new worktree for a different branch.

## Shared object store

Commits made in any worktree are immediately visible from every other worktree
(`git log` in worktree A will see a commit made in worktree B).
You only ever push or fetch from *one* worktree; the result is global.
This also means `git cherry-pick <sha>` from another branch's commits works
without any fetch step — useful when a sibling feature branch carries a fix
or capability you need now and waiting for its merge to staging would block progress.

## Adding a new worktree

From inside any existing worktree or from the bare repo:

```sh
# from hydra/worktrees/<existing>/
git worktree add ../<branch-name> <branch-name>

# from hydra/
git -C hydra/hydra.git worktree add worktrees/<branch-name> <branch-name>
```

## Removing a worktree

Use `git worktree remove <path>`, not `rm -rf`.
Manual removal leaves dangling metadata in `hydra.git/worktrees/`.
If you did remove one by hand, run `git worktree prune` to clean up.

## Don't edit files under `hydra.git/`

It is the shared object store and should only be modified by git commands themselves.
