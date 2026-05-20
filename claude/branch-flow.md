# Branch promotion flow

Hydra uses a four-tier promotion ladder: `feature → integration → staging → main`.
Each tier has its own long-lived worktree. Promotion happens by merge.

```
feature_NNN  ──→  integration  ──→  staging  ──→  main  ──→  origin/main
 (active           (passive          (active        (passive         (remote)
 sessions)         collector)        checks)        local mirror)
```

For the read/modify rules across worktrees, see CLAUDE.md ("Working with worktrees").
For git-worktree mechanics (adding, removing, cherry-picks across worktrees),
see [worktree-workflow.md](worktree-workflow.md).

## Where work happens

- **Feature worktrees** are where Claude sessions live and where conflicts are
  resolved. They are the only tier with active, ongoing work.
- **integration** is a passive collector. It typically has no Claude session
  attached. It receives merges from feature branches and forwards them to staging.
- **staging** is the only intermediate tier with an attached Claude session.
  After integration's batch arrives, the staging session runs `/sync`, `/bootstrap`,
  and any optional checks the user specifies. Only after those pass does the
  batch advance to main.
- **main** is a local stable mirror. No active session. It receives only from
  staging. The user pulls into local main first (so feature branches can pull
  from it to refresh their base), then pushes to `origin/main`.

## Conflict resolution happens in the feature worktree

When promoting a feature branch up to integration, the conflict-resolution work
belongs in the *feature* worktree, not in integration. Sequence:

1. In the feature worktree, pull `integration` into the feature branch
   (`git pull . integration` or `git merge integration`).
2. Resolve any conflicts there, using the Claude session attached to the
   feature worktree.
3. Once the feature branch is clean and merges into integration would be
   conflict-free, perform the merge into integration's worktree.

This keeps the active Claude session in the loop and keeps integration passive.
The same principle applies one level up: staging pulls from integration in the
staging worktree, with the staging Claude session resolving any conflicts.

## Cadence

- **Don't promote up the chain reflexively.** After a merge lands at one level,
  wait for the next level to drain its current batch — e.g., a `/bootstrap`
  on staging still in progress. The user signals when each promotion is due.
- Every promotion is `git merge --no-ff`. Use a commit message of the form
  `Merge branch '<source>' into <dest>. For #NNN [#MMM ...]`.
- The destination worktree is where the merge command runs. If the feature
  session needs to drive a merge into a passive worktree (integration, main),
  the user must explicitly authorize it — per the "modify only your assigned
  worktree" rule in CLAUDE.md.
- `main` is pushed to `origin/main` by the user, not by a Claude session.
