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
- `main` is pushed to `origin/main` by the staging session as part of its
  workflow loop (see [Staging workflow](#staging-workflow) below), once `/sync`
  and `/bootstrap` are green — but the session confirms with the user before
  each `origin/main` push, since it is outward-facing and hard to reverse.
  Feature and integration sessions never push to `origin/main`.

## Staging workflow

The staging worktree runs one repeating cycle. (This is the *staging tier's*
loop. A feature worktree may be directed by the user to run the same loop —
e.g. a long-lived feature branch driving its own sync → bootstrap → push
cadence — but by default this belongs to staging.)

Each iteration:

1. **Pull in new work.** Merge the local `integration` branch into staging
   (`git merge integration` from the staging worktree). When the user directs,
   also pull a remote GCE-based branch they name. Resolve any conflicts here,
   in the staging worktree.
2. **`/sync`** — the default full host × target sync.
3. **`/bootstrap`** — the default triad (haskell/java/python) bootstrap demo.
4. **On failure in step 2 or 3:** attempt a fix. If the correct fix is clear,
   apply it and re-run the failing step and everything after it (per the
   shorthand-commands "resume from the failing step" rule in CLAUDE.md). If the
   fix is not clear, reach out to the user before proceeding. Repeat the failing
   step until green.
5. **Push to `origin/main`** once `/sync` and `/bootstrap` are both green.
   Confirm with the user before the push (see the cadence note above).
6. **Monitor the CI build** after the push.
7. **Handle CI fallout:**
   - For a clear, simple fix, fold it into the next cycle.
   - For a complicated or non-obvious issue, surface it to the user and draft a
     GitHub issue — **do not file it without explicit approval** (per the
     "Never file a GitHub issue …" hard rule in CLAUDE.md).

### Cycle cadence

- The next cycle may begin as soon as the push to `origin/main` succeeds; it
  does not wait for CI to go green.
- When a CI issue surfaces mid-cycle, decide case-by-case whether to **interrupt**
  the running `/sync` or `/bootstrap` to fix it now, or let the current cycle
  finish and fix it in the next one. **If in doubt, ask the user.**
- This is the staging tier's own loop; promotion *into* staging still follows
  the cadence and conflict rules above (resolve conflicts in the source worktree).
