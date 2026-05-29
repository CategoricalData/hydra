---
description: Run bin/sync-typescript.sh — regenerates the TypeScript-only sync matrix (hosts × targets = typescript × typescript). Thin wrapper around bin/sync.sh. Does NOT run target tests — use /test typescript for that.
allowed-tools:
  - Bash(bin/sync-typescript.sh*)
  - Read
  - Bash(git status*)
---

# TypeScript sync

## Procedure

```bash
bin/sync-typescript.sh
```

Thin wrapper around `bin/sync.sh --hosts typescript --targets typescript`.
Extra flags (e.g. `--no-tests`) are forwarded. Runs:

- Phase 1: regenerate Haskell coder dist for `hydra-typescript`
- Phase 2: regenerate `hydra-typescript` coder Haskell dist
- Phase 3: emit `hydra-kernel` into `dist/typescript/hydra-kernel/`
  (assemble-distribution.sh copies the hand-written runtime alongside)

TypeScript is a "head bud" — Phase 4 (`host=typescript`) is skipped
because the TS runtime cannot yet host generation of coder packages in
TypeScript's own language. See issue #126.

This script does **not** run the TypeScript test suite. To validate the
generated code, run the TypeScript head's test entry point or do a
`/bootstrap typescript`. (Pending #387: `/test typescript` will roll
sync + tests into one step.)

## First-run gotcha

`bin/sync-packages.sh` does not build `hydra:exe:digest-check` itself.
On a fresh worktree (or after a major source-tree change that
invalidated the exec), the first sync will fail late at the assemble
step with "Executable named digest-check not found on path." Workaround:

```bash
cd heads/haskell && stack build hydra:exe:digest-check
```

then re-run `bin/sync-typescript.sh`.
