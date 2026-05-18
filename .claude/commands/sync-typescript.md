---
description: Run bin/sync-typescript.sh — regenerates the hydra-typescript package's DSL sources into Haskell. Transitional until the TypeScript coder is wired into bin/sync.sh.
allowed-tools:
  - Bash(bin/sync-typescript.sh*)
  - Read
  - Bash(git status*)
---

# TypeScript sync (transitional)

## Procedure

```bash
bin/sync-typescript.sh
```

Currently delegates to `bin/sync-packages.sh --targets haskell
hydra-typescript`. This regenerates only the hydra-typescript package's
own DSL sources (Syntax/Language/Operators/Serde) into Haskell under
`dist/haskell/hydra-typescript/`.

The TypeScript coder itself does not yet exist. Once `Coder.hs` and
`writeTypeScript` land, `bin/sync-typescript.sh` will switch to the
standard matrix-tool invocation (`bin/sync.sh --hosts haskell --targets
typescript`).

See issue #126 for the broader TypeScript head plan.

## First-run gotcha

`bin/sync-packages.sh` does not build `hydra:exe:digest-check` itself.
On a fresh worktree (or after a major source-tree change that
invalidated the exec), the first sync will fail late at the assemble
step with "Executable named digest-check not found on path." Workaround:

```bash
cd heads/haskell && stack build hydra:exe:digest-check
```

then re-run `bin/sync-typescript.sh`.
