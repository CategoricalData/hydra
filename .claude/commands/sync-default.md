---
description: Run bin/sync-default.sh to sync the default java/python matrix (Haskell opt-in) — equivalent to /bootstrap's default scope but for sync (no demo dashboard).
allowed-tools:
  - Bash(bin/sync-default.sh*)
  - Read
  - Bash(git status*)
---

# Sync the default java/python matrix

## When to run

User-invoked. Typical trigger: you want to regenerate the java/python
subset of the full sync matrix without paying for the long-tail
Lisp/Scala/Go targets — or the shared Haskell build, which is opt-in
(see [docs/build-system.md §Haskell is opt-in](../../docs/build-system.md)).

## Procedure

```bash
bin/sync-default.sh
```

Equivalent to `bin/sync.sh --hosts java,python --targets java,python`.
Same scope as `/bootstrap` (the default), but without the demo dashboard.
For a change that specifically concerns Haskell-authoring source, pass
`--hosts`/`--targets` including `haskell` explicitly, or use `bin/sync.sh`
directly / `/sync-haskell`.

## Long-running command guidance

Same as `/sync`: capture full stdout+stderr, ~10-minute status
updates if running interactively, investigate and resume from the
failing step on error.
