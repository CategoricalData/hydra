---
description: Run bin/sync-default.sh to sync the haskell/java/python bootstrapping triad — equivalent to /bootstrap's default scope but for sync (no demo dashboard).
allowed-tools:
  - Bash(bin/sync-default.sh*)
  - Read
  - Bash(git status*)
---

# Sync the haskell/java/python triad

## When to run

User-invoked. Typical trigger: you want to regenerate the
bootstrapping-triad subset of the full sync matrix without paying for
the long-tail Lisp/Scala/Go targets.

## Procedure

```bash
bin/sync-default.sh
```

Equivalent to `bin/sync.sh --hosts haskell,java,python --targets
haskell,java,python`. Same scope as `/bootstrap` (the default),
but without the demo dashboard.

## Long-running command guidance

Same as `/sync`: capture full stdout+stderr, ~10-minute status
updates if running interactively, investigate and resume from the
failing step on error.
