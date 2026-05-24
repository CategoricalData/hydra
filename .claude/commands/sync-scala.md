---
description: Run bin/sync-scala.sh — Scala-host Scala sync.
allowed-tools:
  - Bash(bin/sync-scala.sh*)
  - Read
  - Bash(git status*)
---

# Scala-host Scala sync

## Procedure

```bash
bin/sync-scala.sh
```

This script does **not** run the Scala test suite. To validate the
generated code, run `heads/scala/bin/test-distribution.sh hydra-kernel`
or do a `/bootstrap scala`. (Pending #387: `/test scala` will roll
sync + tests into one step.)

> **Caveat.** This skill is narrow — `dist/scala/` will contain only
> `hydra-{kernel,pg,rdf,scala}/` afterward, not `dist/scala/hydra-{haskell,java,python,lisp}/`.
> If you need those (Scala sbt's source-set crossover does), use
> `bin/sync.sh --hosts scala --targets all`. This is a general
> property of every host-only sync wrapper — see
> [`claude/pitfalls.md` § Scoped `bin/sync.sh --hosts X --targets X` is narrow](../../claude/pitfalls.md).
