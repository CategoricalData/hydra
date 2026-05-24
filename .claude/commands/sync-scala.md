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

> **Caveat.** This skill populates `dist/scala/hydra-scala/` (and
> `hydra-kernel`/`hydra-pg`/`hydra-rdf` via Phase 3) only. To test against
> the cross-language Scala dists (`dist/scala/hydra-{haskell,java,python,lisp}/`),
> use the broader sync:
>
> ```bash
> bin/sync.sh --hosts scala --targets all
> ```
>
> Symptom of skipping this: `sbt test` from `packages/hydra-scala/` reports
> `Type Mismatch Error` in a generated `dist/scala/hydra-<lang>/.../*.scala`
> file whose mtime predates a recent kernel-type change. Details:
> [`claude/pitfalls.md` § sbt test from packages/hydra-scala/ needs cross-target dist trees](../../claude/pitfalls.md).
