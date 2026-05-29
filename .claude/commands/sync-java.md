---
description: Run bin/sync-java.sh (--hosts java --targets java). Regenerates Java kernel + coder + Java host's own outputs. Does NOT run target tests — use /test java for that.
allowed-tools:
  - Bash(bin/sync-java.sh*)
  - Read
  - Bash(git status*)
---

# Java-host Java sync

## Procedure

```bash
bin/sync-java.sh
```

Equivalent to `bin/sync.sh --hosts java --targets java`. Generates
the Java kernel, Java coder, and any Java-side derived sources into
`dist/java/hydra-{kernel,java,pg,rdf}/`.

This script does **not** run the Java test suite. To validate the
generated code, use `/test java` (which rolls sync + tests into one
step) or `/bootstrap java`.

> **Caveat.** This skill is narrow — `dist/java/` will contain only
> `hydra-{kernel,pg,rdf,java}/` afterward, not `dist/java/hydra-{haskell,python,scala,lisp,typescript}/`.
> If you need those (the gradle rollup's `:hydra-java:compileHeadsExtrasJava`
> does), use `bin/sync.sh --hosts java --targets all`. This is a
> general property of every host-only sync wrapper — see
> [`claude/pitfalls.md` § Scoped `bin/sync.sh --hosts X --targets X` is narrow](../../claude/pitfalls.md).
