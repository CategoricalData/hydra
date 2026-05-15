---
description: Run bin/sync-java.sh (--hosts java --targets java). Regenerates Java kernel + coder + Java host's own outputs.
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

Use `/sync` or a broader `bin/sync.sh --hosts java --targets all`
if you need Java to emit other targets too (e.g., to populate
`dist/java/hydra-{python,scala,lisp,...}/` for the rollup compile —
see `claude/pitfalls.md`'s entry on `:hydra-java:test` needing all
coder language packages).
