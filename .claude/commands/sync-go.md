---
description: Run bin/sync-go.sh — generates the kernel into dist/go/. Go is a "head bud" today; only hydra-kernel is targeted (no hydra-pg/hydra-rdf), and Phase 4 host=go rows are skipped. Does NOT run target tests — use /test go for that.
allowed-tools:
  - Bash(bin/sync-go.sh*)
  - Read
  - Bash(git status*)
---

# Go kernel generation

## Procedure

```bash
bin/sync-go.sh
```

Generates only `hydra-kernel` into `dist/go/`. Go's coder has known
emission bugs and the head's hand-written runtime is mostly placeholder;
Go is not yet a complete implementation and does not host the test
suite. See issue #289 for the broader Go head plan.
