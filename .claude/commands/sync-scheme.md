---
description: Run bin/sync-scheme.sh — Scheme-host Scheme sync. One of four Lisp-dialect entry points; shares the hydra-lisp coder. Does NOT run target tests — use /test scheme for that.
allowed-tools:
  - Bash(bin/sync-scheme.sh*)
  - Read
  - Bash(git status*)
---

# Scheme-host Scheme sync

## Procedure

```bash
bin/sync-scheme.sh
```

This script does **not** run the Scheme test suite. To validate the
generated code, run `packages/hydra-lisp/bin/run-tests.sh scheme` or
do a `/bootstrap scheme`. (Pending #387: `/test scheme` will roll
sync + tests into one step.)
