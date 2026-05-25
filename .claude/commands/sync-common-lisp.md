---
description: Run bin/sync-common-lisp.sh — Common-Lisp-host Common-Lisp sync. One of four Lisp-dialect entry points; shares the hydra-lisp coder.
allowed-tools:
  - Bash(bin/sync-common-lisp.sh*)
  - Read
  - Bash(git status*)
---

# Common-Lisp-host Common-Lisp sync

## Procedure

```bash
bin/sync-common-lisp.sh
```

This script does **not** run the Common Lisp test suite. To validate
the generated code, run `packages/hydra-lisp/bin/run-tests.sh common-lisp`
or do a `/bootstrap common-lisp`. (Pending #387: `/test common-lisp`
will roll sync + tests into one step.)
