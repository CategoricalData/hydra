---
description: Run bin/sync-python.sh — Python-host Python sync.
allowed-tools:
  - Bash(bin/sync-python.sh*)
  - Read
  - Bash(git status*)
---

# Python-host Python sync

## Procedure

```bash
bin/sync-python.sh
```

Generates the Python kernel and coder into the Python-host workspace.

This script does **not** run the Python test suite. To validate the
generated code, run `heads/python/bin/test-distribution.sh hydra-kernel`
or do a `/bootstrap python`. (Pending #387: `/test python` will roll
sync + tests into one step.)
