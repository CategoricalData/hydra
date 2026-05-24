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
