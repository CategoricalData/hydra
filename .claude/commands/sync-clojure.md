---
description: Run bin/sync-clojure.sh — Clojure-host Clojure sync. One of four Lisp-dialect sync entry points; they share the hydra-lisp coder under the hood.
allowed-tools:
  - Bash(bin/sync-clojure.sh*)
  - Read
  - Bash(git status*)
---

# Clojure-host Clojure sync

## Procedure

```bash
bin/sync-clojure.sh
```

Generates the Clojure kernel via the hydra-lisp coder configured for
the Clojure dialect.

This script does **not** run the Clojure test suite. To validate the
generated code, run `packages/hydra-lisp/bin/run-tests.sh clojure` or
do a `/bootstrap clojure`. (Pending #387: `/test clojure` will roll
sync + tests into one step.)
