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

Generates the Clojure kernel and runs Clojure-side tests via the
hydra-lisp coder configured for the Clojure dialect.
