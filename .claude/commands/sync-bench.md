---
description: Run bin/sync-bench.sh — regenerate the hydra-bench package (synthetic inference workloads) for the default haskell/java/python hosts. Pass --hosts H,... to scope. Opt-in: the default sync does NOT touch hydra-bench.
argument-hint: [--hosts <list>]
allowed-tools:
  - Bash(bin/sync-bench.sh*)
  - Read
  - Bash(git status*)
---

# Regenerate hydra-bench

## When to run

User-invoked. Typical triggers:

- After editing the synthetic inference workloads
- Before running `/inference-bench` if the workloads have changed
  (otherwise the inference-bench runner invokes sync-bench itself)

## Procedure

Run with any host-scoping arguments passed through:

```bash
bin/sync-bench.sh $ARGUMENTS
```

Default (no `$ARGUMENTS`): haskell/java/python hosts. Scoped:
`/sync-bench --hosts python` etc.

The default `/sync` / `bin/sync.sh` does NOT touch hydra-bench
— it's opt-in via this command (or via `/inference-bench`).
