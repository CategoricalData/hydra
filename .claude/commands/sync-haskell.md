---
description: Run heads/haskell/bin/sync-haskell.sh — Phase 1 only (DSL → JSON + Haskell kernel + stack test). The lexicon is no longer regenerated as part of sync; use /lexicon to refresh it.
allowed-tools:
  - Bash(heads/haskell/bin/sync-haskell.sh*)
  - Read
  - Bash(git status*)
---

# Phase 1 Haskell sync

## When to run

User-invoked. Typical triggers:

- Quickest path to verify Haskell-side changes after editing DSL sources
- Iteration loop during kernel development (validates JSON + Haskell
  compile + stack test without paying for downstream targets)

For full matrix propagation, use `/sync` instead.

## Procedure

```bash
heads/haskell/bin/sync-haskell.sh
```

This runs Phase 1 only:
1. DSL sources → JSON
2. JSON → Haskell kernel
3. `stack test`

The lexicon is **not** regenerated here. Use `/lexicon` on
demand when needed.

## On failure

Investigate the failing step, fix, then re-run from there forward.
Don't re-run steps that succeeded. See `docs/troubleshooting.md` and
`claude/pitfalls.md` for the common failures.
