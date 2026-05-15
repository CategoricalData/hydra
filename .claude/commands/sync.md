---
description: Run bin/sync.sh to regenerate code across the full host × target matrix (or a scoped subset). Default is all × all. Pass language names to scope to a Cartesian subset (same languages on both sides).
argument-hint: [lang1,lang2,...]
allowed-tools:
  - Bash(bin/sync.sh*)
  - Read
  - Bash(git status*)
  - Bash(git diff *)
---

# Full host × target sync

## When to run

User-invoked. Typical triggers:

- After Haskell source changes that need to propagate to all targets
- Before a release
- After resolving a kernel-types-cache-gap (see
  `claude/pitfalls.md`'s "Stale dist/haskell artifacts after
  non-baseline edits" entry)

## Procedure

If `$ARGUMENTS` is empty (no scoping list passed), full all-hosts ×
all-targets:

```bash
bin/sync.sh --hosts all --targets all
```

This is the **full matrix**. Note the asymmetry vs `/bootstrap`:
`/sync` defaults to `all`; `/bootstrap` defaults to the
haskell/java/python triad.

If `$ARGUMENTS` is a comma-separated language list, apply it to both
hosts and targets (Cartesian):

```bash
bin/sync.sh --hosts $ARGUMENTS --targets $ARGUMENTS
```

So `/sync haskell,java` runs `--hosts haskell,java --targets haskell,java`
— four combinations.

## Examples

- `/sync` — full matrix
- `/sync haskell,java,python` — bootstrapping triad (use
  `/sync-default` for the same triad via a more direct wrapper)
- `/sync haskell` — just regenerate Haskell against Haskell

## Long-running command guidance

A full `bin/sync.sh` matrix can take 30 minutes or more. Capture full
stdout+stderr to a temp file (don't pipe through `grep`/`tail` —
diagnostic context will be lost). Give the user a brief status update
every ~10 minutes.

If sync fails, investigate and fix the root cause, then re-run from
the failing step forward. Don't re-run steps that already succeeded.
Common failures are documented in `docs/troubleshooting.md` and
`claude/pitfalls.md`.
