---
description: Run the full maintenance pass — stale-file detection, design-violation checks, dist consistency, digest hygiene. Follow the procedure in docs/recipes/maintenance.md verbatim.
allowed-tools:
  - Read
  - Bash(*)
  - Grep
  - Glob
---

# Full maintenance pass

## When to run

User-invoked. Typical triggers:

- Periodic repo hygiene (monthly, before a release)
- After a large refactor that may have left orphan files
- When investigating a suspected design violation

## Procedure

Follow [docs/recipes/maintenance.md](../../docs/recipes/maintenance.md)
verbatim, specifically the **Full maintenance pass** section. The
recipe is authoritative; this command is a stable entry point.

High-level scope (the recipe expands each):

1. **Orphan-file detection** — files in `dist/` that no source produces
2. **Design-violation checks** — see "Checking for design violations"
   in the recipe (post-generation patches, sed in sync scripts, etc.)
3. **Dist consistency** — does `dist/` match what a fresh regen would
   produce?
4. **Digest hygiene** — per-package digests up-to-date, no kernel-types
   cache gaps
5. **Cross-host parity spot-checks** — generated outputs across heads
   compare correctly

## What this command does NOT do

- It does not fix anything autonomously. If a check fails, report the
  finding and ask the user how to proceed.
- It does not rerun sync. Sync hygiene is the user's call after
  reviewing findings.
