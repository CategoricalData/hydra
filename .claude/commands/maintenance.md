---
description: Run the full maintenance pass — stale-file detection, design-violation checks, dist consistency, digest hygiene. Follow the procedure in docs/recipes/maintenance.md verbatim. Apply fixes autonomously; stage them for review before committing.
allowed-tools:
  - Read
  - Edit
  - Write
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

## How fixes are handled

- **Apply fixes autonomously where the right action is clear** —
  delete confirmed-orphan files, fix definition ordering, refresh
  out-of-date digests, etc. The point of `/maintenance` is to make
  these mechanical fixes, not just report them.
- **Pause and ask** when the fix isn't obvious or carries non-trivial
  risk (e.g. a deletion that could affect downstream consumers, a
  generator change to remove a class of stale outputs).
- **Stage fixes for review; do not commit.** Leave the working tree
  dirty so the user can review the diff before deciding what to
  commit. Summarize what was fixed and what still needs the user's
  judgment.
- **Do not rerun sync automatically.** After applying fixes that
  affect generated content, surface them; the user decides whether
  to follow up with `/sync`.
