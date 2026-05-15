---
description: Refresh docs/hydra-lexicon.txt from the current Haskell kernel by running bin/regenerate-lexicon.sh. The lexicon is the canonical reference for kernel types + ~180 primitive signatures and the most important LLM context document. Run on demand and as part of the pre-release flow; not part of regular sync.
allowed-tools:
  - Bash(bin/regenerate-lexicon.sh)
  - Bash(git status*)
  - Bash(git diff *)
  - Read
---

# Regenerate the kernel lexicon

## When to run

User-invoked. Typical triggers:

- After kernel signature changes (new primitive, changed primitive type)
- Before a release
- After a significant refactor that touched kernel API surface

The lexicon is **not** part of the normal sync pipeline. Edits to it
appear as a normal commit alongside other generated artifacts.

## Procedure

1. Verify the working tree is in a state where lexicon regeneration is
   meaningful: kernel changes have been synced (`/sync-haskell` or
   `/sync` was run after the latest kernel edits).

2. Run:

   ```bash
   bin/regenerate-lexicon.sh
   ```

3. Inspect the diff. `docs/hydra-lexicon.txt` should show the expected
   changes (new primitives, updated signatures, removed entries).
   Surprises are usually a sign the sync wasn't run or the kernel
   change isn't where expected.

4. The lexicon is committed alongside the source change it documents,
   or in a follow-up commit if the source already landed.
