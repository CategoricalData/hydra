---
description: Regenerate the generated module-reference pages under docs/specification/{primitives,types}/ from the current kernel by running bin/regenerate-spec.sh. Java-host (contrast /lexicon's Haskell-host regenerate-lexicon.sh). Run on demand and as part of the pre-release flow; not part of regular sync.
allowed-tools:
  - Bash(bin/regenerate-spec.sh*)
  - Bash(git status*)
  - Bash(git diff *)
  - Read
---

# Regenerate the module-reference spec pages

## When to run

User-invoked. Typical triggers:

- After kernel doc-string, signature, or provision changes to a module
  with a committed page under `docs/specification/{primitives,types}/`
- Before a release
- After landing a provisions-authoring change (#725) to a definition,
  to pick up a new normative entry on its page

The spec generator is **not** part of the normal sync pipeline. Edits
to the generated pages appear as a normal commit alongside other
generated artifacts.

## Procedure

1. Verify `dist/json` is current: kernel changes have been synced
   (`/sync-java` or `/sync` was run after the latest kernel edits).
   `bin/regenerate-spec.sh` fails loudly with a pointer to `/sync` if
   `dist/json/hydra-kernel` is missing.

2. Run:

   ```bash
   bin/regenerate-spec.sh
   ```

   Pass one or more `--module <namespace>` flags to scope regeneration
   to specific kernel modules while iterating on a single page, e.g.
   `bin/regenerate-spec.sh --module hydra.lib.lists`.

3. Inspect the diff. Each regenerated page should show the expected
   changes (new/changed doc strings, signatures, provisions). Surprises
   are usually a sign the sync wasn't run or the kernel change isn't
   where expected.

4. **Not every committed page converges under this generator** — three
   are structurally out of scope and will not match generated output
   even when everything else is current:
   - `primitives/equality.md` and `primitives/ordering.md` catalog
     primitives by type-class membership across many source files, not
     by a single module; this generator walks one module at a time.
   - `primitives/functions.md` documents a primitive module
     (`Lib/Functions.hs`) that does not exist in the kernel yet.

   These three keep their `generator not yet built` IOU header
   regardless of how current the generator is — a diff against them is
   expected, not a bug. See the page-name-mapping notes in
   `feature_723_spec_generator_markdown-plan.md` for the full mapping
   rule (some `types/*.md` pages combine a type module with its paired
   `hydra.error.*` module onto one page).

5. Once a page's generated output genuinely matches, strip its
   `generator not yet built` IOU header in the same commit as the
   regeneration that first achieves the match.

6. The regenerated pages are committed alongside the source change they
   document, or in a follow-up commit if the source already landed.
