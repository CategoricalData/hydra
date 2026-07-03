# Pull request

## What does this change?

A short description of the change and the motivation. Link the issue it addresses
(e.g. `For #123` / `Resolves #123`).

## How was it tested?

- [ ] `stack test` passes in `heads/haskell` (required for any kernel/DSL change)
- [ ] Affected target-language tests pass (`bin/test.sh <langs>` or the per-head test scripts)
- [ ] Regenerated code is included if DSL sources changed (`bin/sync.sh` or a scoped wrapper)

## Checklist (core expectations — see [CONTRIBUTING.md](../CONTRIBUTING.md))

- [ ] No hand-edits to generated files under `dist/` — generator or DSL fixes only
- [ ] No failing tests skipped or worked around
- [ ] Change is focused; unrelated cleanups are in separate commits
- [ ] Follows the [coding style](https://github.com/CategoricalData/hydra/wiki/Coding-style)
      (including alphabetical ordering of DSL definitions lists)
- [ ] User-facing behavior changes are documented (README, docs/, or wiki)
