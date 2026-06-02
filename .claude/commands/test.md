---
description: Run bin/test.sh to validate target-language test suites against the dist/ tree. Default is the bootstrapping triad (haskell,java,python). Pass language names to scope. Pre-syncs unless --no-sync is given.
argument-hint: [lang1,lang2,...] [--no-sync]
allowed-tools:
  - Bash(bin/test.sh*)
  - Read
  - Bash(git status*)
  - Bash(git diff *)
---

# Target-language test validation

## When to run

User-invoked. Typical triggers:

- After `/sync` to confirm each target's test suite passes against the
  freshly regenerated dist/.
- Before a push to main, to match what CI will run.
- Investigating a CI failure that's not reproducible from `/sync`
  alone (since `bin/sync.sh` only runs Haskell `stack test`).

Closes the asymmetry: `/sync` regenerates everything but only validates
the Haskell-side kernel (because Haskell hosts it). `/test` validates
every other target's runtime.

## Procedure

If `$ARGUMENTS` is empty (no scoping list passed), run the bootstrapping
triad:

```bash
bin/test.sh
```

Defaults to `haskell, java, python` — matches `/bootstrap`'s default,
which is **not** the same as `/sync`'s default of `all × all`. `/test
all` is a deliberate ask, typically reserved for pre-release validation.

If `$ARGUMENTS` is a comma-separated language list, scope to those:

```bash
bin/test.sh $ARGUMENTS
```

## Examples

- `/test` — bootstrapping triad
- `/test java` — Java only
- `/test java,python,scala` — three targets
- `/test lisp` — all four Lisp dialects
- `/test all` — every target
- `/test --no-sync java` — skip pre-sync, run Java tests against the
  current dist/. Symmetric to `bin/sync.sh --no-tests`. Useful when
  you've just synced and only want the test pass.

## Cache behavior

Warm trees are near-instant: each per-target `test-distribution.sh`
maintains its own `dist/<lang>/test-cache.json` keyed on generated
sources + test infrastructure, and `bin/sync.sh` skips unchanged input
phases.

## Cold-checkout precondition

Standalone `/test java` on a cold dist tree needs the python/lisp/
typescript dist trees too, because the Java rollup's
`compileHeadsExtrasJava` imports from `hydra.{python,haskell,lisp,...}.*`
across packages. The pre-sync handles this by always going through
`bin/sync.sh`; with `--no-sync`, ensure your tree is already populated.

## Long-running command guidance

`/test all` can take 30 minutes or more on a cold tree. Capture full
stdout+stderr to a temp file (don't pipe through `grep`/`tail` — the
per-target summary will be lost). Give the user a brief status update
every ~10 minutes.

If a target fails, investigate and fix the root cause, then re-run with
`/test --no-sync <lang>` for a fast re-validation. Common failures are
documented in `docs/troubleshooting.md` and `claude/pitfalls.md`.
