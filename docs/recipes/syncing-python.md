# Synchronizing Hydra-Python

This guide explains how to keep Hydra-Python synchronized after changes to the
**Hydra kernel** (in Hydra-Haskell) or to the **Python coder DSL sources**.

## Overview

Hydra-Haskell is the bootstrapping implementation for the Hydra kernel itself.
The **Python coder DSL sources** (the `hydra.python.*` modules: syntax, language,
coder, serde, names, utils, environment, testing) are authored in Python and live
under `packages/hydra-python/src/main/python/hydra/sources/python/`. They are
the source of truth for `dist/json/hydra-python/`, exported via
`bin/generate-hydra-python-from-python.sh`.

> A legacy Haskell-DSL copy of these same Python coder modules lives at
> `packages/hydra-python/src/main/haskell/Hydra/Sources/Python/` and produces
> byte-identical output. It was kept as a backup through the 0.15 line and
> is scheduled for removal during 0.16 development. Edits to the Python
> coder should go into the Python sources.

`bin/sync.sh` runs `bin/generate-hydra-python-from-python.sh` automatically in
Phase 5 on every invocation. The native generator owns `dist/json/hydra-python/`;
Phase 1 (Haskell DSL → JSON) skips `hydra.python.*` module names by default and
only writes them on a cold-start bootstrap (when `dist/json/hydra-python/` is
empty), so in any warm state the only writer is the native generator. A diff
report against the pre-run snapshot reports the number of changed files. The
Phase 5 skip case is when the native Python host (`dist/python/hydra-python/`)
isn't built yet — i.e. `python` was not in `--hosts` of a prior sync; in that
case the JSON remains whatever Phase 1 last wrote.

When you make changes to:
- **the kernel, default-impls (`Hydra.Sources.Kernel.Lib.Defaults.*`), or test suite**
  (in Hydra-Haskell): regenerate `dist/json/hydra-kernel/`, then run
  `bin/sync-python.sh` to refresh `dist/python/`.
- **the Python coder DSL sources**: run `bin/sync-python.sh`, which now
  invokes `bin/generate-hydra-python-from-python.sh` in Phase 5. You can
  also run the native generator directly with `--compare` to verify
  byte-identical output before the full sync.

The synchronization process generates four categories of Python code:

| Category | Source | Target | Description |
|----------|--------|--------|-------------|
| Kernel modules | `Hydra.Sources.All.kernelModules` | `dist/python/hydra-kernel/src/main/python/hydra/` | Core Hydra types and functions |
| Default-impl modules | `Hydra.Sources.Kernel.Lib.Defaults.*` (per-module-name) | `dist/python/hydra-kernel/src/main/python/hydra/lib/defaults/` | Interpreter-friendly term-level reference implementations |
| Kernel tests | `Hydra.Sources.Test.All.testModules` | `dist/python/hydra-kernel/src/test/python/hydra/test/` | Test data structures |
| Generation tests | TestSuite + TestGroups | `dist/python/hydra-kernel/src/test/python/generation/` | Executable pytest tests |

## Prerequisites

Before synchronizing Python, ensure Hydra-Haskell is consistent:

```bash
cd heads/haskell

# Regenerate all Haskell artifacts
stack ghci
> writeHaskell "../../dist/haskell/hydra-kernel/src/main/haskell" kernelModules kernelModules
> :quit

# Run all Haskell tests
stack test
```

All Haskell tests must pass before proceeding.

## Quick sync (recommended)

The simplest way to synchronize the bootstrapping triad (Haskell, Java, Python)
is using the top-level convenience wrapper:

```bash
./bin/sync-default.sh          # from repo root; or --no-tests to skip tests
```

For the full all-languages × all-languages matrix:

```bash
./bin/sync.sh --hosts all --targets all
```

To synchronize only Python (host == target == python):

```bash
./bin/sync-python.sh           # or --no-tests to skip tests
```

The `sync-python.sh` wrapper invokes
`bin/sync.sh --hosts python --targets python`, which:
1. Refreshes JSON sources via Phase 1 (DSL → JSON, stack test)
2. Generates the Python coder in Haskell, then kernel + the Python coder
   in Python
3. Runs Python tests to verify (unless `--no-tests`)

For faster iteration during development, skip tests:

```bash
./bin/sync-python.sh --no-tests
```

## Manual Sync (Step by Step)

Prefer `./bin/sync-python.sh` (above) for routine work — it builds the
executable from current source before running it. The manual steps below are
for when you need to regenerate only specific parts; note that Step 1's
`stack build` is not optional, because `stack exec` (Step 2) never rebuilds on
its own and would otherwise run a stale binary.

### Step 1: Build the bootstrap executable

```bash
cd heads/haskell
stack build hydra:exe:bootstrap-from-json
```

### Step 2: Generate all Python artifacts

The `bootstrap-from-json` executable generates kernel modules, default-impl modules, coder modules, kernel tests,
and generation tests in a single invocation:

```bash
stack exec bootstrap-from-json -- --target python --include-coders --include-tests --include-gentests +RTS -K256M -A32M -RTS
```

You can omit flags to generate only a subset:
- Without `--include-coders`: skip coder modules
- Without `--include-tests`: skip kernel tests
- Without `--include-gentests`: skip generation tests

### Step 3: Run Python tests

```bash
cd ../../packages/hydra-python
source .venv/bin/activate
PYTHONPATH=../../heads/python/src/main/python:../../dist/python/hydra-kernel/src/main/python:../../dist/python/hydra-kernel/src/test/python pytest ../../heads/python/src/test/python/test_suite_runner.py ../../dist/python/hydra-kernel/src/test/python/generation -q
```

### Step 4: Add new files to git

```bash
git add heads/python/src/main/python dist/python/hydra-kernel/src/main/python dist/python/hydra-kernel/src/test/python
```

## GHCi Environment

For interactive generation, use:

```bash
cd heads/haskell
stack ghci hydra:lib hydra:hydra-test
```

This loads both hydra-haskell (for `writePython`) and hydra-test (for `testModules`).

Key module lists available:
- `kernelModules` - Core Hydra kernel
- `mainModules` - Kernel + JSON + other modules
- `evalLibModules` - Interpreter primitives
- `testModules` - All test modules

## Memory Requirements

Python generation requires significant memory. The executables use these RTS flags:
- `-K256M` - 256MB stack size
- `-A32M` - 32MB allocation area

If you encounter stack overflow, increase these values.

## Troubleshooting

### "Unknown variable: x" errors

This usually means a lambda variable wasn't properly tracked.
Check that `Coder.hs` handles both typed and untyped lambda variables.

### Generation stops on first failure

By default, the generator continues past module failures.
Check the output for "Skipping" messages to identify problematic modules.

### Test failures after sync

1. Ensure Haskell tests pass first
2. Check for missing primitive implementations in `overlay/python/hydra-kernel/src/main/python/hydra/lib/`
3. Review `docs/work/python-test-failures-analysis.md` for known issues

## Related Documentation

- [Creating a new Hydra implementation](new-implementation.md) - Full guide for new implementations
- [Adding primitive functions](adding-primitives.md) - How to add primitives across implementations
- [Extending the test suite](extending-tests.md) - Adding new tests
