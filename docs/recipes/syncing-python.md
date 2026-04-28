# Synchronizing Hydra-Python

This guide explains how to keep Hydra-Python synchronized with the source of truth in Hydra-Haskell.

## Overview

Hydra-Haskell is the bootstrapping implementation and source of truth for Hydra.
When you make changes to the Hydra kernel, test suite, or eval lib in Hydra-Haskell,
you need to regenerate the corresponding Python artifacts.

The synchronization process generates four categories of Python code:

| Category | Source | Target | Description |
|----------|--------|--------|-------------|
| Kernel modules | `Hydra.Sources.All.kernelModules` | `dist/python/hydra-kernel/src/main/python/hydra/` | Core Hydra types and functions |
| Eval lib modules | `Hydra.Sources.Eval.Lib.All.evalLibModules` | `dist/python/hydra-kernel/src/main/python/hydra/eval/` | Interpreter-level primitives |
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

If you prefer to run steps individually, or need to regenerate only specific parts:

### Step 1: Build the bootstrap executable

```bash
cd heads/haskell
stack build hydra:exe:bootstrap-from-json
```

### Step 2: Generate all Python artifacts

The `bootstrap-from-json` executable generates kernel modules, eval lib, coder modules, kernel tests,
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
2. Check for missing primitive implementations in `heads/python/src/main/python/hydra/lib/`
3. Review `docs/work/python-test-failures-analysis.md` for known issues

## Related Documentation

- [Creating a new Hydra implementation](new-implementation.md) - Full guide for new implementations
- [Adding primitive functions](adding-primitives.md) - How to add primitives across implementations
- [Extending the test suite](extending-tests.md) - Adding new tests
