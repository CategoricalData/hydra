# Synchronizing Hydra-Python

This guide explains how to keep Hydra-Python synchronized with the source of truth in Hydra-Haskell.

## Overview

Hydra-Haskell is the bootstrapping implementation and source of truth for Hydra. When you make changes to the Hydra kernel, test suite, or eval lib in Hydra-Haskell, you need to regenerate the corresponding Python artifacts.

The synchronization process generates four categories of Python code:

| Category | Source | Target | Description |
|----------|--------|--------|-------------|
| Kernel modules | `Hydra.Sources.All.kernelModules` | `hydra-python/src/gen-main/python/hydra/` | Core Hydra types and functions |
| Eval lib modules | `Hydra.Sources.Eval.Lib.All.evalLibModules` | `hydra-python/src/gen-main/python/hydra/eval/` | Interpreter-level primitives |
| Kernel tests | `Hydra.Sources.Test.All.testModules` | `hydra-python/src/gen-test/python/hydra/test/` | Test data structures |
| Generation tests | TestSuite + TestGroups | `hydra-python/src/gen-test/python/generation/` | Executable pytest tests |

## Prerequisites

Before synchronizing Python, ensure Hydra-Haskell is consistent:

```bash
cd hydra-haskell

# Regenerate all Haskell artifacts
stack ghci
> writeHaskell "src/gen-main/haskell" kernelModules kernelModules
> :quit

# Run all Haskell tests
stack test
```

All Haskell tests must pass before proceeding.

## Quick sync (recommended)

The simplest way to synchronize everything (Haskell, Ext, Java, and Python) is using the top-level script:

```bash
./bin/sync-all.sh              # from repo root; or --quick to skip tests
```

To synchronize only Python:

```bash
cd hydra-ext
./bin/sync-python.sh           # or --quick to skip tests
```

The `sync-python.sh` script:
1. Builds the `bootstrap-from-json` executable
2. Generates all Python artifacts (kernel modules, eval lib, coder modules, kernel tests, generation tests) from JSON
3. Runs Python tests to verify (unless `--quick`)

For faster iteration during development, skip tests:

```bash
./bin/sync-python.sh --quick
```

## Manual Sync (Step by Step)

If you prefer to run steps individually, or need to regenerate only specific parts:

### Step 1: Build the bootstrap executable

```bash
cd hydra-ext
stack build hydra-ext:exe:bootstrap-from-json
```

### Step 2: Generate all Python artifacts

The `bootstrap-from-json` executable generates kernel modules, eval lib, coder modules, kernel tests, and generation tests in a single invocation:

```bash
stack exec bootstrap-from-json -- --target python --include-coders --include-tests --include-gentests +RTS -K256M -A32M -RTS
```

You can omit flags to generate only a subset:
- Without `--include-coders`: skip coder modules
- Without `--include-tests`: skip kernel tests
- Without `--include-gentests`: skip generation tests

### Step 3: Run Python tests

```bash
cd ../hydra-python
source .venv/bin/activate
PYTHONPATH=src/main/python:src/gen-main/python:src/gen-test/python pytest src/test/python/test_suite_runner.py src/gen-test/python/generation -q
```

### Step 4: Add new files to git

```bash
git add src/main/python src/gen-main/python src/gen-test/python
```

## GHCi Environment

For interactive generation, use:

```bash
cd hydra-ext
stack ghci hydra-ext:lib hydra:hydra-test
```

This loads both hydra-ext (for `writePython`) and hydra-test (for `testModules`).

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

This usually means a lambda variable wasn't properly tracked. Check that `Coder.hs` handles both typed and untyped lambda variables.

### Generation stops on first failure

By default, the generator continues past module failures. Check the output for "Skipping" messages to identify problematic modules.

### Test failures after sync

1. Ensure Haskell tests pass first
2. Check for missing primitive implementations in `hydra-python/src/main/python/hydra/lib/`
3. Review `docs/work/python-test-failures-analysis.md` for known issues

## Related Documentation

- [Creating a new Hydra implementation](new-implementation.md) - Full guide for new implementations
- [Adding primitive functions](adding-primitives.md) - How to add primitives across implementations
- [Extending the test suite](extending-tests.md) - Adding new tests
