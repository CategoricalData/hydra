# Synchronizing Hydra-Python

This guide explains how to keep Hydra-Python synchronized with the source of truth in Hydra-Haskell.

## Overview

Hydra-Haskell is the bootstrapping implementation and source of truth for Hydra. When you make changes to the Hydra kernel, test suite, or eval lib in Hydra-Haskell, you need to regenerate the corresponding Python artifacts.

The synchronization process generates five categories of Python code:

| Category | Source | Target | Description |
|----------|--------|--------|-------------|
| Kernel modules | `Hydra.Sources.All.kernelModules` | `hydra-python/src/main/python/hydra/` | Core Hydra types and functions |
| Kernel sources | `Hydra.Sources.All.kernelModules` | `hydra-python/src/main/python/hydra/sources/` | Module AST as Python data (for tests) |
| Eval lib modules | `Hydra.Sources.Eval.Lib.All.evalLibModules` | `hydra-python/src/main/python/hydra/eval/` | Interpreter-level primitives |
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

## Quick Sync (Recommended)

The simplest way to synchronize is using the unified script:

```bash
cd hydra-ext
./bin/sync-python.sh
```

This script:
1. Builds all required executables
2. Generates kernel modules
3. Generates kernel sources modules
4. Generates eval lib modules
5. Generates kernel tests
6. Generates generation tests
7. Runs Python tests to verify
8. Reports new files to git add

For faster iteration during development, skip tests:

```bash
./bin/sync-python.sh --quick
```

## Manual Sync (Step by Step)

If you prefer to run steps individually, or need to regenerate only specific parts:

### Step 1: Build executables

```bash
cd hydra-ext
stack build hydra-ext:exe:update-python-kernel \
            hydra-ext:exe:update-python-kernel-sources \
            hydra-ext:exe:update-python-eval-lib \
            hydra-ext:exe:update-python-kernel-tests \
            hydra-ext:exe:update-python-generation-tests
```

### Step 2: Generate kernel modules

```bash
stack exec update-python-kernel -- +RTS -K256M -A32M -RTS
```

Or interactively in GHCi:
```bash
stack ghci hydra-ext:lib hydra:hydra-test
```
```haskell
writePython "../hydra-python/src/main/python" kernelModules kernelModules
```

### Step 3: Generate kernel sources modules

These are "source-level" representations of the kernel modules, containing the Module AST as Python data structures. They're needed for Python tests that evaluate Hydra terms referencing kernel functions.

```bash
stack exec update-python-kernel-sources -- +RTS -K256M -A32M -RTS
```

### Step 4: Generate eval lib modules

```bash
stack exec update-python-eval-lib -- +RTS -K256M -A32M -RTS
```

Or in GHCi:
```haskell
writePython "../hydra-python/src/main/python" mainModules evalLibModules
```

### Step 5: Generate kernel tests

```bash
stack exec update-python-kernel-tests -- +RTS -K256M -A32M -RTS
```

Or in GHCi:
```haskell
writePython "../hydra-python/src/gen-test/python" mainModules testModules
```

### Step 6: Generate generation tests

```bash
./bin/update-python-generation-tests.sh
```

Or using the executable directly:
```bash
stack exec update-python-generation-tests -- +RTS -K256M -A32M -RTS
```

### Step 7: Run Python tests

```bash
cd ../hydra-python
source .venv/bin/activate
PYTHONPATH=src/main/python:src/gen-test/python pytest src/gen-test/python/generation -q
```

### Step 8: Add new files to git

```bash
git add src/main/python src/gen-test/python
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
