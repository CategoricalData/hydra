# Python Generation Tests Implementation

> **Status**: **COMPLETED** - Archived to done/ (Feb 2026). All generation tests pass.

This document summarizes the work done to implement Python generation tests for Hydra, similar to the existing Haskell generation tests.

## Overview

The goal was to generate pytest-style tests that verify Hydra's Python code generation produces correct, runnable code. These tests are generated from Hydra's common test suite (`hydra.test.testSuite`) and placed in `hydra-python/src/gen-test/python/generation/`.

## Current Status

- **18 test modules generated** covering chars, eithers, equality, flows, lists, literals, logic, maps, math, maybes, pairs, sets, strings, annotations, formatting, monads, reduction, and sorting
- **100% pass rate**: All 623 tests passing (matching Haskell test count exactly)
- Test generation completes in ~6 seconds
- Test execution completes in ~0.5 seconds

## Files Modified

### hydra-ext/src/main/haskell/Hydra/Ext/Staging/Python/Coder.hs

1. **External binding resolution** (lines 913-917): Added fallback to check `graphElements` when a variable is not in `typeContextTypes` and not a primitive. This fixes resolution of bindings like `hydra.monads.pure` which are term elements, not primitives.

2. **Curried lambda generation** (lines 1073-1086): Modified `makeSimpleLambda` to generate properly curried lambdas:
   ```python
   # Before (incorrect for Python):
   lambda x1, x2: f(x1, x2)

   # After (correct - supports curried calls):
   lambda x1: lambda x2: f(x1, x2)
   ```

3. **Flow unwrapping** (lines 175-183): Modified `EliminationWrap` handling to apply all remaining arguments at once instead of curried. Python's `Flow.value` is a 2-arg callable `(state, trace)`, not a curried function.

4. **Variable application for graph elements** (lines 194-216): Modified the `TermVariable` case in `applyArgs` to detect graph elements with known arity and consume the appropriate number of arguments all at once, rather than applying them one-by-one in curried fashion.

### hydra-ext/src/main/haskell/Hydra/Ext/Staging/Python/TestCodec.hs

1. **Added `cast` import** (line 293): Added `from typing import cast` to the standard imports since some generated tests use type casts.

2. **Fixed duplicate test names** (lines 224-264): Modified `generatePythonTestGroupHierarchy` and `generatePythonTestCase` to include the group path in test function names. This prevents test shadowing when multiple test groups have cases with the same name (e.g., "empty list" appears in bind, concat, filter subgroups).
   ```python
   # Before (caused shadowing - only last definition survived):
   def test_empty_list():
       ...
   def test_empty_list():  # shadows previous
       ...

   # After (unique names):
   def test_bind__empty_list():
       ...
   def test_concat__empty_list():
       ...
   ```

### hydra-haskell/src/main/haskell/Hydra/Staging/Testing/Generation/Generate.hs

1. **Added Monads module** (line 162): Added `Monads.module_` to `extraModules` so that `hydra.monads.pure`, `hydra.monads.bind`, etc. are available in the graph for test generation.

## Running the Tests

```bash
# Generate the tests
cd hydra-ext
stack exec update-python-generation-tests

# Run the tests
cd ../hydra-python
PYTHONPATH=src/main/python:src/gen-test/python pytest src/gen-test/python/generation -v
```

## Python Library Fixes Applied

The following fixes were applied to the Python library to achieve 100% test pass rate:

### hydra-python/src/main/python/hydra/lib/math.py

1. **`abs` alias**: Added `abs = abs_` alias so generated code can use `hydra.lib.math.abs`. Used `import builtins as _builtins` to avoid recursion since `abs` shadows the builtin.

2. **`rem` function**: Fixed to use truncated division (Haskell semantics) instead of floored division (Python default). Changed from `a - (a // b) * b` to `a - int(a / b) * b`.

### hydra-python/src/main/python/hydra/lib/lists.py

1. **`drop` function**: Added check for `n <= 0` to return full list (Haskell semantics). Python slicing with negative n returns elements from the end.

2. **`take` function**: Added check for `n <= 0` to return empty list (Haskell semantics). Python slicing with negative n returns elements from the end.

3. **`intercalate` function**: Added check for empty `values` list to return empty tuple instead of crashing on `values[-1]`.

### hydra-python/src/main/python/hydra/lib/strings.py

1. **`split_on` function**: Added special handling for empty delimiter. Haskell semantics: `splitOn "" "abc" == ["", "a", "b", "c"]`.

2. **`unlines` function**: Changed to add trailing newline. Haskell semantics: `unlines ["a", "b"] == "a\nb\n"`.

## Architecture Notes

### Test Generation Flow

1. `generateGenerationTestSuite` (Generate.hs) extracts test modules from the test suite
2. `modulesToGraph` creates a graph including test modules and extra modules (Formatting, Monads, Core)
3. `inferGraphTypes` performs type inference once for the entire graph
4. For each module with generation tests:
   - `generatePythonTestFile` builds namespaces and TypeContext
   - `pythonTestCodecWithContext` creates an efficient codec with pre-built context
   - `generatePythonTestGroupHierarchy` generates test functions
   - Tests are written to `generation/hydra/test/lib/test_*.py` or `generation/hydra/test/test_*.py`

### Key Design Decisions

1. **Skip casts for performance**: Test generation uses `skipCasts = True` to avoid expensive type checking during code generation. This dramatically reduces memory usage.

2. **Single type inference pass**: Type inference (`inferGraphTypes`) is performed once upfront for the entire graph, not per-module, which is critical for performance.

3. **Curried vs multi-arg functions**: Python functions are not curried by default. The coder generates curried lambdas for polymorphic functions and flattens curried applications into multi-arg calls when the target function's arity is known.

4. **Graph elements vs primitives**: The coder checks three places for variable resolution:
   - `typeContextTypes` (local bindings with inferred types)
   - `graphPrimitives` (standard library primitives)
   - `graphElements` (module bindings like `hydra.monads.pure`)

## Prior Session Context

This work continued from a prior session that was interrupted by memory exhaustion. Key fixes from that session:

1. **Memory exhaustion fix**: Added pattern match for untyped lambdas in `analyzeFunctionTerm` to prevent infinite recursion.

2. **Performance optimization**: Added `pythonEnvironmentSkipCasts` option to skip cast generation during test generation.

3. **Monads module import**: Added the import but hadn't yet added it to `extraModules`.
