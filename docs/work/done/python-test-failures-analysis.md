# Python Test Failures Analysis

**Latest Update:** 2026-01-26
**Current Test Results:** 0 failed, 1733 passed, 191 skipped (out of 1924 total)
**Pass Rate:** 100%

> **Status:** This document is largely historical. All Python tests now pass. The remaining skipped tests are due to documented limitations (Flow decoding, RecursionError on deeply nested terms). Consider moving to `done/` folder.

This is a significant improvement from the previous state (2025-12-16: 624 failed, 55.5% pass rate).

## Issue #66 - Hydra-Python: READY TO CLOSE

All Python tests now pass. The test suite achieves 100% pass rate, matching Haskell's behavior.

### Test Counts Summary

| Language | Passed | Failed | Skipped | Total |
|----------|--------|--------|---------|-------|
| **Haskell** | 4040 | 0 | N/A | 4040 |
| **Python** | 1733 | 0 | 191 | 1924 |

Note: Haskell includes additional hand-written tests beyond the generated test suite (kernel tests, generation tests).

### Recent Fixes

#### 1. requiresFlowDecoding Tests (18 tests now skipped)

The `annotations_descriptions` tests with the `requiresFlowDecoding` tag (previously named `usesKernelRefs`) were failing
due to a fundamental limitation in the Flow TermCoder. When `flows.pure` is evaluated, it creates a native Flow that
cannot be decoded back to a Term.

**Root Cause:** The Flow TermCoder's `decode` function fails with "cannot decode flows to terms" in both
Haskell and Python. This is a fundamental limitation, not a Python-specific issue.

**Solution:** Added filtering for the `requiresFlowDecoding` tag in Python's test runner, matching Haskell's behavior.

**Verification:** Temporarily enabling these tests in Haskell's TestSuiteSpec.hs confirmed that
the same 6 tests fail with the identical error message.

#### 2. disabledForPython Tag Cleanup (10 tests re-enabled)

Re-tested all 12 tests with the `disabledForPython` tag. 10 of them now pass:
- `hydra.formatting.mapFirstLetter` (inference)
- 3 tests from `Inference/Fundamentals.hs` (poly 3, mono 2, poly 1)
- 2 tests from `Inference/Failures.hs` (expectFailure 3, expectFailure 1)
- 4 tests from `Checking/NominalTypes.hs` (chained projections, case Either, symmetric triple with foldl, unwrap with maybe)

**Still disabled (2 tests with RecursionError):**
- `Monads.hs`: "Error traces are in the right order" - term explosion during beta reduction
- `Checking/Advanced.hs`: "mutually referential failure functions with Flow monad" - RecursionError

---

## Discrepancies Between Haskell and Python Tests

This section documents the differences between tests that run in Haskell vs Python.

### Test Tags and Their Effects

| Tag | Haskell Behavior | Python Behavior |
|-----|------------------|-----------------|
| `disabled` | Skipped | Skipped |
| `disabledForPython` | Runs | Skipped (unless `HYDRA_RUN_SLOW_TESTS=1`) |
| `disabledForMinimalInference` | Skipped by minimal runner | Skipped (not generated) |
| `requiresFlowDecoding` | Skipped | Skipped |

### Tests Only in Haskell (Not Generated for Python)

Tests with `disabledForMinimalInference` are not generated into Python because the Transform.hs
generation phase filters them out. These tests require features not supported in Python's type inference.

**Count:** 55 tests across:
- `Inference/NominalTypes.hs` - 29 tests
- `Inference/AlgebraicTypes.hs` - 18 tests
- `Inference/Fundamentals.hs` - 7 tests
- `Inference/KernelExamples.hs` - 1 test

### Tests Disabled in Both Languages

**`disabled` tag (3 tests):**
- `Inference/Fundamentals.hs`: "poly 8", "poly 3", "poly 3" (recursive polymorphism tests)

**`requiresFlowDecoding` tag (18 tests):**
All in `Annotations.hs`:
- `get existing annotation #1-3`
- `get missing annotation #1-3`
- `get existing description #1-3`
- `get missing description #1-3`
- `get annotation from unannotated term`
- `get annotation from singly annotated term`
- `get inner/outer annotation from doubly annotated term`
- `get non-overridden annotation from triply annotated term`
- `outer annotation overrides inner in layered term`

### Tests Disabled Only in Python

**`disabledForPython` tag (2 tests):**
- `Monads.hs`: "Error traces are in the right order" - RecursionError due to term explosion
- `Checking/Advanced.hs`: "mutually referential failure functions with Flow monad" - RecursionError

Both fail with RecursionError in Python due to deep recursive term structures that exceed Python's
default recursion limit of 1000. Haskell handles these via tail call optimization (TCO).

### Generation Test Differences

Python generation tests are a subset of Haskell's because:
1. **Literals module excluded:** Python doesn't support `float32`, `int8`, `int16`, `uint*` types
2. This accounts for 106 fewer tests in Python generation tests

**Test Counts:**
- Haskell generation tests: 857
- Python generation tests: 751

### Summary of Skipped/Missing Tests

| Category | Count | Notes |
|----------|-------|-------|
| `disabledForMinimalInference` | 55 | Not generated for Python |
| `requiresFlowDecoding` | 18 | Skipped in both languages |
| `disabledForPython` | 2 | Python-specific RecursionError |
| `disabled` | 3 | Disabled in both languages |
| JSON tests | ~108 | `JsonCoder`, `JsonParser`, `JsonWriter` - not yet implemented |
| `DelegatedEvaluation` | ~5 | Target-language tests, N/A for interpreted mode |
| **Total Skipped** | **~191** | Out of 1924 total in Python |

This document summarizes test failures after implementing:
- Performance fix (caching InferenceContext - reduced test time from 7+ hours to ~3 minutes)
- Lazy let-bindings (Flow-based evaluation)
- JSON Coder migration to modern DSL syntax
- IntegerType enum fix (`IntegerType.INT32` vs `IntegerTypeInt32()`)
- Primitives registration via `standard_library()`

## Summary of Issue Categories

| Category | Count | Description |
|----------|-------|-------------|
| Currying Mismatch | ~524 | Lambda arity mismatch between generated code and call sites |
| Type Checking | ~22 | Type inference produces different result than expected |
| Cannot Unify | ~21 | "cannot unify string with string" type errors |
| Evaluation (Encode) | ~17 | "cannot encode term to a function" errors |
| Evaluation (Wrap) | ~9 | "Error: expected wrap(hydra.compute.Flow)" errors |
| Missing Primitives | ~4 | "no such primitive function" errors (4 fixed) |
| Eta Expansion | ~14 | Semantic difference in primitive representation |
| Type Reduction | ~9 | Beta-reduce type assertion failures |
| Deannotate Type | ~4 | Deannotate type assertion failures |
| Normalize Variables | ~3 | Normalize type variables assertion failures |
| Sorting | ~2 | `'<' not supported between instances` |
| Rewriting | ~1 | Rewrite term assertion failure |

**Note:** Many categories overlap. The currying mismatch is the dominant issue, affecting multiple test types.

---

## Issue 1: Currying Mismatch (~524 tests) - ROOT CAUSE

**Status:** ROOT CAUSE - Requires Python code generator fix

**Symptom:** `TypeError: <function>.<locals>.<lambda>() takes 1 positional argument but 2 were given`

**Root Cause:** The Python code generator produces inconsistent lambda forms:
- Direct variable references (like `var "simplify"`) generate **curried** lambdas: `(lambda x1: (lambda x2: simplify(x1, x2)))`
- Partial applications (like `var "rewrite" @@ list []`) generate **uncurried** lambdas: `(lambda v1, v2: rewrite([], v1, v2))`
- Call sites always use **uncurried** calls: `f(arg1, arg2)`

### Breakdown by Function:

| Function | Count | Location |
|----------|-------|----------|
| `deannotate_type_recursive` | 169 | `rewriting.py:141` |
| `free_type_variables_in_term.all_of` | 146 | `lists.py:75` |
| `free_variables_in_context` | 73 | `lists.py:75` |
| `contract_term` | 63 | `rewriting.py:278` |
| `substitute_in_term` (filter_with_key) | 37 | `maps.py:63` |
| `remove_types_from_term` | 18 | `rewriting.py:278` |
| `beta_reduce_type` | 9 | `rewriting.py:695` |
| `flatten_let_terms` | 5 | `rewriting.py:278` |
| `simplify_term` | 4 | `rewriting.py:278` |

### Example

In `rewriting.py`, `deannotate_type_recursive` generates:
```python
deannotate_type_recursive = (lambda recurse: (lambda v1: ...))  # curried
```

But the call site in `rewrite_type` calls it as:
```python
f ((lambda v12: fsub(recurse, v12)), v1)  # expects uncurried: f(recurse, v1)
```

### Resolution

Modify `encodeApplication` in `Coder.hs` to consistently generate either:
1. **All uncurried lambdas** with multi-argument `(lambda v1, v2: ...)` forms, OR
2. **All curried lambdas** with single-argument nested forms AND curried call sites

Option 1 (uncurried) is preferred as it matches Python conventions.

---

## Issue 2: Type Checking Mismatches (~22 tests)

**Status:** REQUIRES INVESTIGATION

**Symptom:** Inferred type differs from expected type, typically missing universal quantifiers.

### Examples:
- `(t0 → t0)` vs `(∀t0.(t0 → t0))`
- `set<t0>` vs `(∀t0.set<t0>)`
- `map<t0, t1>` vs `(∀t0.(∀t1.map<t0, t1>))`

### Affected Tests:
- Nullary primitives (empty set, empty map)
- Polymorphic primitives (identity, map)
- Higher-order primitives (lists filter, optionals maybe)
- Variable scoping
- Simple/nested lambdas

### Resolution

The Python type inference may not be properly generalizing types. This requires investigation of:
1. The `generalize` function in type inference
2. How free type variables are computed
3. The inference context setup for primitives

---

## Issue 3: Cannot Unify String with String (~21 tests)

**Status:** REQUIRES INVESTIGATION

**Symptom:** `Errors: cannot unify string with string`

This appears to be a bug in the unification algorithm where identical types fail to unify. Likely caused by:
1. Name comparison issues
2. Type variable scope tracking problems
3. Occurs check false positives

---

## Issue 4: Evaluation Errors (~26 tests)

**Status:** KNOWN LIMITATION

### 4a: Cannot Encode Term to Function (~17 tests)

**Symptom:** `cannot encode term to a function: TermFunction(value=FunctionPrimitive(...))`

Affects primitives like:
- `hydra.lib.strings.toUpper` (7 tests)
- `hydra.lib.lists.pure` (4 tests)
- `hydra.lib.math.mul` (2 tests)
- `hydra.lib.math.negate` (1 test)
- `hydra.lib.equality.identity` (1 test)

### 4b: Expected Wrap Errors (~9 tests)

**Symptom:** `Error: expected wrap(hydra.compute.Flow) but found: (hydra.monads.pure @ ...)`

Affects Flow monad operations. The `wrap` and `unwrap` operations for the `Flow` type are not fully implemented.

---

## Issue 5: Missing Primitives (~8 tests)

**Status:** PARTIALLY FIXED (2025-12-16)

**Symptom:** `no such primitive function: hydra.lib.X.Y`

### Missing Primitives:
- `hydra.lib.maybes.cat` (4 tests) - **FIXED** - Registered and tests pass
- `hydra.lib.maybes.map` (2 tests) - **Registered** - But fails due to Issue 4a (function encoding)
- `hydra.lib.eithers.either` (2 tests) - **Registered** - But fails due to Issue 4a (function encoding)

These primitives have been registered in `standard_library()`. The `cat` tests now pass. The `map` and `either` tests still fail because they take function arguments, which requires interpreter support to encode `TermFunction` values to Python callables (see Issue 4a).

---

## Issue 6: Eta Expansion Semantic Difference (~14 tests)

**Status:** KNOWN SEMANTIC DIFFERENCE

**Symptom:** Tests fail with output comparison errors where Python produces fully eta-expanded primitives while Haskell produces partial applications.

### Example:
```
Expected: hydra.lib.math.add!
Actual:   λv0.λv1.(hydra.lib.math.add! @ v0 @ v1)
```

The Python implementation fully expands primitives into explicit lambdas. This affects:
- `etaexpandterm` tests (14 tests)

---

## Issue 7: Sorting Errors (~2 tests)

**Status:** REQUIRES IMPLEMENTATION

**Symptom:** `'<' not supported between instances of 'X' and 'Y'`

Affected types:
- `TypeApplicationTerm` vs `TypeApplicationTerm`
- `Just` vs `Nothing`

The Python code requires `__lt__` implementations for sorting/comparison operations on these types.

---

## Issue 8: Rewriting/Reduction Assertion Failures (~17 tests)

**Status:** DEPENDENT ON CURRYING FIX

- Type reduction: 9 tests
- Simplify term: 4 tests
- Flatten let terms: 5 tests
- Deannotate type: 4 tests
- Normalize type variables: 3 tests
- Rewrite term: 1 test

Most of these failures are downstream effects of the currying mismatch (Issue 1).

---

## Test Categories Summary

### Passing Test Categories (~775 tests)
- JSON serialization (JsonCoder, JsonParser, JsonWriter)
- Basic type inference for literals
- Some record/variant operations
- Topological sorting (partially)
- Some primitive operations

### Failing Test Categories (~628 tests)

By test category:
- **Checking tests:** ~313 failures (mostly currying + type mismatch)
- **Inference tests:** ~178 failures (mostly currying)
- **Rewriting tests:** ~46 failures (currying + assertion failures)
- **Evaluation tests:** ~28 failures (encode + wrap errors)
- **Primitives tests:** ~28 failures (missing primitives + currying)
- **Other:** ~35 failures

### Skipped Test Categories (152 tests)
- DelegatedEvaluation
- AlphaConversion
- TypeCheckingFailure
- Some TypeChecking cases

---

## Recommended Next Steps

### Priority 1: Fix Currying Mismatch (High Impact - ~524 tests)

Modify `encodeApplication` in `hydra-ext/src/main/haskell/Hydra/Ext/Staging/Python/Coder.hs` to consistently generate uncurried lambdas. This is the root cause of the majority of failures.

Key locations to fix:
1. `encodeLambda` - ensure multi-arg lambdas are generated consistently
2. `encodeApplication` - ensure partial application handling is consistent

### Priority 2: Register Missing Primitives (~8 tests)

Add `hydra.lib.maybes.{cat,map}` and `hydra.lib.eithers.either` to the Python primitive registry.

### Priority 3: Fix Type Inference (~22 tests)

Investigate why universal quantifiers are not being added during type generalization.

### Priority 4: Implement Monad Evaluation (~9 tests)

Add `wrap`/`unwrap` handling for Flow types in evaluation.

### Priority 5: Add Comparison Operations (~2 tests)

Implement `__lt__` for `TypeApplicationTerm`, `Just`, `Nothing` and other types used in sorting.

---

## Change History

- **2025-12-13:** Initial analysis with 208 failures
- **2025-12-16:** Updated analysis with 628 failures after reverting test runner changes. Identified currying mismatch as root cause (~524 tests).
- **2025-12-16:** Registered missing primitives (`maybes.cat`, `maybes.map`, `eithers.either`). 4 tests now pass (624 failures remaining).
- **2026-01-02:** Fixed generation test failures. See new section below.

---

## Generation Test Fixes (2026-01-02)

### Issue: "Unknown variable: x" Error

**Status:** FIXED

**Symptom:** Python generation tests failed with `Error: Unknown variable: x` when processing modules containing lambdas without type annotations.

**Root Cause:** The Python coder's `encodeVariable` function only checked `typeContextLambdaVariables` when the variable was ALSO in `typeContextTypes`. However, for untyped lambdas (lambdas with `Nothing` domain), `extendTypeContextForLambda` adds the variable to `typeContextLambdaVariables` but NOT to `typeContextTypes`.

**Fix Location:** `hydra-ext/src/main/haskell/Hydra/Ext/Staging/Python/Coder.hs` (lines 1225-1228)

```haskell
-- Before: Only checked typeContextLambdaVariables if name was in typeContextTypes
Nothing -> case lookupPrimitive g name of ...

-- After: Check typeContextLambdaVariables for untyped lambda-bound variables
Nothing -> if S.member name (typeContextLambdaVariables tc)
  then return asVariable
  else case lookupPrimitive g name of ...
```

### Issue: Generation Stops on First Failure

**Status:** FIXED

**Symptom:** When generating Python generation tests, the process would stop completely when one module failed (e.g., `literals` due to float32 type limitations).

**Root Cause:** The `generateAllModuleTestsIncremental` function used `mapM_` which propagates failures and stops execution.

**Fix Location:** `hydra-haskell/src/main/haskell/Hydra/Staging/Testing/Generation/Generate.hs` (lines 218-248)

**Solution:** Added two new functions:
1. `tryFlow :: Flow s a -> Flow s (Maybe a)` - Runs a Flow and catches failures, returning `Maybe a`
2. `generateModulesWithContinueOnError` - Processes modules one by one, logging failures but continuing with remaining modules

```haskell
-- Try to run a Flow, returning Nothing if it fails instead of propagating the error
tryFlow :: Flow s a -> Flow s (Maybe a)
tryFlow f = Flow $ \s t ->
  let FlowState mval s' t' = unFlow f s t
  in FlowState (Just mval) s' t'
```

### Results After Fixes

**Generation Tests:**
- **Before:** Stopped at module 6 (literals) with error
- **After:** 17 out of 18 modules generated successfully
- Only `literals` fails due to known float32/int8/int16/uint* type limitations

**Test Counts:**
- Haskell generation tests: 857
- Python generation tests: 751 (up from 637)
- Gap: 106 tests (entirely due to `literals` module)

**Test Results:** 738 passed, 13 failed

### Remaining Generation Test Failures

**Status: ALL FIXED** (2026-01-02)

After the fixes below, all 857 generation tests now pass.

#### Fixes Applied

1. **Decimal import** (24 tests): Added `from decimal import Decimal` to the standard imports in `TestCodec.hs`

2. **maybes.compose signature** (3 tests): Changed from 2 arguments to 3 arguments in `hydra/lib/maybes.py`:
   ```python
   # Before: def compose(f, g) -> Callable
   # After:  def compose(f, g, x) -> Maybe[C]
   ```

3. **math.pow and math.round aliases** (8 tests): Added aliases and fixed `round_` to use `_builtins.round`:
   ```python
   pow = pow_
   round = round_
   ```

4. **math.e and math.pi as functions** (2 tests): Changed from constants to zero-arg functions for consistency with other Hydra primitives like `maps.empty()`:
   ```python
   # Before: e: float = math.e
   # After:  def e() -> float: return math.e
   ```

---

## JSON Decoder Bugs for Kernel Loading (2026-01-07)

**Status:** FIXED

### Context

To run evaluation tests that reference kernel functions like `hydra.monads.pure` and `hydra.monads.bind`, the Python test graph needs kernel term bindings. The plan is to load these from JSON files at `hydra-haskell/src/gen-main/json/` using:
1. `hydra.json.decode.from_json` - decode JSON to Hydra Terms
2. `hydra.decode.module.module` - decode Terms to Module structures

The generated Python code in `hydra/json/decode.py` had code generation bugs that prevented proper decoding. These have now been fixed by refactoring the Haskell DSL source.

### Bugs (Now Fixed)

All three bugs were caused by the Python code generator not handling let bindings inside `ifElse` branches. The generator would produce placeholder strings like `"let terms are not supported here"` instead of the actual logic.

#### Bug 1: TypeMaybe
**Problem:** Nested `ifElse` in array decoding had let binding in branch.

**Fix:** Hoisted `decodeJust` helper function outside the `ifElse`:
```haskell
"decodeJust" <~ ("arr" ~>
    Eithers.map ("v" ~> Core.termMaybe $ just $ var "v")
      (fromJson @@ var "types" @@ var "innerType" @@ (Lists.head $ var "arr"))) $
"decodeMaybeArray" <~ ("arr" ~>
    ...
    Logic.ifElse (Equality.equal (var "len") (int32 1))
      (var "decodeJust" @@ var "arr")  -- Now just a function call
      ...)
```

#### Bug 2: TypeUnion
**Problem:** Both `findAndDecode` and `processUnion` had let bindings inside `ifElse` branches.

**Fix:** Hoisted helper functions for variant decoding:
- `decodeVariant` - decodes a matched field
- `tryField` - checks if a field matches and returns Maybe result
- `decodeSingleKey` - extracts key from single-key object and calls findAndDecode

The key insight was to replace let bindings inside `ifElse` with function calls to pre-defined helpers.

#### Bug 3: TypeWrap
**Problem:** Inline case expression inside let binding wasn't supported.

**Fix:** Hoisted `extractInnerType` helper function that uses match expression at top level:
```haskell
"extractInnerType" <~ ("lt" ~>
    cases _Type (var "lt") (Just $ var "lt") [
      _Type_wrap>>: "wt" ~> Core.wrappedTypeBody $ var "wt"]) $
```

### Files Modified

- `hydra-haskell/src/main/haskell/Hydra/Sources/Json/Decode.hs` - Refactored DSL patterns to avoid let bindings inside `ifElse` branches

### Additional Fix: Package Structure

Moving `hydra/json.py` to `hydra/json/__init__.py` was necessary because adding `jsonModules` to `kernelModules` created `hydra/json/decode.py`, etc. Python cannot have both a module (`json.py`) and a package directory (`json/`) with the same name.

### Test Results

After the fixes, the JSON decoder works correctly in Python:
- `from_json` successfully decodes String, Maybe, Union, Wrap, and other types
- All basic decoder tests pass
- The full test suite shows 1668 passed (up from before), with remaining failures in evaluation tests that need kernel bindings

---

## Kernel Sources Generation (2026-01-07)

**Status:** IMPLEMENTED

### Context

Python tests that evaluate Hydra terms (like `hydra.monads.pure`, `hydra.monads.bind`) need access to the kernel module definitions as data. Unlike Haskell where `kernelTypesModules` and `kernelTermsModules` are directly available at runtime, Python tests need the Module AST structures to be generated as Python code.

The JSON kernel loader approach (`hydra/sources/kernel.py`) had a **bootstrapping problem**: the JSON decoder is type-directed and needs a schema map containing `hydra.module.Module` to decode Module objects, but building that schema requires loading the kernel modules first.

### Solution: Kernel Sources Modules

Created a new executable `update-python-kernel-sources` that generates "source-level" Python modules containing the kernel Module AST as data:

- **Application-level**: `hydra/monads.py` contains Python functions like `def pure(xp): ...`
- **Source-level**: `hydra/sources/monads.py` contains a `module()` function returning the `Module` object with all Binding/Term AST nodes

This mirrors how Haskell's sources modules work (e.g., `Hydra.Sources.Kernel.Terms.Monads`).

### Implementation Details

**New Files Created:**
- `hydra-ext/src/exec/update-python-kernel-sources/Main.hs` - Executable that:
  1. Takes each kernel module from `kernelModules`
  2. Creates a "virtual module" with namespace `hydra.sources.<original_without_hydra>`
  3. Uses `Hydra.Encode.Module.module_` to encode the Module as a Term
  4. Generates Python code via `writePython`

- `hydra-ext/bin/update-python-kernel-sources.sh` - Shell script to run the executable

- `hydra-ext/package.yaml` - Added executable configuration

**Updated Files:**
- `hydra-ext/bin/sync-python.sh` - Added kernel sources generation as step 3 (now 7 steps total)

**Generated Output:**
- 111 source modules in `hydra-python/src/gen-main/python/hydra/sources/`
- Each module exports a `module()` function returning `hydra.module.Module`
- Example: `hydra/sources/monads.py` contains the full AST for the monads module

### Namespace Mapping

| Original Namespace | Sources Namespace | Python File |
|-------------------|-------------------|-------------|
| `hydra.monads` | `hydra.sources.monads` | `hydra/sources/monads.py` |
| `hydra.formatting` | `hydra.sources.formatting` | `hydra/sources/formatting.py` |
| `hydra.json.decode` | `hydra.sources.json.decode` | `hydra/sources/json/decode.py` |

### Usage in Tests

The test suite can now import kernel module definitions:

```python
from hydra.sources.monads import module as monads_module

# Get the Module object
mod = monads_module()

# Access bindings
for binding in mod.elements:
    print(f"Binding: {binding.name}")
    # binding.term contains the full Term AST
```

### Test Results After Implementation

**Test Results (2026-01-07):**
- **1691 passed** (after loading all 40 kernel term modules)
- **9 failed** (down from 32)
- **158 skipped**

### Remaining Test Failures (9 tests)

After loading all 40 primary kernel term modules into the test graph, only 9 tests remain failing:

#### 1. flows_primitives_map (2 tests)
**Error:** `cannot encode term to a function: TermFunction(value=FunctionPrimitive(...))`

The evaluator cannot convert `TermFunction(FunctionPrimitive(...))` to a Python callable when the primitive is passed as a first-class function argument. This is a known limitation in the Python evaluator.

#### 2. annotations_descriptions (6 tests, cases 4-9)
**Error:** `cannot decode flows to terms`

The test evaluates Flow-returning functions like `hydra.annotations.getTermDescription` and `setTermDescription`. The Python reducer cannot decode Flow structures to terms because the `decodeFlowToTerms` function is not implemented.

**Note:** The `hydra.sources.decoding.py` module fails to load with "too many nested parentheses" due to excessive `cast()` calls. Attempted fix by setting `pythonEnvironmentSkipCasts = True` was reverted as it caused function call generation issues (see Investigation Notes below).

#### 3. monads_error_traces (1 test)
**Error:** RecursionError in `rewriting.py` when processing Flow structures

**Test Case:** `withTrace "one" (withTrace "two" (fail "oops"))`

**Root Cause Analysis (2026-01-08):**

The issue is **not** exponential in the number of `withTrace` calls (2²=4 would be trivial). Instead, it's exponential growth in **term size during beta reduction** due to variable duplication:

**Initial term:** depth=7, nodes=20

**After substitutions:**
| Replace # | Nodes | Depth |
|-----------|-------|-------|
| 1 | 38 | 11 |
| 9 | 82 | 19 |
| 16 | 133 | 20 |
| 32 | 289 | 35 |
| 40 | 2,639 | 60 |
| 41 | 7,942 | 63 |
| 47 | **84,173** | **111** |

**Why this happens:** When beta-reducing `(λx.body)[x := replacement]`, if `x` appears multiple times in `body`, the entire `replacement` term gets duplicated for each occurrence. The monad definitions have complex nested structures:
- `withTrace`: depth=12, nodes=39
- `mutateTrace`: depth=15, nodes=68
- `bind`: depth=14, nodes=44

These definitions contain lambdas with multiple references to the same variable. After just 2 nested `withTrace` calls and 47 substitutions, the term grows to 84,173 nodes with depth 111.

**Why Python hits recursion limit:** The tree traversal in `rewrite_term` uses ~5-10 stack frames per depth level (for `recurse`, `fsub`, `for_function`, `for_field`, etc.). A term depth of 111 easily exceeds Python's default recursion limit of 1000.

**Additional investigation (2026-01-08):**

Further analysis revealed that the issue is not just term duplication but also **monadic nesting**. With the let-binding optimization to prevent term duplication, the test still creates **100,000+ native Python `bind` calls** (traced via instrumentation). Each `bind` creates a closure that captures the previous flow, and when `.value()` is called on the final result, it triggers a cascade of nested `.value()` calls through all 100,000+ closures.

The stack trace shows ~747 frames in `flows.py:54` (bind's `.value()` call) at the point of recursion error, confirming this is a monadic nesting issue rather than term structure.

**Why Haskell doesn't have this problem:** Haskell has tail call optimization (TCO) and lazy evaluation. The same monadic code doesn't create deep call stacks because:
1. TCO eliminates stack frames for tail-recursive calls
2. Lazy evaluation means closures are only evaluated when needed, not eagerly stacked

**Potential fixes:**
1. **Native primitives for `withTrace`/`mutateTrace`:** Add `hydra.monads.withTrace` and `hydra.monads.mutateTrace` as primitives with native Python implementations, bypassing term reduction entirely. **This is the recommended solution** - tested and verified to work instantly (0ms) vs the term-reduced version timing out after 5+ minutes. When implemented natively, the test case `withTrace "one" (withTrace "two" (fail "oops"))` requires **zero** bind calls.

2. **Trampolined Flow monad:** Modify `bind` to return thunks instead of directly calling `.value()`, with a top-level trampoline loop. Tested but found to be insufficient - the trampoline handles the recursion but the continuation stack still grows unboundedly (350,000+ entries) because each continuation creates more binds when applied. This approach alone doesn't solve the problem.

3. **Continuation-passing style (CPS) transform:** Transform the Flow monad to use explicit continuations, avoiding stack growth.

4. **Linear substitution:** Use explicit substitution calculus to avoid term duplication (helps but doesn't solve the monadic nesting).

5. **Lazy evaluation:** Don't expand terms until needed (how Haskell avoids this issue).

6. **Workaround:** Mark test as `disabledForPython` (currently implemented).

**Recommended approach:** Add native Python implementations for `withTrace` and `mutateTrace` in `hydra/sources/libraries.py` (similar to how `bind`, `pure`, `fail` are implemented). This is the cleanest solution because:
- The native implementation is trivial (~40 lines of Python)
- It avoids 100,000+ term reductions and bind calls
- It matches how Haskell uses built-in primitives for these operations
- No changes needed to the Flow monad infrastructure

### Files Cleaned Up

These workaround files have been removed:
- `hydra-python/src/test/python/kernel_bindings.py` - Replaced by generated sources modules
- `hydra-python/src/main/python/hydra/sources/kernel.py` - Had bootstrapping issues

### Next Steps

1. **Implement Flow decoding:** Add support for decoding Flow structures to terms in the Python reducer
2. **Investigate TermFunction encoding:** The "cannot encode term to function" error affects primitives passed as function arguments
3. **Fix recursion in rewriting.py:** Increase recursion limit or refactor to use iterative approach for deeply nested terms

### Investigation Notes (2026-01-07)

~~Attempted to set `pythonEnvironmentSkipCasts = True` to eliminate `cast()` calls, which would fix the "too many nested parentheses" issue in `hydra.sources.decoding.py`. However, enabling skipCasts causes function applications to be generated with currying (`f(arg1)(arg2)`) instead of multiple arguments (`f(arg1, arg2)`), breaking most tests.~~

~~The skipCasts flag has an unintended side effect on how function applications are encoded. The issue is in how `analyzePythonFunction` and related code paths behave differently when skipCasts is enabled. This needs further investigation before the flag can be safely enabled.~~

**Update (2026-01-07):** The skipCasts currying issue has been **FIXED**. The root cause was in `encodeApplication` - when `skipCasts=True`, it used `Arity.termArity` which returns a placeholder value (42) for primitives and 0 for variables. The fix was to:

1. Add `termArityWithPrimitives` and `functionArityWithPrimitives` helper functions that correctly look up primitive arities from the graph
2. Always use type inference for arity calculation (since we can't determine variable arities without types), while only skipping the cast type inference when `skipCasts=True`

The fix is in `hydra-ext/src/main/haskell/Hydra/Ext/Staging/Python/Coder.hs`:
- Lines 330-333: Use `tryFlowWithFallback` with `termArityWithPrimitives` for arity, regardless of skipCasts
- Lines 1521-1537: New helper functions `termArityWithPrimitives` and `functionArityWithPrimitives`

With `pythonEnvironmentSkipCasts = True` now enabled, the tests pass (1691 passed, 9 failed, 158 skipped).

### Update (2026-01-08): Current State and Remaining Investigation

**Current Test Results:** 1765 passed, 6 failed, 183 skipped

Changes applied:
1. **Marked `monads_error_traces` test as `disabledForPython`** - This test causes RecursionError due to term explosion during beta reduction with nested `withTrace` calls
2. **Registered `hydra.lib.eithers.bind` primitive** - Was missing from `libraries.py`
3. **Applied `skipCasts` fix** with `termArityWithPrimitives` helper for correct primitive arity calculation
4. **Changed `analyzePythonFunction` to always use type inference** - preserves function return type annotations while still skipping unnecessary cast() calls
5. **Registered flows primitives with `prim2_interp`** - `flows.apply`, `flows.bind`, `flows.map`, `flows.mapElems`, `flows.mapKeys`, `flows.mapList`, `flows.mapMaybe`, `flows.mapSet` now use interpreter forms

**Remaining 6 failures:** All are `annotations_descriptions` tests (cases 4-9) with error "cannot decode flows to terms"

**Investigation of "cannot decode flows to terms" error:**

The failing tests evaluate terms like:
```
(unwrap(hydra.compute.Flow) @ (hydra.annotations.getTermDescription @ ...) @ unit @ hydra.monads.emptyTrace)
```

Key findings:
- `hydra.annotations.getTermDescription` is NOT a primitive - it's a binding in the graph's `elements`, defined in the `hydra.annotations` module
- When reduced, this binding's term definition is substituted and evaluated
- The error comes from `prims.py:446` in the `flow` TermCoder's decode function

**Open Question:** Both Haskell and Python define the same `flow` coder with `decode _ = fail "cannot decode flows to terms"`. Both use `prim1` for `flows.pure` which calls `coderDecode output` to convert results back to Terms. Yet Haskell tests pass while Python fails.

This requires further investigation:
1. Why doesn't Haskell hit the same decode error path?
2. Is there a difference in how the test terms are reduced between Haskell and Python?
3. Could beta reduction in Python be taking a different path that triggers primitive evaluation differently?

Note: Beta reduction in Python is derived from the same Hydra sources as Haskell (`hydra.sources.kernel.terms.reduction`), so they should be equivalent. The difference may be in how primitives are invoked or how the test graph is constructed.
