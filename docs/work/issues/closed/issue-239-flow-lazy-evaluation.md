# Issue #239: Flow Lazy Evaluation in Python

## Problem Summary

Python evaluates expressions eagerly, while Haskell evaluates lazily. This causes problems when generated Python code contains `Flow` values (Hydra's state monad), because:

1. In Haskell: `if condition then flowA else flowB` only evaluates ONE flow
2. In Python: Both flows would be evaluated before the conditional is checked

This leads to incorrect state monad behavior where both branches execute and modify state, even though only one result is used.

## Analysis

### Level 1: `if_else` Bug (FIXED)

The `if_else` function in `hydra/lib/logic.py` had a bug where BOTH branches were evaluated:

```python
# BEFORE (buggy):
def if_else(b, x, y):
    actual_x = x() if callable(x) else x  # Both evaluated!
    actual_y = y() if callable(y) else y  # Both evaluated!
    return actual_x if b else actual_y

# AFTER (fixed):
def if_else(b, x, y):
    if b:
        return x() if callable(x) else x
    else:
        return y() if callable(y) else y
```

This fix is now in place and verified working.

### Level 2: Thunk Wrapping in Python Coder

The Python coder (`hydra-ext/src/main/haskell/Hydra/Ext/Staging/Python/Coder.hs`) already wraps `if_else` arguments in lambdas:

```haskell
wrapLazyArguments :: Name -> [Py.Expression] -> [Py.Expression]
wrapLazyArguments name args
  | name == Name "hydra.lib.logic.ifElse" && length args == 3 =
      [args !! 0, wrapInNullaryLambda (args !! 1), wrapInNullaryLambda (args !! 2)]
  | otherwise = args
```

This generates:
```python
hydra.lib.logic.if_else(condition, (lambda: flowA), (lambda: flowB))
```

With the `if_else` fix, this now correctly evaluates only the chosen branch.

### Level 3: Other Lazy Evaluation Points

Beyond `if_else`, there may be other places where eager evaluation causes problems:

1. **Short-circuit operators**: `and_` and `or_` should short-circuit but currently don't
2. **Case expressions**: Pattern matching on unions - the coder handles these via `match` statements which are lazy
3. **Let bindings**: Already handled by converting to `def` functions that are called lazily

## Current State After Fix

The `if_else` fix enables correct Flow behavior for conditional expressions. Test verification:

```python
# Test: Only chosen branch's Flow executes
execution_log = []
flow_a = make_flow('flow_a', 'result_a')  # logs 'flow_a' when run
flow_b = make_flow('flow_b', 'result_b')  # logs 'flow_b' when run

result_flow = if_else(True, (lambda: flow_a), (lambda: flow_b))
result_state = result_flow.value(0, empty_trace)
# execution_log == ['flow_a']  ✓ Only flow_a ran
```

## Remaining Work

### Short-Circuit Boolean Operators

The `and_` and `or_` functions in `hydra/lib/logic.py` currently evaluate both arguments:

```python
def and_(x: bool, y: bool) -> bool:
    return x and y  # y is already evaluated before and_ is called!
```

**Recommendation**: If `and_`/`or_` are used with Flow-producing expressions, the coder should wrap the second argument in a thunk, similar to `if_else`. The runtime functions would need corresponding updates:

```python
def and_(x: bool, y: bool | Callable[[], bool]) -> bool:
    if not x:
        return False
    return y() if callable(y) else y

def or_(x: bool, y: bool | Callable[[], bool]) -> bool:
    if x:
        return True
    return y() if callable(y) else y
```

### Potential Future Issues

1. **Tuple/list construction with Flows**: `(flowA, flowB)` evaluates both. This is only a problem if both are passed to something that only uses one - which would be unusual.

2. **Function arguments**: When calling `f(flowA, flowB)`, both are evaluated. This matches Haskell's call-by-value for strict functions, so it's usually correct.

## Recommendations

1. **Immediate**: The `if_else` fix is complete. Monitor test results to see if this resolves the Flow-related test failures.

2. **Short-term**: If `and_`/`or_` cause issues, apply similar thunk-wrapping treatment in the coder.

3. **Long-term**: Consider a more systematic approach where the coder tracks which expressions have type `Flow[S, A]` and automatically wraps them in thunks when passed to lazy positions.

## Test Coverage

The fix can be verified by running tests that exercise conditional Flow expressions:
- Type inference tests that use `if_else` with monadic branches
- Reduction tests that conditionally execute different Flows
- Any test where incorrect eager evaluation would cause state corruption

## Related Files

- `hydra-python/src/main/python/hydra/lib/logic.py` - Runtime `if_else` (FIXED)
- `hydra-ext/src/main/haskell/Hydra/Ext/Staging/Python/Coder.hs` - Thunk wrapping logic
- `hydra-python/src/main/python/hydra/lib/flows.py` - Flow monad implementation
- `hydra-python/src/main/python/hydra/compute.py` - Flow type definition

---

## Deep Analysis: Let-Binding Laziness Problem

### The Problem

In Haskell, let-bindings are lazy. Consider this Hydra DSL code:

```haskell
"step" <~ ("acc" ~> "bs" ~>
  "b"  <~ Lists.head (var "bs") $
  "tl" <~ Lists.tail (var "bs") $
  Logic.ifElse (Lists.null (var "bs"))
    (Lists.reverse (var "acc"))
    (... use b and tl ...))
```

In Haskell's lazy evaluation:
1. `b` and `tl` are thunks - they're not evaluated until used
2. The `ifElse` check runs first
3. If `bs` is null, `Lists.reverse(acc)` is returned - `b` and `tl` are **never evaluated**
4. Only if `bs` is non-null are `b` and `tl` forced

In the generated Python:
```python
def step(acc, bs):
    b = hydra.lib.lists.head(bs)      # EVALUATED IMMEDIATELY - crashes if bs is empty!
    tl = hydra.lib.lists.tail(bs)     # EVALUATED IMMEDIATELY
    return hydra.lib.logic.if_else(
        hydra.lib.lists.null(bs),
        (lambda: hydra.lib.lists.reverse(acc)),
        (lambda: ... use b and tl ...))
```

The Python code evaluates `head(bs)` and `tail(bs)` BEFORE the null check, causing `IndexError: tuple index out of range`.

### Current Python Coder Architecture

The Python coder handles bindings in several ways:

1. **Simple assignments** (`isSimpleAssignment` returns True):
   - `x = expr` - simple variable assignment
   - Used when term is not a lambda, let, type lambda, or case statement

2. **Function definitions** (`isSimpleAssignment` returns False):
   - `def x(): return expr` - nullary function
   - Used for lambdas, let-bindings, case statements

3. **Inline let expressions** (in `encodeFunction`):
   - `(a := expr1, b := expr2, body)[-1]` - walrus operator trick
   - Used when inlining lambdas with let-bindings

4. **Statement-level bindings** (`encodeBindingsAsDefs`):
   - Generates either simple assignments or function definitions
   - The binding statements are placed BEFORE the body

### The Fix Required

The coder needs to detect bindings that are:
1. Used **only** within branches of an `ifElse` (or similar guard)
2. Could fail if evaluated when the guard is false

**Proposed Solution: Lazy Binding Wrapping**

For bindings that appear before an `ifElse` and are only used in one branch, wrap them in thunks:

```python
# CURRENT (buggy):
def step(acc, bs):
    b = hydra.lib.lists.head(bs)
    tl = hydra.lib.lists.tail(bs)
    return hydra.lib.logic.if_else(...)

# PROPOSED (correct):
def step(acc, bs):
    def b(): return hydra.lib.lists.head(bs)
    def tl(): return hydra.lib.lists.tail(bs)
    return hydra.lib.logic.if_else(
        hydra.lib.lists.null(bs),
        (lambda: hydra.lib.lists.reverse(acc)),
        (lambda: ... b() ... tl() ...))  # Call b() and tl() when used
```

### Implementation Options

**Option 1: Wrap ALL non-simple bindings as thunks**
- Simple but potentially inefficient
- Every binding becomes `def x(): return expr`
- Every usage becomes `x()`
- Pro: Always correct
- Con: Performance overhead, less readable code

**Option 2: Static analysis for guard-dependent bindings**
- Analyze the term structure to find bindings used only in guarded contexts
- Only wrap those bindings as thunks
- Pro: More efficient, cleaner output
- Con: Complex analysis, might miss edge cases

**Option 3: Reorder bindings after guards**
- Move binding definitions inside the branch where they're used
- Pro: Clean output, no thunks needed
- Con: May not always be possible (bindings used in multiple branches)

### Key Files to Modify

1. **`Coder.hs`** (main changes):
   - `encodeBindingsAsDefs` - Detect which bindings need lazy wrapping
   - `encodeBindingAs` - Generate thunk-wrapped bindings when needed
   - `encodeVariable` - Call thunked bindings with `()`

2. **`CoderUtils.hs`**:
   - Add `needsLazyBinding :: Term -> [Binding] -> Binding -> Bool`
   - Analyze term structure to detect guard-dependent bindings

3. **`PythonEnvironment`** (in Names.hs or similar):
   - Track which bindings are thunked so `encodeVariable` knows to add `()`

### Affected Generated Files

The bug manifests in any generated code with this pattern:
- `rewriting.py` - `normalizeTypeVariablesInTerm` (the failing test)
- Likely others with similar `step` recursion patterns

### Test Results Context

Current test suite results:
- **129 passed**: Case conversion tests, inference failure tests
- **164 failed**: Inference success tests (hit lazy evaluation bug)
- **1262 skipped**: Other test types not yet implemented

All 164 failures trace back to `rewriting.py:793` where `Lists.head(bs)` is called before the null check.
