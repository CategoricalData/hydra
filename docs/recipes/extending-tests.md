# Extending the common test suite

This recipe explains how to extend Hydra's common test suite with new test cases.

## Background

Hydra's [common test suite](../../wiki/Testing.md#common-test-suite) is designed to run identically in all language
implementations (Haskell, Java, Python, Scala, and Lisp).
Tests are written in Hydra's term-encoded DSL in [hydra-haskell/src/main/haskell/Hydra/Sources/Test/][sources-test]
and code-generated into each target language.

**The normal way to add tests** is to write them directly in the [Sources/Test][sources-test] directory using the
[TTerms][tterms] and [TTypes][ttypes] DSLs.
This recipe covers that workflow, plus a special section on migrating existing Haskell-specific tests to the common
suite.

For more context on Hydra's testing strategy, see [Testing in Hydra](../../wiki/Testing.md).

## Prerequisites

- Familiarity with [Hydra's core concepts](../../wiki/Concepts.md)
- Understanding of term-encoded DSLs ([TTerms][tterms], [TTypes][ttypes])
- Knowledge of the area you're testing (e.g., type inference, library functions, formatting)

## Adding new tests (normal workflow)

This is the standard way to add tests to the common test suite.

### 1. Choose the appropriate test module

Test modules are organized in [Hydra/Sources/Test/][sources-test]:

- **Type checking tests**: [Checking.hs][test-checking]
- **Type inference tests**: [Inference/*.hs][test-inference] (AlgebraicTypes, NominalTypes, Fundamentals, etc.)
- **Library function tests**: [Lib/*.hs][test-lib] (Lists, Strings, etc.)
- **Formatting tests**: [Formatting.hs][test-formatting]
- etc.

[sources-test]: https://github.com/CategoricalData/hydra/tree/main/hydra-haskell/src/main/haskell/Hydra/Sources/Test
[test-checking]: https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Sources/Test/Checking.hs
[test-inference]: https://github.com/CategoricalData/hydra/tree/main/hydra-haskell/src/main/haskell/Hydra/Sources/Test/Inference
[test-lib]: https://github.com/CategoricalData/hydra/tree/main/hydra-haskell/src/main/haskell/Hydra/Sources/Test/Lib
[test-formatting]: https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Sources/Test/Formatting.hs

See [Testing.md](../../wiki/Testing.md#test-categories) for the full list.

### 2. Verify DSL coverage (rarely needed)

The [TTerms][tterms] and [TTypes][ttypes] DSLs should already have the constructors you need.
Only extend them if absolutely necessary - changes to these modules can break existing code for other users.

If you must add DSL functions:

In [TTerms.hs][tterms]:
```haskell
-- Example: Adding constructors for Either
left :: TTerm Term -> TTerm Term
left t = Core.termEither $ Phantoms.left t

right :: TTerm Term -> TTerm Term
right t = Core.termEither $ Phantoms.right t
```

In [TTypes.hs][ttypes]:
```haskell
-- Example: Adding a type constructor
either :: TTerm Type -> TTerm Type -> TTerm Type
either leftType rightType = Core.typeFunction _Either [leftType, rightType]
```

Pattern: Import `qualified Hydra.Dsl.Meta.Core as Core`, use `Core.*` functions, return `TTerm` phantom types.

**Note**: As Hydra matures, DSL extensions should become increasingly rare.
Most new tests can be written using existing DSL functions.

### 3. Write test cases

Open the appropriate test module in
[Sources/Test/](https://github.com/CategoricalData/hydra/tree/main/hydra-haskell/src/main/haskell/Hydra/Sources/Test)
and add test cases using the term-encoded DSL.
The test helper functions vary by test type:

**Type checking tests** (`Checking.hs`) - Use `checkWithType`:
```haskell
checkWithType "test name" []  -- environment (type bindings)
  (inputTerm)                 -- term to type check
  (expectedOutputTerm)        -- term after type annotation
  (expectedType)              -- inferred type
```

**Type inference tests** (`Inference/*.hs`) - Use `inferWithType`:
```haskell
inferWithType "test name" []
  (inputTerm)
  (expectedType)
```

**Library function tests** (`Lib/*.hs`) - Use `primCase` to test primitive evaluation:
```haskell
primCase "test name" _lists_reverse  -- primitive name
  [intList [1, 2, 3]]                -- input arguments
  (intList [3, 2, 1])                -- expected result
```

**Formatting tests** (`Formatting.hs`) - Use `caseConversionCase`:
```haskell
caseConversionCase "test name"
  _caseConversion_lowerSnakeCase
  _caseConversion_upperCamelCase
  "hello_world"
  "HelloWorld"
```

**Example: Type checking test**
```haskell
checkWithType "apply function to function" []
  (lets ["apply">: lambda "f" $ lambda "x" $ var "f" @@ var "x"] $
        var "apply" @@ var "double")
  (letsTyped [
    ("apply", tylams ["t0", "t1"] $ ...,
      T.poly ["t0", "t1"] $ ...)] $
    tyapps (var "apply") [...])
  expectedType
```

**Note for type checking tests**: In `letsTyped` bindings, use `TypeScheme` (via `T.mono` or `T.poly`).
For the final expected type parameter, use `Type` directly.
The test framework converts TypeSchemes to Types using `typeSchemeToFType`.

### 4. Build and test

Build the Haskell code:
```bash
cd hydra-haskell
stack build
```

Run the test suite:
```bash
stack test
```

The test framework automatically discovers and runs tests from all modules in `Sources/Test/`.

### 5. Debug failures

Common issues when tests fail:

**For type checking/inference tests:**
- Check `T.mono` vs `T.poly` usage in `letsTyped` bindings
- Verify type variable order in `tylams` and `tyapps` matches expectations
- Ensure TypeScheme quantification is correct

**For library function tests:**
- Verify input arguments match primitive function signature
- Check that expected output matches the function's behavior
- Ensure term constructors (like `intList`, `string`) are used correctly

**General debugging:**
- Compare verbose test output with expected values
- Use `unTTerm` to extract `Term` from `TTerm` for inspection
- Create minimal test cases to isolate the issue
- Look at similar passing tests in the same module for patterns

**Missing DSL functions:**
- Add to [TTerms.hs][tterms] or [TTypes.hs][ttypes] as needed
- Ensure consistency with `Terms` and `Types` when migrating

## Migrating existing Haskell-specific tests

This section covers the special case of migrating tests from Haskell-specific spec files (like `CheckingSpec.hs`) to
the common test suite.
This is **not the normal workflow** for adding tests - most tests should be written directly in
[Sources/Test/](https://github.com/CategoricalData/hydra/tree/main/hydra-haskell/src/main/haskell/Hydra/Sources/Test)
as described above.

### When to migrate

Migrate tests when:
- Moving tests from `hydra-haskell/src/test/haskell/Hydra/*Spec.hs` to the common suite
- Ensuring test coverage across all language implementations
- Deprecating Haskell-specific tests in favor of cross-language tests

### Migration steps

1. **Identify tests to migrate** - Look in spec files:
   ```bash
   ls hydra-haskell/src/test/haskell/Hydra/*Spec.hs
   ```

2. **Check for missing DSL functions** - Find `Terms.*` calls in the spec file:
   ```bash
   grep -o 'Terms\.[a-z][a-zA-Z0-9]*' CheckingSpec.hs | sort -u > /tmp/terms_used.txt
   grep -o '^[a-z][a-zA-Z0-9]* ::' TTerms.hs | sed 's/ :://' | sort > /tmp/tterms_have.txt
   comm -23 /tmp/terms_used.txt /tmp/tterms_have.txt
   ```

3. **Add missing DSL functions** - Add to [TTerms.hs][tterms] or [TTypes.hs][ttypes] as needed

4. **Translate test cases** - Key differences:

   | Spec file (Terms/Types) | Common suite (TTerms/TTypes) |
   |-------------------------|------------------------------|
   | `Terms.lambda "x"` | `lambda "x"` |
   | `Types.int32` | `T.int32` |
   | Direct `Term` values | `TTerm Term` phantom types |
   | Haskell test framework calls | `checkWithType`, `primCase`, etc. |

5. **Migrate in batches** - Add 5-10 test cases at a time

6. **Verify consistency** - Cross-check migrated tests match source behavior

### Example migration workflow

```bash
cd hydra-haskell

# 1. Find missing DSL functions
grep -o 'Terms\.[a-z][a-zA-Z0-9]*' src/test/haskell/Hydra/CheckingSpec.hs | sort -u

# 2. Add missing functions to TTerms.hs and TTypes.hs

# 3. Migrate a batch of test cases to Sources/Test/Checking.hs

# 4. Build and test
stack build && stack test

# 5. Repeat until all tests migrated
```

## Best practices

1. **Write tests directly in [Sources/Test][sources-test]** - This is the normal workflow; only migrate when necessary
2. **Use descriptive test names** - Make test names clear and specific
3. **Keep DSLs consistent** - [TTerms][tterms] should mirror Terms, [TTypes][ttypes] should mirror Types when migrating
4. **Test in small increments** - Build and test frequently to catch issues early
5. **Follow existing patterns** - Look at similar tests in the same module for guidance
6. **Document complex cases** - Add comments for non-obvious type checking or inference scenarios

## Floating-point test portability

Transcendental math functions (`sin`, `cos`, `exp`, `atanh`, `sinh`, `log`, etc.) are implemented via the
platform's C math library (`libm`). IEEE 754 only mandates bit-exact results for basic operations
(`+`, `-`, `*`, `/`, `sqrt`); transcendental functions are allowed to differ by one unit in the last place
(1 ULP) across platforms. Different `libm` implementations (glibc on Linux, Apple's libm on macOS,
musl on Alpine) do produce such differences in practice.

This affects Hydra because GHC delegates floating-point math to `libm`. When the test suite is compiled
on one platform (e.g., macOS), GHC bakes expected values like `sinh 1.0` into the test data as exact
`Double` bit patterns. When Java or Python runs the same test on a different platform (e.g., Linux CI),
the computed result may differ by 1 ULP, causing the test to fail.

See [GitHub issue #264](https://github.com/CategoricalData/hydra/issues/264) for the original report.

### Guidelines for floating-point test cases

When writing test cases for `hydra.lib.math` primitives that use transcendental functions:

1. **Prefer inputs that produce exact results.** For example:
   - `sin(0) = 0`, `cos(0) = 1`, `exp(0) = 1`, `log(1) = 0`, `sqrt(4) = 2`, `atanh(0) = 0`
   - These are mathematically exact and every `libm` implementation agrees on them.

2. **Use `roundedPrimCase1` / `roundedPrimCase2` for non-trivial inputs.** These helpers (defined in
   `Hydra.Sources.Test.Lib.Math`) wrap both the computed result and the expected value in
   `roundFloat64 12`, rounding to 12 significant digits. This eliminates platform-dependent differences
   in the last few digits while preserving meaningful precision. For example:
   ```haskell
   -- In Hydra.Sources.Test.Lib.Math:
   -- Instead of:  test "sinh 1" 1.0 (sinh 1.0)       -- fragile: GHC-computed value may not match
   -- Use:         roundedPrimCase1 "sinh 1" _math_sinh 1.0 (sinh 1.0)  -- platform-independent
   --
   -- For two-argument primitives:
   --              roundedPrimCase2 "atan2 3 4" _math_atan2 3.0 4.0 (atan2 3.0 4.0)
   ```

   The underlying primitives `roundFloat64`, `roundFloat32`, and `roundBigfloat` (`hydra.lib.math`)
   round a floating-point value to N significant digits. They are available for general use, not just
   tests.

3. **Understand which operations are safe.** IEEE 754 guarantees exact results for:
   - Addition, subtraction, multiplication, division
   - Square root
   - Comparisons
   - Conversions between integer and floating-point types

   All other math functions (trig, hyperbolic, logarithmic, exponential) are potentially non-deterministic
   across platforms.

4. **Java's test runner has additional protection.** `TestSuiteRunner.java` includes a `termsEqual()` method
   with 2-ULP tolerance for float comparisons, providing a safety net. Python and Haskell test runners use
   exact equality, so they are more susceptible to this issue.

## See also

- [Testing in Hydra](../../wiki/Testing.md) - Complete testing documentation
- [Common test suite](../../wiki/Testing.md#common-test-suite) - Test suite structure and categories
- [Adding new tests](../../wiki/Testing.md#adding-new-tests) - Official guide for test creation

## Key files

- [TTerms.hs][tterms] - Term-encoded term constructors
- [TTypes.hs][ttypes] - Term-encoded type constructors
- [Sources/Test/][sources-test] - All common test suite modules
  - [Checking.hs][test-checking] - Type checking tests
  - [Inference/][test-inference] - Type inference tests
  - [Lib/][test-lib] - Library primitive tests
- [TestSuiteSpec.hs][test-suite-spec] - Test runner framework
- [*Spec.hs files][spec-files] - Haskell-specific tests (migration sources)

[tterms]: https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Dsl/TTerms.hs
[ttypes]: https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Dsl/TTypes.hs
[test-suite-spec]: https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/test/haskell/Hydra/TestSuiteSpec.hs
[spec-files]: https://github.com/CategoricalData/hydra/tree/main/hydra-haskell/src/test/haskell/Hydra
