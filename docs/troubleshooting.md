# Troubleshooting

This document collects common problems and debugging strategies across the Hydra project.
For task-specific troubleshooting, see also:

- [Code generation troubleshooting](recipes/code-generation.md#troubleshooting)
- [JSON kernel troubleshooting](recipes/json-kernel.md#troubleshooting)
- [Syncing Python troubleshooting](recipes/syncing-python.md#troubleshooting)
- [DSL troubleshooting](dsl-guide.md#troubleshooting)

## General strategy

When something breaks, follow this order:

1. **Check Haskell first.** Run `stack test` in `packages/hydra-haskell/`. If Haskell doesn't pass,
   nothing downstream will work correctly.
2. **Check whether the file is generated.** If the problem is in `src/gen-main/` or
   `src/gen-test/`, the fix belongs in the Haskell source or code generator, not in the
   generated file.
3. **Check primitive registration.** Many "unknown primitive" or test failures trace back to
   a primitive that exists as a class/function but isn't registered. See
   [primitive registration](#primitive-registration-errors) below.
4. **Regenerate.** If Haskell passes but a downstream implementation fails, regenerate with
   the appropriate `sync-*.sh` script before debugging further.

## Primitive registration errors

**Symptom**: "unknown primitive" errors, or test failures for primitives that clearly have
implementations.

**Cause**: A primitive class/function exists but isn't listed in the language's registration
file.

**Registration files** (check all of these when adding or debugging primitives):

| Language | Registration file |
|----------|-------------------|
| Haskell | `packages/hydra-haskell/src/main/haskell/Hydra/Sources/Libraries.hs` |
| Java | `heads/java/src/main/java/hydra/lib/Libraries.java` |
| Python | `heads/python/src/main/python/hydra/lib/libraries.py` |
| Scala | `heads/scala/src/main/scala/hydra/lib/Libraries.scala` |
| Lisp (Clojure) | `heads/lisp/clojure/src/main/clojure/hydra/lib/libraries.clj` |

**Fix**: Add the primitive to the appropriate `*Primitives()` method (Java) or equivalent
in the registration file. See [adding primitives](recipes/adding-primitives.md) for the
full checklist.

## Java test failures

### Running tests

```bash
# From the repo root
./gradlew :packages:hydra-java:test

# With detailed failure output
./gradlew :packages:hydra-java:test --tests "*TestSuiteRunner*" --info 2>&1 | grep -A 20 "FAILED"
```

The test runner is at `heads/java/src/test/java/hydra/TestSuiteRunner.java`. It dispatches
test cases by type (Evaluation, Inference, Checking, etc.) using the visitor pattern.
Evaluation tests call `Reduction.reduceTerm` and assert the result matches expected output.

### Tracing primitive dispatch

When a primitive test fails in Java, the call chain is:

1. Test defines a term using `primitive _flows_xxx @@ arg1 @@ arg2`
2. `Reduction.reduceTerm` reduces the term
3. The reducer looks up the primitive in the graph by name
4. It calls `prim.implementation.apply(reducedArgs)`
5. The result is further reduced

**If step 3 fails** (primitive not found): check `Libraries.java` registration
(see [above](#primitive-registration-errors)).

**If step 4 fails**: check the `implementation()` method of the primitive class.
The `implementation()` method must construct a term-level result, not execute native code.
A stub that throws `UnsupportedOperationException` will cause runtime failures.
See `heads/java/src/main/java/hydra/lib/flows/Map.java` for a good example using
`hydra.dsl.Terms` helpers (`wrap`, `unwrap`, `lambda`, `app`, `flowState`, `project`,
`variable`, `just`, `nothing`, etc.).

### Higher-order primitives (`prim2Eval`)

In `Libraries.hs`, primitives are registered with either `prim1`/`prim2`/`prim3` (simple)
or `prim1Eval`/`prim2Eval`/`prim3Eval` (higher-order). The `Eval` variants have an
additional "eval element" in `packages/hydra-haskell/src/main/haskell/Hydra/Sources/Eval/Lib/`,
generated into `dist/java/hydra-kernel/src/main/java/hydra/eval/lib/`.

**Both paths matter**: The reducer calls `implementation()` for all primitives, so even
`prim2Eval` primitives need a working `implementation()` in their Java
`PrimitiveFunction` class.

### Key files for Java debugging

| Purpose | Path |
|---------|------|
| Primitive registration | `heads/java/src/main/java/hydra/lib/Libraries.java` |
| Primitive classes | `heads/java/src/main/java/hydra/lib/<library>/` |
| Generated eval elements | `dist/java/hydra-kernel/src/main/java/hydra/eval/lib/` |
| DSL term builders | `heads/java/src/main/java/hydra/dsl/Terms.java` |
| Either utilities | `heads/java/src/main/java/hydra/util/Either.java` |
| Test runner | `heads/java/src/test/java/hydra/TestSuiteRunner.java` |
| Reducer | `dist/java/hydra-kernel/src/main/java/hydra/reduction/Reduction.java` |

## Haskell build issues

### Stack overflow during generation

**Symptom**: Stack overflow when running code generation in GHCi.

**Fix**: Increase the runtime stack size:

```bash
stack ghci --ghci-options='+RTS -K256M -A32M -RTS'
```

The sync scripts handle this automatically.

### "No such field: X" during code generation

**Cause**: Missing entries in `Meta.hs` enums (`TermVariant`/`TypeVariant`). This happens
when a new type or term constructor is added to the kernel but not reflected in the variant
enums.

**Fix**: Update `TermVariant`/`TypeVariant` in the relevant `Meta.hs` source file, then
rebuild and regenerate.

## Python issues

### Naming collisions with reserved words

Python implementations use trailing underscores on reserved words: `T.list_()`,
`Terms.lambda_()`, `Terms.list_()`. Uses `FrozenDict` instead of regular dicts.

See [Python DSL guide](dsl-guide-python.md) for details.

## Bootstrap problems

When extending core types, you face a circular dependency: the code generator must
understand new constructors to generate itself. The solution is:

1. Manually patch generated files so the project compiles
2. Rebuild
3. Regenerate to overwrite your manual patches

This also applies to generated DSL modules in `src/gen-main/haskell/Hydra/Dsl/`.
See [extending Hydra Core](recipes/extending-hydra-core.md) for the full walkthrough.

## Floating-point test portability

Transcendental math functions (`sin`, `exp`, `atanh`, etc.) can produce results that differ
by 1 ULP across platforms. When adding float64 test cases for these functions, use
`roundedPrimCase1` / `roundedPrimCase2` instead of raw `primCase`.
See [extending tests](recipes/extending-tests.md#floating-point-test-portability).

## Related resources

- [Adding primitives](recipes/adding-primitives.md) -- full checklist for new primitives
- [DSL guide](dsl-guide.md) -- DSL errors and debugging tips
- [Implementation guide](implementation.md) -- architecture and module structure
- [Testing wiki page](https://github.com/CategoricalData/hydra/wiki/Testing) -- test runners
  and test categories
