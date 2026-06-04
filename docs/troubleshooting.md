# Troubleshooting

This document collects common problems and debugging strategies across the Hydra project.
For task-specific troubleshooting, see also:

- [Code generation troubleshooting](recipes/code-generation.md#troubleshooting)
- [JSON kernel troubleshooting](recipes/json-kernel.md#troubleshooting)
- [Syncing Python troubleshooting](recipes/syncing-python.md#troubleshooting)
- [DSL troubleshooting](dsl-guide.md#troubleshooting)

## General strategy

When something breaks, follow this order:

1. **Check Haskell first.** Run `stack test` in `heads/haskell/`. If the Haskell head
   doesn't pass, nothing downstream will work correctly.
2. **Check whether the file is generated.** If the problem is in anything under `dist/`,
   the fix belongs in the Haskell source or code generator (under `packages/` or `heads/`),
   not in the generated file.
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
| Haskell | `packages/hydra-kernel/src/main/haskell/Hydra/Sources/Libraries.hs` |
| Java | `overlay/java/hydra-kernel/src/main/java/hydra/lib/Libraries.java` (relocated by #418) |
| Python | `overlay/python/hydra-kernel/src/main/python/hydra/sources/libraries.py` (relocated by #418) |
| Scala | `heads/scala/src/main/scala/hydra/lib/Libraries.scala` |
| Lisp (Clojure) | `heads/lisp/clojure/src/main/clojure/hydra/lib/libraries.clj` |

**Fix**: Add the primitive to the appropriate `*Primitives()` method (Java) or equivalent
in the registration file. See [adding primitives](recipes/adding-primitives.md) for the
full checklist.

## Java test failures

### Running tests

```bash
# From the Java head
(cd heads/java && ./gradlew :hydra-java:test)

# With detailed failure output
(cd heads/java && ./gradlew :hydra-java:test --tests "*TestSuiteRunner*" --info) 2>&1 | grep -A 20 "FAILED"
```

The test runner is at `heads/java/src/test/java/hydra/TestSuiteRunner.java`. It dispatches
test cases by type (Evaluation, Inference, Checking, etc.) using the visitor pattern.
Evaluation tests call `Reduction.reduceTerm` and assert the result matches expected output.

### Tracing primitive dispatch

When a primitive test fails in Java, the call chain is:

1. Test defines a term using `primitive _lists_xxx @@ arg1 @@ arg2`
2. `Reduction.reduceTerm` reduces the term
3. The reducer looks up the primitive in the graph by name
4. It calls `prim.implementation.apply(reducedArgs)`
5. The result is further reduced

**If step 3 fails** (primitive not found): check `Libraries.java` registration
(see [above](#primitive-registration-errors)).

**If step 4 fails**: check the `implementation()` method of the primitive class.
The `implementation()` method must construct a term-level result, not execute native code.
A stub that throws `UnsupportedOperationException` will cause runtime failures.
See `overlay/java/hydra-kernel/src/main/java/hydra/lib/lists/Map.java` for a higher-order example
using `hydra.dsl.Terms` helpers (`lambda`, `app`, `variable`, etc.).

### Higher-order primitives

Higher-order primitives (e.g. `lists.map`, `lists.foldl`, `eithers.bind`) are
declared the same way as first-order primitives, via `primDef`/`primNoDef` in
their `Hydra/Sources/Kernel/Lib/<Sub>.hs` registry. Their higher-orderness is
expressed in the `TermSignature` (function-typed value parameters) rather than
via a separate registration path. On the Haskell host, the same `prim*` family
in `Hydra.Dsl.Prims` pairs each name with its native implementation; the
native implementation must accept and apply its function arguments correctly.

### Key files for Java debugging

| Purpose | Path |
|---------|------|
| Primitive registration | `overlay/java/hydra-kernel/src/main/java/hydra/lib/Libraries.java` (#418) |
| Primitive classes | `overlay/java/hydra-kernel/src/main/java/hydra/lib/<library>/` (#418) |
| DSL term builders | `overlay/java/hydra-kernel/src/main/java/hydra/dsl/Terms.java` (#418) |
| Either utilities | `overlay/java/hydra-kernel/src/main/java/hydra/util/Either.java` (#418) |
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

### Build appears to succeed but the kernel didn't actually compile

**Symptom**: `stack build` reports exit 0, but a later step fails because
generated artifacts weren't updated.

**Cause**: A pipeline like `stack build 2>&1 | tail -30` masks Stack's
non-zero exit code with `tail`'s zero exit. Stack's compile-error output
also runs into hundreds of lines, so `tail -30` typically captures only the
final `[S-7282]` postscript without the actual `error: [GHC-...]` line that
identifies the broken module.

**Fix**: Redirect to a file and inspect by greppable patterns:

```bash
stack build > /tmp/build.log 2>&1
grep -E "error:|S-7011" /tmp/build.log | head
```

For a long compile, run in the background and tail-watch a specific filter
rather than the raw log; the noise-to-signal ratio for full Stack output
is high.

### "No such field: X" during code generation

**Cause**: Missing entries in `Meta.hs` enums (`TermVariant`/`TypeVariant`). This happens
when a new type or term constructor is added to the kernel but not reflected in the variant
enums.

**Fix**: Update `TermVariant`/`TypeVariant` in the relevant `Meta.hs` source file, then
rebuild and regenerate.

### "Found N untyped binding(s) ..."

**Symptom**: Phase 1 aborts with `Found N untyped binding(s) (after case hoisting); each must
carry a type scheme at this stage. ... Offending bindings (module :: name): hydra.foo :: hydra.foo.bar, …`.

**Cause**: A term definition reached the type-checking gate (post-inference, or on a no-infer
path) without a type scheme. This is legitimate *only* for DSL-defined modules **before**
inference — derived modules (`encode`/`decode`/`dsl`) and post-inference modules must carry a
signature on every term definition. The usual real cause is **stale `dist/json/.../*.json`
field shapes after a kernel record-field rename**: the old JSON decodes against the new schema
with the renamed field unread, so the definition loses its signature and surfaces here rather
than at the decode site. (Seen in #368: `"typeScheme"` JSON vs the renamed `"signature"` field.)

**Fix**: Regenerate the affected package's JSON (the error names the module). For Java/Python
packages, `update-json-main --include-java-python` after busting the input caches; for others,
`assemble-distribution.sh <pkg>`. Don't patch bindings one at a time — a field rename hits every
record in the package. Per #414 the message names the source module + the likely cause precisely;
a record decoder also names the expected type ("expected a record of type T") on a shape mismatch.

## Python issues

### Naming collisions with reserved words

Python implementations use trailing underscores on reserved words: `T.list_()`,
`Terms.lambda_()`, `Terms.list_()`. Uses `FrozenDict` instead of regular dicts.

See [Python DSL guide](dsl-guide-python.md) for details.

### Slow term-level workflows

If generated-Python codegen, inference, or term rewriting is unexpectedly slow,
profile before guessing. The codegen pipeline in particular allocates millions
of small persistent containers; an O(n) operation in `hydra.python.util.{ConsList,
PersistentMap, PersistentSet}` will dominate everything else.

```bash
python -m cProfile -o /tmp/codegen.prof your_script.py
python -c "import pstats; pstats.Stats('/tmp/codegen.prof').sort_stats('tottime').print_stats(40)"
```

Look for any single function consuming >5% of `tottime`. The persistent
collections in `hydra.python.util/` are thin facades over native `dict`/
`frozenset`/`tuple`; if you're hot-spotting in one of their methods, the fix is
usually to delegate to a native operation rather than to loop in Python.

Also: for long-running workloads (anything >1s), use `pypy3` rather than
CPython. PyPy's JIT is several times faster on term walks; CPython only wins on
short microbenchmarks. See the [Python README CPython vs PyPy
section](../packages/hydra-python/README.md#cpython-vs-pypy).

### Inline `cases _Enum` dispatches in let bindings

The Python coder's `encodeUnionEliminationInline` (used when a `cases _Enum`
expression appears inside a let binding rather than at top level) emits
`arg == EnumType.VARIANT` checks. The serialization of `==` lives in
`Sources/Python/Serde.hs::comparisonToExpr`. If that function ever drops the
RHS again, every inline enum dispatch silently picks the first branch — see
the entry in `docs/history/python-host-perf-investigation.md` for the full story.

## Build and test issues across languages

### `gradle :hydra-java:test` fails with "package X does not exist"

The `hydra-java` Gradle build's `main` and `headsExtras` source sets import
`hydra.haskell.*`, `hydra.python.*`, `hydra.scala.*`, `hydra.lisp.*`
directly. The build fails with "package does not exist" if those
`dist/java/<pkg>/` trees are missing.

`bin/sync-java.sh` (and the narrow `bin/sync.sh --hosts java --targets java`)
only populate `dist/java/{hydra-kernel, hydra-java, hydra-pg, hydra-rdf}` —
not the cross-language coder dists. Run `bin/sync.sh` (full host × target)
or `bin/sync.sh --hosts java --targets <every-language>` to produce all
the per-language Java dist trees.

### `sbt test` from `packages/hydra-scala/` fails on type mismatches

Same shape as the Java issue. The Scala sbt project declares unmanaged
source directories over `dist/scala/hydra-{kernel,haskell,java,python,scala,lisp}/`.
`bin/sync-scala.sh` is narrow (host=scala × target=scala only) and does not
populate `dist/scala/hydra-{haskell,java,python,lisp}/`. Use
`bin/sync.sh --hosts scala --targets all` for a full Scala dist refresh.

### `hydra-java:compileJava` OOM during incremental rebuild

The Gradle build daemon's `-Xmx` setting does not apply to the forked
compiler worker, which inherits a 512 MB default that's insufficient for
the rollup's many classes during incremental analysis. The build file
in `packages/hydra-java/build.gradle` already enables forking with a 6 GB
cap (`compileJava { options.fork = true; options.forkOptions.memoryMaximumSize = '6g' }`).
If you still see OOM, check that override hasn't been clobbered locally.

### `bin/sync.sh` does not run target-language tests

`bin/sync.sh` regenerates code into every target language but only runs
the Haskell-side `stack test` step. To validate a target's runtime, run
that head's own test driver:

- Python: `heads/python/bin/test-distribution.sh hydra-kernel`
- Java: `./gradlew :packages:hydra-java:test`
- Scala: `heads/scala/bin/test-distribution.sh hydra-kernel`
- Lisp dialect: `packages/hydra-lisp/bin/run-tests.sh <dialect>`

The full cross-host bootstrap demo (`bin/run-bootstrapping-demo.sh`) is a
heavier validation that exercises cross-host code generation plus tests.

### `verify-json-kernel` reports `element count differs: N vs M`

As of [#392](https://github.com/CategoricalData/hydra/issues/392), the sync
reconciles this automatically — you should rarely see it surface.

Step 3 of `sync-haskell.sh` verifies that each committed
`dist/json/hydra-kernel/.../<module>.json` matches what the source DSL produces.
A merge that updated source DSL but not the corresponding JSON (or vice versa)
leaves them out of sync. The verify step now detects the drift, runs
`update-json-kernel` (the authoritative DSL → JSON writer for that tree),
logs the changed files, re-verifies, and continues. The regenerated `dist/json`
is left in your working tree to review and commit.

If the sync instead *fails* after reconciliation with "still reports drift after
update-json-kernel regenerated the JSON", that is a genuine generator bug —
non-deterministic DSL → JSON output — not recoverable drift. Investigate the
kernel JSON generator (`Hydra.Generation.writeModulesJson` and the encoders it
calls) before re-running; re-running will not help.

## Bootstrap problems

When extending core types, you face a circular dependency: the code generator must
understand new constructors to generate itself. The solution is:

1. Manually patch generated files so the project compiles
2. Rebuild
3. Regenerate to overwrite your manual patches

This also applies to generated DSL modules in `dist/haskell/hydra-kernel/src/main/haskell/Hydra/Dsl/`.
See [extending Hydra Core](recipes/extending-hydra-core.md) for the full walkthrough.

## Floating-point test portability

Transcendental math functions (`sin`, `exp`, `atanh`, etc.) can produce results that differ
by 1 ULP across platforms. When adding float64 test cases for these functions, use
`roundedPrimCase1` / `roundedPrimCase2` instead of raw `primCase`.
See [extending tests](recipes/extending-tests.md#floating-point-test-portability).

## Annotated `Core.Term` values in test fixtures

When a Hydra-DSL test fixture builds a `Core.Term` value that needs to be a
`TermAnnotated` -- e.g. to test a validator that inspects annotation Terms --
do **not** use `Phantoms.doc` to attach the annotation.
The Haskell coder's `encodeTerm` strips outer-layer annotations before its
variant dispatch (the description, if any, has already been captured at the
enclosing binding via `getTermDescription` and emitted as a Haskell comment).
That stripping silently drops the annotation when the annotated term is
nested inside a record literal rather than at a definition's top level.

Use the explicit data-constructor form instead. Since #386 the
`AnnotatedTerm.annotation` field is typed as `Term`, not `Map Name Term`,
so the second argument to `Core.annotatedTerm` is a `Term`. Use
`wrapAnnotationMap` to encode a host-side `Map Name Term`:

```haskell
Core.termAnnotated $ Core.annotatedTerm
  someBody
  (wrapAnnotationMap @@ (Maps.fromList $ list [
    Phantoms.pair (Core.name $ string "description")
      (Core.termLiteral $ Core.literalString $ string "...")]))
```

Or build the annotation Term directly using the canonical
`inject(Term){map: TermMap[(TermVariable key, value), …]}` shape (e.g. via
the test-suite helpers in `Hydra.Sources.Test.Annotations` such as
`annotatedExp` / `annotatedExp1`).

Either form produces a `TermInject _Term "annotated" ...` Hydra term, which
the coder treats as data and emits as `Core.TermAnnotated (Core.AnnotatedTerm{...})`.

## Related resources

- [Adding primitives](recipes/adding-primitives.md) -- full checklist for new primitives
- [DSL guide](dsl-guide.md) -- DSL errors and debugging tips
- [Implementation guide](implementation.md) -- architecture and module structure
- [Testing wiki page](https://github.com/CategoricalData/hydra/wiki/Testing) -- test runners
  and test categories
