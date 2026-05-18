# Pitfalls and gotchas (extended)

CLAUDE.md keeps a short list of hard rules and a short list of mental models.
This page covers specific gotchas — concrete known-issue notes that don't belong
in the top-level orientation.

## Specific known issues

### Primitive registration

A primitive class can exist but be invisible at runtime if it isn't registered
in `Libraries.java` / `Libraries.hs` / `libraries.py` /
`Libraries.scala` / `libraries.clj`.
Always check registration when debugging "unknown primitive" errors.

### Primitive `implementation()` must not throw (Java)

Even higher-order (`prim2Eval`) primitives need a working `implementation()`
that constructs term-level results.
See [docs/recipes/adding-primitives.md](../docs/recipes/adding-primitives.md).

### Floating-point test portability

Use `roundedPrimCase1` / `roundedPrimCase2` for transcendental math tests.
Linux CI and macOS local diverge on the last bits of trig/log/exp results.
See [docs/recipes/extending-tests.md](../docs/recipes/extending-tests.md).

### Memory for code generation

`stack ghci` for Hydra DSL generation needs a larger heap than the default.
Use `stack ghci --ghci-options='+RTS -K256M -A32M -RTS'`,
or let the sync scripts handle it.

### Bash heredoc hangs in Claude shell snapshot

If a sync or other script hangs at a `cat >> file <<HEREDOC`-style construct
with a self-loop pipe (one bash process holding both ends),
the cause is usually `set -o onecmd` inherited from Claude's shell snapshot —
not a real bash bug.
Tests this way: run the same script directly from the user's terminal.
If it works there, the issue is the agent's shell environment, not the script.
Don't patch the script as a workaround;
ask the user to run the command from their own shell.

### `Too many open files` in user's local sync

`digest-check` opens every generated file to hash it.
On macOS, the default per-process FD limit is often 256, which `digest-check`
exceeds during a full sync.
If the user reports this error, recommend `ulimit -n 65536` before re-running.
The kernel limits (`kern.maxfilesperproc` ~ 245760) are far higher; only the
shell's `ulimit` blocks.

### Stale `dist/haskell` artifacts after non-baseline edits

`bin/sync-haskell.sh` regenerates the JSON for every package and the
*baseline* Haskell packages (`hydra-kernel`, `hydra-haskell`) but does not
by default re-run the per-package `assemble-distribution.sh` for any other
package. Every coder package is non-baseline: `hydra-java`, `hydra-python`,
`hydra-scala`, `hydra-lisp`, `hydra-go`, `hydra-pg`, `hydra-rdf`, `hydra-coq`,
`hydra-typescript`, `hydra-wasm`, `hydra-ext`, `hydra-bench`.

Note that `hydra-bench` is also opt-in for the JSON regen — it requires
`--include-bench` on `update-json-main` and `update-json-manifest` (set by
`bin/sync-bench.sh`). The default `bin/sync.sh` does not regenerate it.

A common surprise: editing a Java-coder DSL source under
`packages/hydra-java/src/main/haskell/Hydra/Sources/Java/Coder.hs` and running
`sync-haskell.sh` regenerates the JSON correctly but leaves
`dist/haskell/hydra-java/.../Coder.hs` at its old contents. The runtime Java
codegen is loaded from `dist/haskell/hydra-java/`, so subsequent
`bin/sync-packages.sh hydra-kernel --targets java` runs use the *old* coder
and produce the *old* output. Symptom: source-level fix appears not to take
effect.

If you've edited a DSL source under any non-baseline package and the
corresponding `dist/haskell/<pkg>/` file hasn't picked up your change, run
`bin/sync-packages.sh <pkg> --targets haskell` (or
`heads/haskell/bin/assemble-distribution.sh <pkg>` directly).

Cache hits can also mask edits.
A coarse "make me clean" sequence:

```sh
rm -f dist/haskell/<pkg>/src/main/digest.json
rm -f dist/json/<pkg>/src/main/digest.json
rm -f dist/json/digest.main.json
rm -f heads/haskell/.stack-work/bootstrap-from-json-cache.txt
rm -f heads/haskell/.stack-work/verify-json-kernel-cache.txt
heads/haskell/bin/sync-haskell.sh --no-tests
bin/sync-packages.sh <pkg> --targets haskell --no-tests
```

Then sync forward into whatever target language consumes the regenerated coder.

### `gradle :hydra-java:test` needs all coder language packages in `dist/java/`

The `hydra-java` Gradle build has two main source sets: `main` (the
generated kernel + every coder package's generated Java) and `headsExtras`
(developer drivers like `Generation.java` plus demos). Both source sets
import `hydra.haskell.*`, `hydra.python.*`, `hydra.scala.*`, `hydra.lisp.*`
directly, so the compile fails with "package does not exist" if those
`dist/java/<pkg>/` trees are missing.

`bin/sync.sh --hosts java --targets java` (and by extension
`bin/sync-java.sh`) only populates
`dist/java/{hydra-kernel, hydra-java, hydra-pg, hydra-rdf}`, **not** the
cross-language coder dists `dist/java/hydra-{haskell,python,scala,lisp}/`.
To produce all the per-language Java dist trees you need
`bin/sync.sh` (full host × target sync) or
`bin/sync.sh --hosts java --targets <every-language>`.

User-callable wrapper scripts that compile cross-language Java code
(`bin/generate-hydra-java-from-java.sh`, `heads/java/bin/inference-bench.sh`)
**self-heal**: they call `bin/sync.sh` themselves before invoking gradle.
Warm-cache full sync is ~3 minutes; cold-cache is whatever a real first
build takes. See the next entry for the convention.

Symptom (without self-heal): `compileHeadsExtrasJava FAILED` with many
"package hydra.lisp.syntax does not exist" errors.

### Wrapper scripts auto-sync; testers don't

Convention: any **user-callable wrapper script** that invokes a build
step requiring cross-language `dist/<lang>/hydra-*` trees must call
`bin/sync.sh` (or the narrowest sufficient `sync-*.sh`) itself, gated
by the env var `HYDRA_IN_SYNC`. `bin/sync.sh` exports `HYDRA_IN_SYNC=1`
around its own Phase 5 calls so those wrappers don't recurse.

**Testers** (`heads/<lang>/bin/test-distribution.sh`, anything labeled
"layer 2.5") deliberately do **not** self-sync — their contract is "the
distribution is already assembled". Sync responsibility belongs to the
calling layer.

Scripts following this convention today:

| Script | Prereq sync | Reason |
|---|---|---|
| `bin/generate-hydra-java-from-java.sh` | full `bin/sync.sh` | gradle build imports every per-language `dist/java/hydra-*` |
| `bin/generate-hydra-python-from-python.sh` | `bin/sync-python.sh` | self-host driver only reads `dist/python/hydra-{kernel,python}` |
| `heads/java/bin/inference-bench.sh` | full `bin/sync.sh` | same gradle task as the Java generator |
| `bin/sync.sh` itself | n/a | sets `HYDRA_IN_SYNC=1` around its Phase 5 calls |
| `demos/bootstrapping/bin/bootstrap-all.sh` | scoped `bin/sync.sh` | pre-sync derived from `--hosts`/`--targets` |
| `bin/run-inference-bench.sh` | `bin/sync-bench.sh` | hydra-bench is opt-in; not part of default sync |

When adding a new wrapper that compiles cross-language code, follow the
same pattern. When in doubt, prefer a full `bin/sync.sh` call over a
scoped one — warm-cache sync is cheap and being too narrow is what made
this bug class possible in the first place.

### `sbt test` from `packages/hydra-scala/` needs cross-target dist trees

Same shape as the `hydra-java` issue above. The Scala sbt project at
`packages/hydra-scala/build.sbt` declares `unmanagedSourceDirectories` over
`dist/scala/hydra-{kernel,haskell,java,python,scala,lisp}/...`. If any of
those cross-target dists is stale (e.g., a kernel-type change like #311's
thunked `UniversalTestCase.actual` doesn't propagate because `dist/scala/`
is gitignored), `sbt compile` fails on type mismatches in code that hasn't
been regenerated. `heads/scala/bin/test-distribution.sh hydra-kernel`
exhibits the same.

`bin/sync-scala.sh` is **narrow**: it only covers `host=scala × target=scala`,
so it populates `dist/scala/hydra-scala/` (and `hydra-kernel`/`hydra-pg`/`hydra-rdf`
via Phase 3), but **not** `dist/scala/hydra-{haskell,java,python,lisp}/`. The
package README's "Full sync" label is misleading; that command is a self-host
self-target refresh, not a comprehensive one.

To fully refresh Scala dist, use `bin/sync.sh --hosts scala --targets all`
(or per-package `heads/scala/bin/assemble-distribution.sh hydra-haskell` etc.).
Symptom: `sbt test` reports `Type Mismatch Error: Found (Unit => String),
Required: String` or similar in a generated `dist/scala/hydra-<lang>/.../*.scala`
file with an mtime predating a kernel-type change.

### `hydra-java:compileJava` OOM during incremental rebuild

Symptom: `Exception: java.lang.OutOfMemoryError thrown from the
UncaughtExceptionHandler in thread "Memory manager"` during
`:hydra-java:compileJava`, triggered by editing any non-trivial Java source
in the rollup. The Gradle build daemon's `-Xmx` setting (whether configured
via `org.gradle.jvmargs` or `gradle.properties`) does **not** apply to the
forked compiler worker, which inherits a 512m default that's insufficient
for the rollup's ~2000+ classes during incremental analysis.

Fix is in `packages/hydra-java/build.gradle`:

```groovy
compileJava {
    options.fork = true
    options.forkOptions.memoryMaximumSize = '6g'
}
```

This was added in commit `b2c046e87` after a Testing.java edit triggered the
OOM. Adds 6g transient memory pressure only during compile — no runtime cost.

Note: `gradle.properties` at the repo root is gitignored and exists as a
developer-local escape hatch for `org.gradle.jvmargs` and other per-developer
Gradle config — useful for local experimentation, but JVM args set there only
affect the build daemon, not forked compiler workers, so it would not have
fixed this OOM on its own.

### Stale per-dialect Lisp `struct-compat.lisp`

`heads/lisp/common-lisp/src/main/common-lisp/hydra/struct-compat.lisp` is
hand-generated by `gen-compat.sh` from the current `dist/common-lisp/...` tree.
After kernel-level renames (e.g., merging two Module fields into one),
re-run `heads/lisp/common-lisp/src/main/common-lisp/hydra/gen-compat.sh`,
or the loader's `hydra-defstruct` macro will short-circuit on the old constructor's
`fboundp` and skip defining the new accessor — leading to "function FOO undefined"
errors at test time.

### Emacs Lisp regex needs `case-fold-search` bound to nil

Emacs' default `case-fold-search` is `t` in batch mode, which makes
character classes like `[a-z]` case-insensitive — `[a-z]` then matches `H`.
Hydra follows POSIX-ERE case-sensitive semantics. Every regex primitive
in `heads/lisp/emacs-lisp/src/main/emacs-lisp/hydra/lib/regex.el` binds
`case-fold-search` to `nil` in its `let*`. New EL regex primitives must
do the same.

### `bin/sync.sh` does not run target-language tests

`bin/sync.sh` runs Phase 1 + 2 only (DSL → JSON → assemble). It exits 0
even if a target's tests would fail. To run tests, use
`bin/sync-packages.sh` (single target) or `bin/sync-all.sh` (everything).
When asked "does sync pass?", check which entrypoint the user means.

### Verify "pre-existing" claims against the fork point

When a change surfaces a test failure, do not call it pre-existing
without reproducing it on the fork point. The test for pre-existing
is *can the unchanged baseline reproduce the failure?* — not *does
the failure look unrelated to my changes?*. A change that exposes a
previously-hidden failure (e.g. by loading tests that were silently
skipped before) registers as a regression at the
`bin/sync-packages.sh` exit-code level, even when the underlying bug
is older.

### Bootstrap "Could not find module" early in compile is usually transient

When `/bootstrap` reports a path failing at module 1-of-N with
`Could not find module 'Hydra.Core'` or similar dep-not-built errors
on generated files that clearly exist on disk, the cause is almost
always transient: parallel stack lock contention or OOM from
concurrent host syncs (Java/Python/Haskell building at once). Re-run
that single path with
`bin/run-bootstrapping-demo.sh --hosts <H> --targets <T> --tag retry`.
Don't dig into the generated source first.

### Adapter `cases` over a removed variant: keep remaining cases concrete

When removing a variant from a union (e.g., dropping `bigfloat` from
`FloatType`/`FloatValue`), an adapter like `prepareFloatType` that uses
`prepareSame` as a default needs explicit concrete cases for the
*remaining* variants — not `Nothing`. Without concrete arms, DSL
inference makes the rep function polymorphic (`forall t. t -> t`),
and Java codegen emits `Function<T0, T0>` which doesn't unify with
concrete `FloatValue` callsites. Symptom:
`incompatible types: Object cannot be converted to FloatValue` at the
adapter callsite. Fix: list each remaining variant with an explicit
`inject _Variant _variant_name` identity arm.

### Digest conflicts on staging merges

When merging staging into a feature branch, all `dist/**/digest.json`
and `dist/json/digest.main.json` files conflict if both branches
touched DSL sources. Both sides' hashes are wrong post-merge — the
correct hashes depend on the merged source state. Resolution: take
`--ours` to satisfy git, complete the merge commit, run `/sync` to
regenerate, then commit the digest deltas as a follow-up
"Regenerate digests after staging merge" commit. Don't try to merge
hash maps by hand.

### `run-benchmark-tests.sh` Python leg needs `.venv`

`bin/run-benchmark-tests.sh` invokes `heads/python/.venv/bin/python -m
pytest` if that interpreter exists and falls back to bare `python3`
otherwise. The fallback usually lacks `pytest`, so every Python rep
exits with `No module named pytest` and writes a stub JSON — the
script then proceeds to other hosts and the wrapper still reports
exit code 0. Run `cd heads/python && uv sync` once before benching so
the venv exists.

### `bin/benchmark-dashboard.py` throws `KeyError: 'path'`

The per-host kernel-test JSON written by `run-benchmark-tests.sh` has
heterogeneous schema: Haskell only populates `summary.totalTimeMs`;
Python populates per-group `totalTimeMs`; Common-Lisp leaves most
fields zero. The dashboard assumes every group entry has a `path`
field and crashes mid-render. Until that bug is fixed, capture wall
time directly with a small driver script rather than relying on the
dashboard rollup.

### Sibling-worktree builds skew bench numbers

A heavy `stack`, `ghc`, `update-json`, `bootstrap-from-json`,
`JavaSelfHostDemo`, or `pypy3 -m hydra.bootstrap` running in any
other worktree easily takes load average from ~3 to 10+ on a
10-core machine. Haskell numbers in particular are very sensitive
because the bench step does its own `stack test` build. Before
running `bin/run-benchmark-tests.sh`, `bin/run-inference-bench.sh`,
or `bin/run-bootstrapping-demo.sh`: check `uptime` and
`ps aux | grep -iE 'update-json|ghc-9|bootstrap-from-json|stack
build|JavaSelfHost|hydra.bootstrap'` across all worktrees, and
either wait for sibling activity to clear or warn the user that
numbers will be pessimistic.

### Synthesized TermDefinitions must carry a TypeScheme

`moduleToSourceModule` and any other site that synthesizes a
`TermDefinition` wrapping a statically-typed term (e.g. `encoderFor _T @@ m`
of type `T`) must populate `termDefinitionTypeScheme` with that type, not
`Nothing`. If left as `Nothing`, downstream code that loads the module
(notably `bootstrap-from-json`'s synthesis pass) is forced to call
`inferModulesIO` to derive the type from a large encoded term, which on a
16 GB CI runner OOMs after Phase 3 ("Synthesized N encoder source modules")
with no log output before the watchdog kill.

Symptom: silent SIGKILL during the Haskell CI job's step 5 in the
post-synthesizer phase, ~15-25 min in. Fix: annotate at synthesis time
(see `packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Terms/Generation.hs:583-591`
for the canonical pattern), and skip the downstream `inferModulesIO` call
since the type is already known. Bug #367 (#367's CI hang) was this.

### Java rollup needs matching DSL exclude when target syntax is excluded

When `packages/hydra-java/build.gradle`'s `compileJava` block excludes a
generated subtree like `**/hydra/scala/**` because its types don't
type-check standalone, the matching `**/hydra/dsl/<lang>/**` must also
be excluded. After #297's `5032f8038` (regenerate
`Hydra/Dsl/<lang>/Syntax.hs` for every coder package), each coder package
emits a DSL wrapper at `dist/java/hydra-<lang>/src/main/java/hydra/dsl/<lang>/Syntax.java`
that imports `hydra.<lang>.syntax.*`. If the target syntax is excluded,
the DSL wrapper compile fails with "package hydra.<lang>.syntax does not
exist".

Currently only `hydra/scala/**` triggers this — the other languages'
Java emission type-checks standalone. New coder additions that need
`hydra/<lang>/**` excluded should add `hydra/dsl/<lang>/**` at the same
time.

### Background `stack build` exit code is masked by trailing pipes

Running `stack build ...; echo "EXIT=$?"` inside a `run_in_background:true`
Bash call returns `0` from the wrapper as long as the *final* command in the
chain succeeds — even if stack itself failed. The task-notification's "exit
code 0" reflects the wrapper, not stack. Always capture stack's exit into a
variable before any subsequent command (`stack build ... > /tmp/log; STACK_EXIT=$?; echo "STACK_EXIT=$STACK_EXIT"`),
then read the variable from the task output. Otherwise red builds look green.

### Union-arm record names can collide with sum-ctor names after rename

Hydra generates two top-level Haskell names per union arm pointing at a record:
the sum-ctor `<Parent><ArmCamel>` (no underscore) and the arm's referenced
record type, named verbatim from the DSL source. If the DSL author chose a
record name like `Data_Apply` to keep it distinct from `DataApply` (the auto-generated
sum-ctor), stripping the underscore (`Data_Apply` → `DataApply`) causes GHC
"Multiple declarations" errors. Workaround: rename arm records using the
arm-then-parent convention (`Data_Apply` → `ApplyData`), matching the existing
Haskell/Java model style (`ApplicationExpression`, `RecordConstructor`).

### Hydra Core name collisions in target-language coder aliases

Target-language coder modules (e.g. `Hydra.Sources.Scala.Coder`) often re-export
arm constants via local aliases like `_FunctionType = Scala._FunctionType`. If a
local alias shadows a `Hydra.Kernel` export of the same name — and Hydra Core
exports many `_TypeFoo` / `_ExpressionBar` constants — GHC reports
"Ambiguous occurrence." Affected constants include `_FunctionType`,
`_LambdaType`, and (after renames in #297) `_Type_function`. Fix: drop the
local alias and qualify references with `Scala.` at use sites.

### Lazy primitives: detect head through TypeApplication erasure

When adding a new target-language coder, you'll need to wrap the lazy positions
of `ifElse`/`cases`/`maybe`/`fromMaybe`/`fromLeft`/`fromRight`/`findWithDefault`
in nullary thunks at call sites (see [Lazy evaluation and thunking](../docs/recipes/new-implementation.md#lazy-evaluation-and-thunking)).
To do that, your coder needs to identify the primitive being called by walking
the application spine and asking "is the head a `Term_variable` with one of these names?"

The catch: polymorphic primitives are wrapped in `Term_typeApplication` (and
sometimes `Term_annotated`) layers in the kernel JSON. A naive `Term_variable`-only
matcher will skip nearly every kernel call to `ifElse` (which is `forall a. Bool ->
a -> a -> a`) because the head is actually `TypeApp(Var "ifElse", T)`, not
`Var "ifElse"`. Your head-finder must erase those wrappers. Symptom if you forget:
debug markers show `name=NONAME argc=3` for every ifElse call and zero LAZY wrappings
get emitted, even though detection "works" for monomorphic functions like
`hydra.inference.inferTypeOfTerm`.

### Porting `hydra.lib.math.range` to a new host: it's inclusive

Haskell's `[a..b]` is **inclusive on both ends** and Hydra's `math.range`
follows that: `range 1 3 = [1, 2, 3]`, `range 1 1 = [1]`, `range 2 1 = []`.
The natural JS/Python loop `for i in a..<b` produces the exclusive variant,
which silently breaks anything that depends on it. The biggest surprise: the
kernel's `etaExpandTypedTerm` uses `range(1)(needed)` to generate fresh
variable names — an exclusive impl makes eta expansion a no-op for partial
applications, which in turn fails ~25 eta-expansion tests and any downstream
reduction that depends on canonical forms. Always derive the loop bound from
`<= b`, not `< b`.

### Porting CanonMap/CanonSet: `toList` must sort by original key, not by canonical string

The CanonMap/CanonSet trick stores entries keyed by a string canonicalization
(JSON-stringified). When implementing `toList`/`keys`/`values`, the temptation
is to sort entries by their canonical string for stability. This is wrong for
numeric keys: lex-sorting `"n:10"` and `"n:2"` puts `10` before `2`. Sort by
the original key (numeric for numbers, lex for strings) and only fall back to
the canonical string for object/structural keys. Symptom: topological sort
output and any `Map<Int, _>` test fixture comes out in lex order instead of
numeric order; downstream algorithms (SCC, eta expansion) that consume `keys`
also produce subtly wrong output.

### Primitive type schemes must carry class constraints

The kernel inference reads `typeScheme.constraints :: Maybe (Map TypeVariable
ConstraintSet)` for each primitive and threads those constraints through
unification. Built-in primitives like `hydra.lib.maps.lookup` carry
`(ordering k) =>`; `hydra.lib.lists.sort` carries `(ordering a) =>`;
`hydra.lib.equality.equal` carries `(equality a) =>`. Omit them (`constraints:
{tag: "nothing"}`) and inference still *runs*, but emits the wrong scheme
(no constraints) for downstream uses — tests like `(forall t0. (ordering t0)
=> ...)` fail because the actual scheme is missing the constraint clause. See
`heads/java/src/main/java/hydra/dsl/Types.java` (`ORD`, `EQ`, `NONE`,
`constrained1..4`, `schemeOrd`, `schemeEq`) for the canonical per-primitive
constraint assignments to mirror.

### Kernel-loaded TypeSchemes encode polymorphism in the body, not the variables list

When loading TypeSchemes from `dist/json/hydra-kernel/`, polymorphic types
like `hydra.coders.Coder` arrive as `{variables: [], body: annotated(forall t0.
forall t1. record {...})}`. Inference reads `ts.variables` to know how many
type vars to instantiate, so a `variables: []` scheme is treated as
monomorphic and downstream nominal-type lookups fail with confusing errors.
At load time, walk `ts.body` through `annotated`/`forall` layers and promote
any forall-bound parameters up to `ts.variables`. Python does this via
`f_type_to_type_scheme`; TypeScript via `testEnv.collectForallVars`. Without
this step you'll see `<<inference error>>` for any term that case-matches on
a kernel union type (CoderDirection, etc.).

### Coder `encodeLiteral` must cover all six Literal arms

A `Literal` has six arms: `binary`, `boolean`, `decimal`, `float`, `integer`,
`string`. When writing a new target-language coder's `encodeLiteral`, you
must handle all of them (or at minimum, set the default to something that
crashes loudly rather than emitting a sentinel like `null` or `0`). The
binary and decimal arms are the easy ones to forget — most test fixtures use
strings/ints/floats. Symptom: source emission silently drops binary content
or decimal precision, and the runtime sees `value: null` (or `0`) instead of
the actual literal. Round-trip tests like `binaryToString (binary "hello")`
fail with empty output. The TypeScript coder shipped this bug initially —
see commit `8a91da78e` for the fix template.

### Test runner should honor the `disabled` tag

The Hydra test fixtures include several inference tests intentionally tagged
`{value: "disabled"}` because they exercise unresolved upstream limitations
(let-polymorphism over-generalization, Y-combinator typing, etc.). A naive
test runner reports these as failures, masking real regressions. The Python
runner skips them via `is_disabled(tcase)`; new heads should mirror that
behavior. The related `disabledForMinimalInference` tag is *not* a universal
skip — it only applies to heads using the minimal inference variant.
