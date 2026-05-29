# Pitfalls and gotchas (extended)

CLAUDE.md keeps a short list of hard rules and a short list of mental models.
This page covers specific gotchas — concrete known-issue notes that don't belong
in the top-level orientation.

> **Reader.** This file is primarily Claude-facing. Several issues here would
> apply to any developer, not just an LLM session — the public, audience-neutral
> versions live in `docs/troubleshooting.md`,
> `docs/recipes/code-generation.md#troubleshooting`, and
> `docs/recipes/maintenance.md`. Check those first if you're looking for the
> shipped form of a workaround. The entries here describe Claude-specific
> session dynamics (shell snapshot heredoc behavior, cross-worktree
> contention, "is this process mine to kill," `pgrep` interpretation, etc.)
> or are scratch-pad notes pending promotion.

## Specific known issues

### Primitive registration

A primitive class can exist but be invisible at runtime if it isn't registered
in `Libraries.java` / `Libraries.hs` / `libraries.py` /
`Libraries.scala` / `libraries.clj`.

Two-tier check (post-#156): the **canonical registry** is the
`PrimitiveDefinition` in `packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Lib/<Sub>.hs`.
The **host-side registries** (e.g. `Libraries.hs` `hydraLib<Sub>` lists)
pair each primitive's universal metadata with a native impl. A primitive
is "unknown" if either:
- The canonical `PrimitiveDefinition` is missing (validator-time error during sync).
- The host registry doesn't bind the name to a native impl (runtime "unknown primitive").

Always check both layers when debugging.

### Primitive `implementation()` must not throw (Java)

Higher-order primitives (those that take function arguments and use
`Reduction.reduceTerm` internally) need a working `implementation()` that
constructs term-level results, not one that throws on missing arg shapes.
See [docs/recipes/adding-primitives.md](../docs/recipes/adding-primitives.md).

### Primitive definition list alphabetical-order trap

The kernel validator (`hydra.validate.packaging`) requires the
`definitions` list in each `Hydra/Sources/Kernel/Lib/<Sub>.hs` module to be
in lexical alphabetical order by primitive name. Numeric suffixes
sort lexically, not numerically: `bigintToInt16` < `bigintToInt32` <
`bigintToInt64` < `bigintToInt8` (because `'1' < '3' < '6' < '8'`).

The validator fails with `definitions out of order: <X> precedes <Y>`.

### Empty `description` field fails the documentation validator

`hydra.validate.packaging`'s documentation rule (`checkDefinitionDocumentation`)
flags any `PrimitiveDefinition` whose `description` is the empty string. The
description is a required field on the type but the validator treats `""` as
"undocumented". When using `toPrimitive` or `primNoDef`, always pass a
non-empty description.

### `unary_function` is shallow — it only extracts the outer call

In `Hydra.Dsl.Meta.Phantoms`, `unary_function f` builds a TTerm representing
a unary lambda by calling `f (var "x")` and pattern-matching the result as
`TermApplication (lhs, _)`, then returning `lhs`. If `f` does more than a
single application (e.g. composes two operations), only the outer-most
function survives; the inner one is silently discarded. The bug manifests
as a type-inference failure that says "unify `<inner-output-type>` with
`<outer-input-type>`" downstream. Use `lam "x" (...)` directly to build a
real lambda body containing nested calls.

### Definition.primitive arm: every Definition consumer needs updating

When adding `DefinitionPrimitive` to the `Definition` union, every site
that does `cases _Definition (var "def") Nothing [...]` with a missing
arm becomes a runtime crash (non-exhaustive pattern). Even with a
`(Just default)` fall-through, semantics are usually wrong for the
primitive arm. Sites to audit in `packages/hydra-kernel/src/main/haskell/Hydra/Sources/`:
`Analysis.hs`, `Environment.hs`, `Generation.hs`, `Validate/Packaging.hs`,
plus `Sources/Test/Generation.hs`. The original kernel migration left
these incomplete and surfaced as a `Non-exhaustive patterns in case`
crash inside `Validate/Packaging.hs:definitionName` during the first
sync after adding the first `hydra.lib.<sub>` module that emitted
primitives.

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
rm -rf dist/json/build dist/json/*/build dist/haskell/*/build
rm -f heads/haskell/.stack-work/bootstrap-from-json-cache.txt
rm -f heads/haskell/.stack-work/verify-json-kernel-cache.txt
heads/haskell/bin/sync-haskell.sh --no-tests
bin/sync-packages.sh <pkg> --targets haskell --no-tests
```

The `dist/**/build/` subtree is gitignored cache state (see #379), so
wiping it is always safe and never affects shared history.

Then sync forward into whatever target language consumes the regenerated coder.

### Native (Java/Python) coder edits and the Phase-5-runs-last ordering

The above is about the *legacy Haskell* coder DSL. The **native** coders
(`packages/hydra-{java,python}/src/main/{java,python}/.../Coder.{java,py}`) are
regenerated in **Phase 5 — the last phase**. A single `bin/sync.sh` runs Phase 2 (build
`dist/haskell/hydra-<lang>` from the *current* `coder.json`) and Phase 3/4 (emit
`dist/<target>` via the binary compiled from it) *before* Phase 5 overwrites
`dist/json/hydra-<lang>/coder.json` with the native output. So in the interim dual-write
state (Phase 1 writes `coder.json` from the legacy Haskell DSL, Phase 5 overwrites it from
the native sources), an edit to `Coder.java`/`Coder.py` would reach `coder.json` this pass
but not `dist/haskell/hydra-<lang>/` (what `bootstrap-from-json` compiles) until the next
pass.

Two separate things used to make this worse; both are fixed:

- **The input digest was incomplete (#400, now fixed).** It hashed only the
  `hydra.dsl.<lang>.*` modules, not the native-owned `hydra.<lang>.*` ones (including
  `hydra.<lang>.coder`), so the freshness gate couldn't even see a native coder change.
  `Hydra.Digest.discoverNamespaceFiles` now also scans the native `.java`/`.py` sources,
  and the per-package input digest hashes the unfiltered universe, so a native edit
  correctly invalidates `dist/json/hydra-<lang>/build/main/digest.json`. (Regression test:
  `heads/haskell/src/test/haskell/Hydra/DigestSpec.hs`.)
- **The one-pass lag.** `bin/sync.sh`'s Phase 5 re-assembles `dist/haskell/hydra-<lang>`
  from the just-written native JSON and re-runs `stack build`, so the binary embeds the new
  coder in the same pass. This re-assemble now flows through the *normal* freshness gate
  (the input digest is honest after the #400 fix) — no output-digest force-drop. The old
  `rm -f dist/haskell/hydra-<lang>/build/main/digest.json` hack has been removed.

`dist/<target>/` (e.g. `dist/java`) is gitignored and regenerated downstream, so it is not
part of the consistency gate and lagging it one pass is harmless.

**Post-0.16 cleanup:** once the legacy Haskell DSL copies for hydra-java/hydra-python are
deleted, the native generators become the sole writers of `dist/json/hydra-<lang>/`. The
native DSL→JSON step should then move *ahead* of Phase 2, and the Phase-5 re-assemble block
can be deleted entirely — there will be no producer-ordering left to reconcile.

### Scoped `bin/sync.sh --hosts X --targets X` is narrow — cross-language dists are not populated

Every per-host sync wrapper (`bin/sync-java.sh`, `bin/sync-python.sh`,
`bin/sync-scala.sh`, the four Lisp dialects, `bin/sync-typescript.sh`,
`bin/sync-go.sh`) just delegates to `bin/sync.sh --hosts X --targets X`.
That scoped invocation populates only:

- **Phase 3** — `dist/X/{hydra-kernel, hydra-pg, hydra-rdf}/`
- **Phase 4** — `dist/X/hydra-X/` (X's own coder in X)

It does **not** populate `dist/X/hydra-{the other languages}/`. So any
downstream consumer that needs the cross-language coder dists for X
will fail until a broader sync has run. To fully populate `dist/X/`,
use one of:

```bash
bin/sync.sh --hosts X --targets all        # every coder dist under dist/X/
bin/sync.sh                                # full all × all (most thorough)
heads/<lang>/bin/assemble-distribution.sh hydra-<other>   # per-package
```

Downstream consumers that trip on this:

- **Java rollup** (`packages/hydra-java/build.gradle`). Both `main` and
  `headsExtras` source sets import `hydra.{haskell,python,scala,lisp,typescript}.*`
  directly. Symptom: `compileHeadsExtrasJava FAILED` with many
  `package hydra.lisp.syntax does not exist` errors.
- **Scala sbt** (`packages/hydra-scala/build.sbt`). Declares
  `unmanagedSourceDirectories` over
  `dist/scala/hydra-{kernel,haskell,java,python,scala,lisp}/...`.
  Symptom: `sbt compile` reports `Type Mismatch Error: Found (Unit =>
  String), Required: String` (or similar) in a generated
  `dist/scala/hydra-<lang>/.../*.scala` file whose mtime predates a
  recent kernel-type change.
- **Layer 2.5 testers** (`heads/<lang>/bin/test-distribution.sh`) for
  any language whose build references cross-target dists, e.g.
  `heads/scala/bin/test-distribution.sh hydra-kernel` triggers the
  Scala sbt issue above.
- **Java/Python Phase 5 self-host** (`bin/generate-hydra-{java,python}-from-{java,python}.sh`).
  The Phase 5 driver compiles the gradle rollup before running
  JavaSelfHostDemo, so it hits the Java rollup issue.

User-callable wrapper scripts that compile cross-language code
(`bin/generate-hydra-java-from-java.sh`,
`heads/java/bin/inference-bench.sh`) **self-heal** — they call
`bin/sync.sh` themselves before invoking gradle, gated by
`HYDRA_IN_SYNC` to avoid recursion. See the next entry for the
convention. Warm-cache full sync is ~3 minutes; cold-cache is whatever
a real first build takes.

> When `/test` lands (issue #387), the per-language `/test X` skill
> will own the right pre-sync scope automatically. Until then, prefer
> `bin/sync.sh --hosts X --targets all` over the host-only wrapper
> whenever the downstream consumer compiles or tests cross-language
> code.

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

Note: `gradle.properties` (anywhere — `heads/java/gradle.properties` or the
repo root) is gitignored and exists as a developer-local escape hatch for
`org.gradle.jvmargs` and other per-developer Gradle config — useful for local
experimentation, but JVM args set there only affect the build daemon, not
forked compiler workers, so it would not have fixed this OOM on its own.

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

### Cross-worktree sync contention can multiply sync time 10×+

Two worktrees running `bin/sync.sh` simultaneously share GHC, Stack, the
machine's CPU, and (transiently) the same `.stack` global cache. A sync
that completes in a few minutes solo can stretch to an hour or more with
a sibling sync competing. The work still succeeds — this is contention,
not corruption — but expect dramatically longer wall-clock times.

Before scheduling a long sync in your worktree, scan for sibling activity:
`pgrep -fl "bin/sync.sh"` lists every active sync across all worktrees.
If another session is mid-sync, prefer waiting unless the user explicitly
authorizes parallel syncs. Don't kill the other process — it belongs to a
different session (see CLAUDE.md "Hard rules").

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

### Digest conflicts on staging merges — no longer applies (#379, merged 2026-05-20)

Historical: digest files used to be tracked and would conflict on every
multi-branch merge because hashes always diverge. As of #379 (merged
2026-05-20) the entire `dist/**/build/` subtree is gitignored, so
digests never enter the diff. Merges should be clean for digests on
any branch that has #379 merged in. If you encounter a digest file in
conflict, you're on a pre-#379 branch — run the post-merge recovery:
`rm -rf dist/**/build` then `bin/sync.sh`.

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

### Stage 7 freshness filter is defeated by per-package assemblers

`bootstrap-from-json`'s Stage 7 reads the per-target digest at
`<outBase>/<pkg>/src/<sourceSet>/digest.json` to skip re-inferring fresh
modules. But every per-package `assemble-distribution.sh` runs
`rm -f "$OUTPUT_DIGEST_MAIN"` (and the test analog) immediately before
invoking bootstrap-from-json, so Stage 7 always sees an empty digest and
keeps all modules. Stage 7 only delivers its speedup to direct callers like
`heads/haskell/bin/sync-haskell.sh`. If you're trying to make a per-package
sync faster by leaning on Stage 7, you're chasing a ghost — fix the digest
ordering or the upstream cache instead.

### `dist/` is mixed generated + hand-written content

Three categories of files coexist under `dist/<lang>/<pkg>/src/{main,test}/<lang>/`:

1. Generator output from `bootstrap-from-json` (most files).
2. Hand-written runtime support copied in by `copy-kernel-runtime.sh`
   (Java + Python only): `hydra/Adapters.java`, `hydra/util/...`,
   `hydra/dsl/...`, etc.
3. Hand-written skip-emit stubs whose namespace appears in
   `testSkipEmitNamespaces` (currently `hydra.test.testEnv`):
   `dist/haskell/.../Hydra/Test/TestEnv.hs` and per-Lisp-dialect
   `test_env.<ext>`. The generator deliberately does NOT write these;
   they're committed in git as hand-written bridge modules that the
   generated test_graph imports.

Any pass that walks the dist tree (prune, manifest, copy) has to keep all
three categories alive. The mechanism for protecting (2) is the
`--keep-paths-from` manifest emitted by `copy-kernel-runtime.sh --manifest`;
the mechanism for (3) is to include skip-emit namespaces in the keep set
(via the pre-filter `testModsForKeep` in bootstrap-from-json).

### `moduleFilePaths` is target-specific; Java is not 1-to-1

`Hydra.TargetFilePaths.moduleFilePaths target m` returns the paths a coder
would write for module `m`. Most targets (haskell, python, scala, go, all
lisp dialects) emit one file per namespace via `Names.namespaceToFilePath`
with a target-specific case convention. **Java is the outlier:** it emits
one file per top-level definition that passes `Predicates.isNominalType`
(filtering out type-only aliases) plus an `<Module>Elements` interface file
when the module has any term defs. Code that needs the path set for a
module — prune, manifest generation, future cache work — has to either call
`moduleFilePaths` or replicate this filter. Don't assume one path per
module.
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

### `tryIncrementalInference` dirty set must be filtered by `targetMods`

`Hydra.Generation.tryIncrementalInference` takes two `[Module]` arguments:
`universeMods` (full type-resolution context) and `targetMods` (the
modules the caller is authorized to write). The two are not the same —
`update-json-main` deliberately excludes native-owned namespaces from
`targetMods` (currently `hydra.java.*` and `hydra.python.*` per #344;
those are produced by native generators under `bin/generate-hydra-{java,
python}-from-{java,python}.sh`, not by the Haskell DSL).

When extending the dirty-detection logic in `tryIncrementalInference`,
intersect the final dirty set with `targetMods` before partitioning. The
transitive-closure walk over `moduleDependencies` (added in #347) runs
on `universeMods` for correctness — modules outside `targetMods` can be
upstream of dirty modules — but only modules in both the closure *and*
`targetMods` should be re-inferred and re-written. Without this filter,
the legacy Haskell-DSL copies of native-owned packages clobber the
native generators' canonical JSON output on disk.

The `cleanMods` set (modules whose typed JSON is loaded for inference
context) should still span `universeMods \\ dirtyMods` — wider than
`targetMods` — so cross-package type references resolve.

### Hand-written runtime files in `heads/<lang>/` clobber generated kernel
### modules with the same name

`bin/sync-typescript.sh` (and `bin/copy-kernel-runtime.sh` more generally)
copies `heads/typescript/src/main/typescript/hydra/*.ts` into
`dist/typescript/hydra-kernel/src/main/typescript/hydra/`. If the
hand-written tree contains a file whose name matches a generated kernel
module (e.g. `core.ts`), the copy SILENTLY OVERWRITES the generated
file. Symptoms: cascading TS2305 "Module 'X' has no exported member
'Term'/'Type'/…" at every site that imports kernel types from
`./core.js`, plus runtime "Cannot read .tag of undefined" because the
runtime's structural shape no longer matches what the kernel emits.

Fix: rename the hand-written file. In #126 the hand-written
`heads/typescript/src/main/typescript/hydra/core.ts` was renamed to
`runtime.ts`; the corresponding `copy-kernel-runtime.sh` loop was
updated. Any future head should pick a name that cannot collide with
the kernel's namespace (e.g. `hydra.<lang>.core`, `hydra.<lang>.context`,
etc.).

### dist trees need a `package.json` with `"type": "module"` for NodeNext

When a generated dist tree under `dist/typescript/hydra-kernel/` is
checked by `tsc --moduleResolution nodenext`, tsc walks up looking for
the nearest `package.json` to decide whether `.ts` files compile as
ESM or CommonJS. Without a `package.json` in the dist subtree, tsc
walks past the worktree root and lands on `/Users/<you>/package.json`
or fails entirely — at which point any `import.meta` reference fails
with TS1470 ("not allowed in files which will build into CommonJS
output"), and the runtime imports are treated as CJS.

`heads/typescript/bin/copy-kernel-runtime.sh` writes a minimal
`{"name":"hydra-kernel-dist","private":true,"type":"module"}` into
`dist/typescript/hydra-kernel/` for exactly this reason. Other heads
that grow a similar dist subtree need the same.

### TS coder: hand-edit the dist Syntax.hs to bootstrap a new AST node

Adding a new variant to a TS Syntax binding (e.g. `Expression_asExpression`
for #126) follows the standard Hydra bootstrap pattern but is non-obvious:

1. Add the variant to `packages/hydra-typescript/src/main/haskell/Hydra/Sources/TypeScript/Syntax.hs`.
2. Add the matching constructor + `_X` Name to
   `dist/haskell/hydra-typescript/src/main/haskell/Hydra/TypeScript/Syntax.hs`
   by hand (the dist file is generated, but stack-built coder/serde code
   that *uses* the new variant won't compile until the dist Haskell
   declares it).
3. `stack build hydra:lib` — compiles the coder/serde code.
4. `bin/sync-typescript.sh` — regenerates the dist Syntax.hs, overwriting
   the hand-edit. This step is required to keep the JSON canonical.

Step 2 will be reverted on every sync, so it's a per-edit ritual, not
a permanent patch. See [Extending Hydra core](../docs/recipes/extending-hydra-core.md)
for the general bootstrap pattern; the TS-specific wrinkle is just
that the AST lives in `Syntax.hs`, not the kernel.

### Lazy `readFile` keeps the handle open across a subsequent `writeFile`

Standard Haskell pitfall, but it specifically bit `readPerPackageDigest`
during #347: `readFile` returns a lazy String backed by an open handle,
which isn't closed until the string is fully consumed. If the caller
then immediately calls `writeFile` on the same path, the write fails
with "resource busy (file is locked)".

Fix: force evaluation of the read content before returning, e.g.
`length s \`seq\` return (parse s)`. The `parse` call alone is not
enough — laziness in the parser means the underlying string may not
be fully consumed even after parsing nominally completes.

Affects any `readDigest`/`readPerPackageDigest`/`readDigestV2`-style
function in `Hydra.Digest` that might be followed by `writeFile` to
the same path during a single `update-json-main` run.

### `pgrep` + frozen log doesn't mean `bin/sync.sh` died

During Phase 2's stack builds the parent `sync.sh` shell is blocked in
`wait` on a `stack exec ...` child, and stdout is buffered inside
GHC's runtime. The result: `pgrep -fl "<branch>.*sync"` can return
empty (the matching process at that moment is a child not matching
your filter) and `/tmp/<log>` can sit unmodified for many minutes,
while the sync is still happily compiling. Don't relaunch in panic;
that just produces a second contending sync. The authoritative "is it
still running" signal is the background-task completion notification.
If you must check directly, `pgrep -fl "stack\\|sync.sh\\|update-json"`
under your worktree CWD is more reliable than filtering on the branch
name.

### Branches forked from pre-#379 staging regen to obsolete digest paths

Until #379 landed on `main`, sync-pipeline digests lived under
`dist/json/<pkg>/src/main/digest.json` (tracked). #379 moved them to
`dist/json/<pkg>/build/main/digest.json` and gitignored the subtree.

A feature branch forked from staging *before* #379 propagated still
runs the old code, so every Phase 1 run writes digests at the old
paths. That looks like a tracked-file regression on `git status` even
though no real change happened — the regen is catching up to the
committed *content* but writing to the wrong *path*.

When you see modified `dist/json/<pkg>/src/main/digest.json` files on
a feature branch, check `git merge-base --is-ancestor 443e036c36
HEAD`. If "NOT ancestor," merge `main` first, then re-run sync — the
old-path files will get deleted by the merge and the new path will be
gitignored.

### Cross-host C3-style renames: places to sweep beyond the schema

When a kernel type or field renames (Namespace→ModuleName,
Module.namespace→name, Projection.field→fieldName, etc.), the Haskell
compile catches the kernel-side and Java-side breakage cleanly. But
Python and Scala are looser — and bin/ scripts and string-literal
projection sites slip past the type checker. After updating the schema
and sync passes Phase 1, expect a second wave of issues in:

- **DSL bodies with stringly-typed projection paths.** Calls like
  `project("hydra.core.Projection", "field")` (Java),
  `_proj("hydra.core.Projection", "field", "proj")` (Python), and
  `Testing.java`'s `project("hydra.packaging.Module", "namespace")`
  encode the OLD field name as a literal. Inference fails late with
  `NoMatchingFieldError(field_name=Name(value='field'))`.
- **`[ModuleDependency]` lists with stray bare `ModuleName` entries.**
  e.g. `[LEXICAL_NS] + KERNEL_TYPES_NAMESPACES` after the C3 rename
  builds a list of mixed types; Python silently accepts it and inference
  later barfs with `'ModuleName' object has no attribute 'module'`. The
  fix is to wrap with `unqualified_dep(LEXICAL_NS)` — and to add the
  helper import. Two patches in `language.py` (only file affected for
  hydra-python).
- **`QualifiedName.namespace` accessors on host code.** Only the *type*
  renamed to `ModuleName`; the *field* on `QualifiedName` is
  `module_name` (Python) / `moduleName` (Haskell/Java/Scala). Code like
  `qname.namespace` in `phantoms.py` and equivalents needs the field
  rename, not the type rename.
- **`Packaging.un_namespace` / `Packaging.unNamespace` accessors.** Now
  `un_module_name` / `unModuleName`. Easy to miss in host runtime code.
- **`m.name.startsWith(...)` in Scala.** `ModuleName` is a wrapper, not
  a `String`. Needs `m.name.value.startsWith(...)`.
- **Regex literals in bin/ scripts.** `bin/lib/check-dsl-fresh.py` has
  patterns like `r'ns\s*=\s*Namespace\s*"..."'` that need updating to
  match the new type name. Any rename to a type that appears in a
  source-file regex anywhere under `bin/` is a candidate.
- **Heads runtime accessors.** `heads/python/src/main/python/hydra/`
  contains hand-written helpers like `generation.py:228` (filter on
  `m.namespace.value`) and `bootstrap.py` that all need the field
  rename — these are checked at *use* time, not import time, so they
  pass Python import but break Phase 1.

The full sync cycle for cross-host C3 took ~10 iterations before all
runtime callsites were caught; each iteration was a 15-minute sync
exposing the next callsite. After the schema-side sync passes Phase 1,
proactively grep across `packages/hydra-{java,python,scala}/src/`,
`heads/{java,python,scala}/`, and `bin/` for the OLD field/type name
before relying on sync to surface it.

A separate chicken-and-egg surfaces on **warm trees** after the rename
partially propagates: `dist/json/hydra-{java,python}/.../<lang>/*.json`
is owned by Phase 5 (#344), but Phase 2 reads those JSONs early to
assemble the Haskell dist for hydra-java / hydra-python. If the JSON
references the OLD type name but the kernel has the new one, Phase 2
fails with `bootstrap-from-json: ... no such element:
hydra.packaging.Namespace` before Phase 5 ever runs to refresh the JSON.
Recovery:

1. Delete the cold-start sentinels:
   `rm dist/json/hydra-{java,python}/src/main/json/hydra/{java,python}/*.json`
2. Bust the Phase 1 freshness gate (otherwise Phase 1 short-circuits):
   `rm heads/haskell/.stack-work/phase1-input-cache.txt`
3. Re-run `bin/sync.sh`. Phase 1 sees the missing sentinels, sets
   `HYDRA_INCLUDE_JAVA_PYTHON=1`, regenerates the JSON from the Haskell
   DSL (which is the up-to-date source for the rename). Phases 2-4 then
   succeed; Phase 5 overwrites with the native generators' output.

Stale `dist/<lang>/hydra-kernel/<old-type>.{java,py,...}` files are a
sibling symptom: `--prune-stale` in `assemble-distribution.sh` does NOT
detect renamed kernel types as stale (it only prunes files no longer
referenced *anywhere* in the manifest, and the file's basename is the
type name not a path). Manual `rm` is required; safe because the next
assemble regenerates the new-named file.

### Deleting a `dist/json/<pkg>/build/main/digest.json` causes Phase 2 silent exit

`assemble_refresh_digest` in `bin/lib/assemble-common.sh` is gated by
`[ -f "$input_digest" ] && (cd ... && stack exec digest-check refresh ...)`.
Under `set -e`, when the input digest file is missing the `[ -f ]` returns 1
and the whole `&& (...)` returns 1, killing the calling `assemble-distribution.sh`
silently — no error to stderr, no "FAILED" banner. The next package in the
sync loop never runs.

Symptom: `bin/sync.sh` Phase 2 exits EXIT=1 after writing the first package's
files ("Done: N main files") with no error message and no Phase 3 banner.

Recovery: when you nuke a `dist/json/<pkg>/build/main/digest.json` to force
regen, also nuke `heads/haskell/.stack-work/phase1-input-cache.txt`. The
Phase 1 cache miss triggers a full Phase 1 rerun, which regenerates the
json digest as part of `update-json-manifest`. Without busting the Phase 1
cache, Phase 1 stays skipped and the dist/json side never gets a new digest,
so Phase 2 keeps re-failing the same way.

Documented in the build-system cache model
([docs/build-system.md §Cache files are not tracked](../docs/build-system.md#cache-files-are-not-tracked)),
but the silent-exit mechanism deserves the explicit pitfall callout.

### "Found untyped bindings (after case hoisting)" usually means stale JSON field shapes

A non-baseline package whose `dist/json/.../*.json` files were generated
before a kernel record-field rename can carry stale field names that match
no current `TermDefinition` shape. The Haskell decoder silently falls back
to "untyped" and Phase 1 eventually fails with `Found N untyped bindings
(after case hoisting): ...`. Seen during the #368 merge: `dist/json/hydra-java/...`
JSON files still had `"typeScheme": {...}` while `TermDefinition` had renamed
the field to `"signature"` (#156), so every Java/Python definition lost its
type and downstream inference saw a wall of untyped bindings.

Recovery: regenerate the affected packages' JSON. If the rename only affects
Java/Python packages (which native generators own per #344), the targeted
fix is `bin/update-json-main --include-java-python` after busting the input
caches. For other packages, run `assemble-distribution.sh <pkg>` explicitly.
Resist the urge to patch one untyped binding at a time — the field rename
hit every record in the package.

### Post-merge bootstrap patches when cached binaries expect old field shapes

When merging a branch that renames a generated record field into a target
branch with a stale `.stack-work/` cache, source DSL changes propagate
cleanly but the cached `update-json-main` / `bootstrap-from-json` /
`digest-check` binaries still encode the *old* field shape. Running `/sync`
then fails in Phase 1 because the binaries can't decode the new JSON.

Two options:
- **Rebuild the binaries.** `stack build` from `heads/haskell/` is the
  clean fix, but it can be 20+ minutes and may itself fail if the kernel
  is mid-migration.
- **Bootstrap-patch the generated Haskell.** Edit
  `dist/haskell/hydra-kernel/.../Encode/<Type>.hs`,
  `Decode/<Type>.hs`, and any `Inference.hs` call sites so the binary's
  expected field names match. The patch is overwritten by the next clean
  regeneration once the kernel is stable, so it's bootstrap-safe.

During the #368 merge the second path was needed because every patch
attempt to rebuild triggered another wave of kernel-source changes. Trace
the binary's expected shape by reading the relevant
`Sources/Encode/<Type>.hs` and `Sources/Decode/<Type>.hs` at the merge base.

### `git reset --soft` after the user ran their own git commands

If you've been committing across a session, your mental model of HEAD
can lag behind reality. Common case: the user runs `git pull` (or
`git merge`) between your turns. Their merge commit becomes the new
HEAD, and `git reset --soft HEAD~` now undoes *their* merge, not the
commit you intended to amend.

Before any `git reset --soft HEAD~` (or `HEAD~N`), check `git log
-1 --oneline` and `git reflog | head -5`. If the most recent commit
isn't yours, stop and ask. Recovery is straightforward via the
reflog (`git reset --soft <sha-of-the-merge>`) provided you notice
quickly — but the safer rule is "verify before reset," not "recover
after reset."

### `stack build` may relink stale executables even with fresh `.hi` files

Editing a file in `dist/.../Hydra/<Mod>.hs` and running `stack build`
sometimes recompiles the module (fresh `.hi` mtime) but does *not* relink
downstream executables. The executable in
`.stack-work/install/.../bin/<exe>` keeps its old behavior even though the
.hi files have changed. Symptom seen on this branch: a patched
`Hydra/Show/Errors.error` was not visible to `update-json-kernel` after a
clean `stack build`; the old "inference error" message kept appearing.

Fix: `find .stack-work -name <exe> -type f -delete && stack build`. The
forced relink restores the expected behavior. Suspect this any time a
binary's behavior contradicts source you know you edited.

### Bash CWD drifts across foreground `cd` and into background tasks

Documented in memory but worth surfacing here too:
- Foreground `cd /path && cmd` does *not* change CWD for the next
  foreground Bash call. The next call still runs in the worktree root
  (or wherever the shell snapshot left it).
- A foreground `cd` does *not* propagate to a `run_in_background: true`
  Bash invocation either.
- This bites `git`, `stack`, and any path-relative tool.

Default to absolute paths in Bash invocations. If you must `cd`, do it
inside the same single Bash invocation as the work it sets up for.

### Schema-extending `hydra.packaging.Module` or `Package` ramifies into DSL term sources

`dist/haskell/hydra-kernel/.../Sources/Decode/Packaging.hs` and
`Sources/Encode/Packaging.hs` contain 1500–2000 line nested-AST Hydra
*Term* representations of the per-type encoders and decoders, generated
during a prior `bootstrap-from-json --synthesize-sources` run. They are
imported by `Hydra.Sources.Kernel.Terms.All` as `DecodeModule`/
`EncodeModule` and are part of `kernelTermsModules`, so `update-json-kernel`
infers them every run.

Adding a field to `Module` or `Package` (or reordering) is therefore
*not* a localized change: the new field must be threaded through the
DSL term AST in both files, with carefully-matched paren counts. Worse,
`update-json-kernel` may report success while emitting JSON that doesn't
match the on-disk source schema, because the term-DSL files encode their
own (now-stale) view of the schema.

Issue #402 tracks the proper sequencing: (1) reorder existing fields,
(2) add `comments`, (3) populate. Do not attempt this as a sidecar to
unrelated work; the regen pipeline needs investigation/fix in tandem.
