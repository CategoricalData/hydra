# Generating code with Hydra

A guide to generating source code in target languages from Hydra module definitions.

## Prerequisites

- Familiarity with Hydra's module system (see [Concepts](https://github.com/CategoricalData/hydra/wiki/Concepts))
- A working build environment for at least one host language (see the per-package READMEs under `packages/`)

## Overview

Hydra modules can be defined in any host language's DSL and generated
into any target language (Haskell, Java, Python, Scala, TypeScript, Lisp dialects,
plus schema-only targets like Coq, Rust, GraphQL, etc.).
There are two source representations for modules:

- **DSL modules** are human-authored source code, designed for readability
  and maintainability. They live under `packages/<pkg>/src/main/<lang>/`
  and are consumed directly by the corresponding host's `writeXxx` /
  self-host driver.
- **JSON modules** are a language-neutral interchange format, exported
  from DSL modules. They live under `dist/json/<pkg>/src/main/json/`
  and allow any host language to load modules and generate code
  without depending on the original authoring language. The
  `bootstrap-from-json` CLI consumes these.

The typical workflow is:

```
DSL modules  ---(Phase 1)--->  JSON modules  ---(Phase 2)--->  generated code
(packages/)                    (dist/json/)                    (dist/<lang>/)
```

The authoring host language varies per package:

| Package | Source of truth (Phase 1) | Self-host script |
|---------|---------------------------|------------------|
| hydra-kernel | Haskell (`src/main/haskell/Hydra/Sources/Kernel/`) | `heads/haskell/bin/sync-haskell.sh` (Phase 1 of) |
| hydra-haskell | Haskell (`src/main/haskell/Hydra/Sources/Haskell/`) | (same as above) |
| **hydra-java** | **Java (`src/main/java/hydra/sources/java/`)** | `bin/generate-hydra-java-from-java.sh` |
| **hydra-python** | **Python (`src/main/python/hydra/sources/python/`)** | `bin/generate-hydra-python-from-python.sh` |
| hydra-scala, hydra-lisp, hydra-pg, hydra-rdf, hydra-ext, ... | Haskell (`src/main/haskell/...`) | (Haskell Phase 1) |

> **Legacy backup for hydra-java and hydra-python:** the Haskell-DSL versions
> of these two packages' coder modules still live under
> `packages/hydra-{java,python}/src/main/haskell/Hydra/Sources/{Java,Python}/`.
> They produce byte-identical Phase-1 output to the host-native sources and
> served as a fallback through the 0.15 line. The main sync sequence
> (`bin/sync.sh`, `bin/sync-all.sh`, the per-language `bin/sync-<lang>.sh`
> wrappers) still drives Phase 1 through the legacy Haskell path until the
> host-native integration lands; explicit Phase-1 regen via the
> `generate-hydra-<lang>-from-<lang>.sh` scripts already uses the host-native
> source. Both copies remained in lock-step through the 0.15 line; the
> legacy backup is scheduled for removal during 0.16 development.

## Per-package layout

Generated code is partitioned across per-package `dist/<lang>/<pkg>/`
trees rather than a single flat `dist/<lang>/` directory. Each package
(hydra-kernel, hydra-haskell, hydra-java, hydra-python, hydra-scala,
hydra-lisp, hydra-pg, hydra-rdf, hydra-coq, hydra-typescript, hydra-wasm,
hydra-ext) owns a range of module names, and the generated output for
those module names lands under that package's directory.

Each package's `package.json` may declare a `targetLanguages` field
restricting which target languages the package is regenerated to. For
example, `hydra-coq` and `hydra-typescript` are coder libraries
implemented only against the Haskell runtime, so their
`targetLanguages` is `["haskell"]`.

`dist/json/` is the tracked source of truth. `dist/haskell/` is tracked
through the 0.15 release to support bootstrapping from a fresh clone.
All other `dist/<lang>/` trees are regenerated from `dist/json/` and
are not checked in.

## The writeXxx functions

All code generation functions follow the same signature pattern:

```
writeXxx(outputDir, universeModules, modulesToGenerate) -> filesWritten
```

- **outputDir**: The base directory where generated files are written.
- **universeModules**: All modules needed for type and term resolution.
  This is the dependency context -- it must include all transitive
  dependencies of the modules being generated, even if those dependencies
  are not themselves being generated.
- **modulesToGenerate**: The subset of modules that actually get written
  to files.

The universe and generate lists are often the same, but they differ
when you generate a subset of modules that depend on others. If the
universe is missing a transitive dependency, generation may fail with
unresolved type references or produce incomplete output.

### Available targets

Full implementations (types + terms):

| Function | Target | Notes |
|----------|--------|-------|
| `writeHaskell` | Haskell | |
| `writeJava` | Java | Hoists polymorphic let bindings to class fields |
| `writePython` | Python | Hoists case statements to let bindings |
| `writeScala` | Scala 3 | |
| `writeClojure` | Clojure | |
| `writeScheme` | Scheme | |
| `writeCommonLisp` | Common Lisp | |
| `writeEmacsLisp` | Emacs Lisp | |

Schema-only (types, no terms):

| Function | Target |
|----------|--------|
| `writeCpp` | C++ headers |
| `writeGraphql` | GraphQL schema |
| `writeJsonSchema` | JSON Schema (2020-12) |
| `writeProtobuf` | Protocol Buffers (Proto3) |
| `writePdl` | Pegasus Data Language |
| `writeRust` | Rust |

### Output conventions

Generated files follow language-specific naming conventions:

| Language | Path pattern | Example for `hydra.core` |
|----------|-------------|--------------------------|
| Haskell | `Prefix/Module.hs` | `Hydra/Core.hs` |
| Java | `namespace/module/Module.java` | `hydra/core/Core.java` |
| Python | `namespace/module/module.py` | `hydra/core/core.py` |
| Scala | `namespace/module.scala` | `hydra/core.scala` |

## The sync scripts

For regenerating Hydra's own implementations, sync scripts automate the
full pipeline. These are the standard developer entry point.

### The three-layer script stack

The sync infrastructure is organized as three layers, with a small set
of orchestrators above them:

| Layer | Script | Role |
|-------|--------|------|
| 1 (transform) | `heads/haskell/bin/transform-haskell-dsl-to-json.sh` | DSL → JSON for one package or `--all` packages |
| 1 (transform) | `heads/haskell/bin/transform-json-to-<lang>.sh` | JSON → target language for one package |
| 2 (assemble) | `heads/<lang>/bin/assemble-distribution.sh <pkg>` | Produce `dist/<lang>/<pkg>/` (calls Layer 1 + post-processing) |
| 2 (assemble) | `heads/<lang>/bin/assemble-all.sh` | Batch: produce every `dist/<lang>/<pkg>/` in one universe load |
| 2.5 (test) | `heads/<lang>/bin/test-distribution.sh` | Run the target's test suite |
| 3 (orchestrate) | `bin/sync-packages.sh` | Phase 1 → Phase 2 → Phase 3 for one or more packages |
| 3 (orchestrate) | `bin/sync.sh` | Matrix tool (`--hosts H,...`, `--targets T,...`); Phase 1 + 2 only, **no tests** |
| 3 (orchestrate) | `bin/sync-all.sh` | Exhaustive run (every package × every target); runs tests via `sync-packages.sh` |

Every-day tasks:

```bash
# Full regen with tests (8 target languages × every package, fails fast)
bin/sync-all.sh

# Matrix prep before a bootstrapping-demo run
bin/sync.sh --hosts haskell,java,python --targets haskell,java,python

# Bootstrapping triad shorthand (host == target == {haskell, java, python})
bin/sync-default.sh

# Single-language wrappers (host == target)
bin/sync-java.sh
bin/sync-python.sh
bin/sync-scala.sh
bin/sync-clojure.sh        # or common-lisp, emacs-lisp, scheme

# Just one package, any target
bin/sync-packages.sh hydra-pg --targets java

# Skip target-language tests (Phase 3)
bin/sync-all.sh --no-tests
```

> **Note on `--hosts` semantics.**
> `bin/sync.sh` and `run-bootstrapping-demo.sh` both accept
> `--hosts` and `--targets` as comma-separated language lists, and
> the flag names line up so the two commands compose — but they
> mean opposite things.
> In `sync.sh`, `--hosts L` names a language whose host capability
> is to be *built*: Phase 4 emits each target's coder into L's
> language, so that after the run L can drive code generation.
> L is an *output* of the script.
> In `run-bootstrapping-demo.sh`, `--hosts L` names an existing
> host to *use* as a generator during the demo. L must already be
> built; it is an *input*.
> The same inversion applies to `--targets`: an emission output
> in `sync.sh`, an expectation about pre-existing dist/ state in
> `bootstrap`.

### Phases and cache model

`bin/sync.sh` runs three phases (Phase 1: DSL → JSON; Phase 2: per-target
assemble; Phase 3: tests). Each phase has its own freshness cache, and a
warm-cache run completes in a few seconds.

For the full model — phase contents, per-phase cache locations, skip
conditions, what invalidates what, and per-target generator stamps —
see [The Hydra build system](../build-system.md#phases-of-binsyncsh) and
[The cache model](../build-system.md#the-cache-model).

The procedural how-to for running sync continues below.

## Host-native self-host scripts

`hydra-java` and `hydra-python` are authored in their own host languages
(Java and Python respectively, with legacy Haskell sources retained as a
backup through the 0.15 line — scheduled for removal during 0.16
development; see [Overview](#overview)). The two corresponding scripts
run Phase 1 directly from the host-native sources, no Haskell required:

```bash
# Regenerate dist/json/hydra-java/ from the Java DSL sources
bin/generate-hydra-java-from-java.sh
bin/generate-hydra-java-from-java.sh --compare        # byte-compare to canonical
bin/generate-hydra-java-from-java.sh --force-rebuild  # rebuild Java host first

# Regenerate dist/json/hydra-python/ from the Python DSL sources
bin/generate-hydra-python-from-python.sh
bin/generate-hydra-python-from-python.sh --pypy       # ~4x faster than CPython
bin/generate-hydra-python-from-python.sh --compare
bin/generate-hydra-python-from-python.sh --force-rebuild
```

Both scripts:
1. Run a prerequisite sync to ensure the `dist/` trees they read from
   are current: `bin/sync.sh` (full) for the Java generator, because the
   gradle rollup imports cross-language `dist/java/hydra-{python,haskell,
   lisp,scala}/`; `bin/sync-python.sh` (scoped) for the Python generator,
   which only reads `dist/python/hydra-{kernel,python}/`. The sync call
   is gated by the `HYDRA_IN_SYNC` env var so that `bin/sync.sh` Phase 5
   invoking these wrappers doesn't recurse. Warm-cache sync is fast
   (~3 minutes for full, less for scoped); cold-cache is self-healing.
   See [`claude/pitfalls.md`](../../claude/pitfalls.md) under "Wrapper
   scripts auto-sync; testers don't" for the full convention.
2. Run the native DSL → JSON driver (`hydra.UpdateJavaJson` via
   `bin/update-java-json.sh` / `bin/update-python-json.py`), which loads the
   kernel universe from `dist/json/hydra-kernel/`, imports/reflects on the
   package's DSL source modules, infers types, and writes the resulting JSON.
3. Optionally byte-compare the new output to the existing
   `dist/json/hydra-{java,python}/` to verify byte-identical reproduction
   of canonical.

Use these when you have edited the Java or Python DSL sources and need to
refresh `dist/json/` from them, or to validate that the host-native sources
and the legacy Haskell sources still agree.

> See [bin/update-java-json.md](../../bin/update-java-json.md) and
> [bin/update-python-json.md](../../bin/update-python-json.md) for
> background on how each driver works internally, including the
> pre-computed type-scheme workaround used for `hydra.java.coder`.

> **Sync integration:** the main sync scripts (`bin/sync.sh`,
> `bin/sync-all.sh`, the `bin/sync-<lang>.sh` wrappers) still drive Phase 1
> via the legacy Haskell pipeline; switching them over to the host-native
> scripts is planned during 0.16 development. Until then, run the
> `generate-...` scripts explicitly when editing the Java or Python DSL
> sources.

## Generating from DSL modules directly

When you have direct access to DSL-defined modules (e.g., in a GHCi
session or a Java program), call the `writeXxx` functions directly.

### Haskell example

```bash
cd heads/haskell
stack ghci --ghci-options='+RTS -K256M -A32M -RTS'
```

```haskell
import Hydra.Generation

-- Generate Java for all main modules
writeJava "../../dist/java/hydra-kernel/src/main/java" mainModules mainModules

-- Generate Python for kernel only
writePython "../../dist/python/hydra-kernel/src/main/python" kernelModules kernelModules

-- Generate GraphQL schema for specific modules
writeGraphql "/tmp/graphql" mainModules [myCustomModule]
```

Module lists are Haskell values from `Hydra.Sources.All`:

| Constant | Contents |
|----------|----------|
| `kernelModules` | Core kernel types and terms |
| `mainModules` | Kernel + standard libraries (encoders, decoders, DSLs) |
| `testModules` | Common test suite modules |
| `hydraExtModules` | All ext modules (coders, domain models) |

### Adding DSL wrapper generation for a new package

`update-json-main` and `update-json-manifest` produce per-package
`hydra.dsl.*` JSON wrappers from a list of "DSL input modules". By
default the list covers `hydra-kernel` and `hydra-haskell`. To enable
wrappers for an additional package (e.g., enabling `hydra-python` so
its source-DSL modules can resolve fully-qualified DSL accessors):

1. Add the package's module list to `dslTypeMods` in
   `heads/haskell/src/exec/update-json-main/Main.hs` and
   `heads/haskell/src/exec/update-json-manifest/Main.hs`.
2. Extend the `packageDslInputModules` dispatch in
   `heads/haskell/src/exec/transform-haskell-dsl-to-json/Main.hs` so
   the per-package split routes the package's modules into
   `dist/json/<pkg>/src/main/json/hydra/dsl/...`.
3. Bust caches once: delete `heads/haskell/.cache/phase1-input-cache.txt`
   and `dist/json/build/digest.json` before the next sync, since these
   exec edits don't otherwise invalidate Phase 1 (see "Phase 1 cache
   doesn't hash `src/exec`" below).

### Coq

Coq output is generated via dedicated executables rather than GHCi.
`dist/coq/` is **not checked into git** — it is fully recreatable:

```bash
cd heads/haskell
stack exec generate-coq         # kernel .v files + lib stubs + _CoqProject
stack exec generate-coq-tests   # common test suite .v files
```

`generate-coq` writes to `dist/coq/hydra-kernel/src/main/coq/` and
`generate-coq-tests` writes to `dist/coq/hydra-kernel/src/test/coq/`.
Hand-written primitive library implementations (axiom stubs with
`Definition`/`Fixpoint` bodies) live under `heads/haskell/src/main/coq/hydra/lib/`
and are copied into `dist/coq/` by `generate-coq`.

## Generating from JSON modules

When you don't have the DSL source (or prefer a language-independent
workflow), load modules from their JSON representation. This is how the
bootstrapping demo works and how non-Haskell hosts generate code.

### JSON directory layout

JSON modules are exported per package under `dist/json/`:

| Directory | Contents |
|-----------|----------|
| `dist/json/hydra-kernel/src/main/json/` | Kernel and main modules |
| `dist/json/hydra-kernel/src/test/json/` | Test modules |
| `dist/json/hydra-haskell/src/main/json/` | Haskell coder modules |
| `dist/json/hydra-java/src/main/json/` | Java coder modules |
| `dist/json/hydra-python/src/main/json/` | Python coder modules |
| `dist/json/hydra-scala/src/main/json/` | Scala coder modules |
| `dist/json/hydra-lisp/src/main/json/` | Lisp coder modules |
| `dist/json/hydra-pg/src/main/json/` | Property graph, TinkerPop, Cypher, GraphSON |
| `dist/json/hydra-rdf/src/main/json/` | RDF, OWL, SHACL, ShEx, XML Schema |
| `dist/json/hydra-coq/src/main/json/` | Coq coder modules |
| `dist/json/hydra-typescript/src/main/json/` | TypeScript coder modules |
| `dist/json/hydra-wasm/src/main/json/` | WebAssembly coder modules |
| `dist/json/hydra-ext/src/main/json/` | Avro, Protobuf, GraphQL, Pegasus, etc. |

Each package directory contains a `manifest.json` listing its modules.
Content-hash freshness state lives separately in the gitignored
`dist/json/<pkg>/build/<set>/digest.json` cache (see [build-system.md](../build-system.md)
"Cache files are not tracked").

### The bootstrap-from-json CLI

The Haskell bootstrap CLI generates code for a target language,
consuming the JSON modules under `dist/json/`. It has three modes:

```bash
cd heads/haskell
stack build hydra:exe:bootstrap-from-json

# Scoped: generate just one package's modules
stack exec bootstrap-from-json -- \
    --target java \
    --package hydra-pg \
    --include-coders \
    --output ../../dist/java

# Batch: generate every package, routed per-package via namespaceToPackage
stack exec bootstrap-from-json -- \
    --target java \
    --all-packages \
    --include-coders --include-dsls \
    --output ../../dist/java

# Flat (demo): every module to a single src/main/<target>/ tree
# (Used by the bootstrapping demo; integration-style layout.)
stack exec bootstrap-from-json -- \
    --target python \
    --include-coders --include-dsls --include-tests \
    --output /tmp/hydra-bootstrap-out
```

Key options:

| Option | Description |
|--------|-------------|
| `--target` | Target language: `haskell`, `java`, `python`, `scala`, `clojure`, `scheme`, `common-lisp`, `emacs-lisp` |
| `--package <pkg>` | Scope to one package's modules (per-package dist layout) |
| `--all-packages` | Batch: iterate every package (per-package dist layout, one universe load) |
| (neither) | Flat layout: everything to `<output>/src/main/<target>/` |
| `--output` | Output base directory |
| `--include-coders` | Also load coder packages (hydra-java/python/scala/lisp) |
| `--include-dsls` | Also load DSL wrapper modules |
| `--include-ext` | Also load long-tail ext packages |
| `--include-tests` | Also generate test modules |
| `--kernel-only` | Only generate kernel modules |
| `--types-only` | Only generate type-defining modules |
| `--synthesize-sources` | Derive `Hydra.Sources.Decode.*` and `Hydra.Sources.Encode.*` from kernel + pg types |
| `--dist-json-root <dir>` | JSON root (default: `../../dist/json`) |

For non-Haskell hosts, each implementation has its own bootstrap
driver (e.g. `hydra.bootstrap` in Python, `hydra.Bootstrap` in Java).
These are invoked by `demos/bootstrapping/bin/invoke-<host>-host.sh`.

### Phase 2 batch generation

Full-universe regeneration for a single target language is most
efficient via the batch assembler:

```bash
# Batch-regenerate every Haskell dist, with hydra-kernel TestGraph patch
heads/haskell/bin/assemble-all.sh

# Same for java / python / scala
heads/java/bin/assemble-all.sh
heads/python/bin/assemble-all.sh
heads/scala/bin/assemble-all.sh
```

These do one `stack exec bootstrap-from-json --all-packages` per
target and then apply per-package post-processing (hydra-kernel's
`TestGraph` patch, hydra-lisp's `Coder.java` fix, etc.).
`sync-packages.sh` dispatches to them automatically when every
package is in scope.

Lisp dialects don't have a batch assembler yet; their Phase 2 loops
per-package.

## Troubleshooting

### Stack overflow during generation

Large module sets require extra memory:

```bash
stack ghci --ghci-options='+RTS -K256M -A32M -RTS'
```

The sync scripts and execs handle this automatically.

### Unresolved type references

The universe modules are missing a transitive dependency. Ensure the
universe includes all modules that the generated modules depend on,
even indirectly. DSL-source-declared `moduleDependencies` must list
every cross-module reference; missing a dep here can cause
`untyped term variable` or `no such element` failures during generation.

### "file not found" for JSON modules

Check that `--dist-json-root` points to the correct directory. The path
is relative to `heads/haskell/`:

```bash
# Default: ../../dist/json (resolves to <worktree>/dist/json/ from heads/haskell/)
stack exec bootstrap-from-json -- --target java --all-packages --output /tmp/out

# Explicit override
stack exec bootstrap-from-json -- --target java --all-packages \
    --dist-json-root /other/path/dist/json --output /tmp/out
```

### Generated files don't compile

Ensure the universe is complete. If generating hydra-ext modules, the
universe must include main modules as dependencies. If generating
tests, the universe must include both main and test dependencies.
Also check that `--include-coders` / `--include-ext` / `--include-dsls`
flags match what the target generation actually references.

### Editing the synthesizer itself (Dsls.hs, the Haskell coder, etc.)

When a kernel source change alters the *behavior of a synthesizer that
produces JSON output* — e.g., editing
`Hydra/Sources/Kernel/Terms/Dsls.hs` (the DSL-wrapper synthesizer) or
`Hydra/Sources/Haskell/Coder.hs` — one `/sync-haskell` is not enough.
Phase 1 builds a new binary with your change, but Phase 2 runs that
binary against the existing JSON inputs to regenerate downstream JSON.
Outputs that depend on the *new* synthesizer behavior (the DSL-wrapper
JSONs, dist Haskell, etc.) need a *second* sync to fully propagate:
the first sync rebuilds the binary, the second runs the new binary
against the now-updated source state.

If you edit a synthesizer and only see partial propagation, run
`/sync-haskell` a second time before debugging.

This is a known limitation of the current cache key, which hashes the
*input source* but not the *transform binary*. The fix is tracked in
[#347 (Merkle trees for cache invalidation)](https://github.com/CategoricalData/hydra/issues/347):
once the cache key incorporates a hash of the synthesizer binary
itself, edits to the synthesizer will invalidate downstream outputs
correctly and a single sync will suffice.

### Phase 1 cache doesn't hash `src/exec`

`bin/lib/check-phase1-fresh.py` hashes the contents of
`heads/haskell/src/main/haskell` and `heads/haskell/src/test/haskell`,
but not `heads/haskell/src/exec`. Edits to executables under `src/exec/`
(e.g., `update-json-main/Main.hs`, `transform-haskell-dsl-to-json/Main.hs`,
`update-json-manifest/Main.hs`) do not invalidate Phase 1, so a sync
after such an edit will reuse cached JSON outputs even though the
producer has changed.

To force a Phase 1 rebuild, delete `heads/haskell/.cache/phase1-input-cache.txt`
before running sync. The Stack build itself will pick up the exec
changes; only the freshness gate is fooled. Likewise, when the change
adds new JSON outputs (e.g., new `hydra.dsl.*` wrappers for a newly
enabled package), wipe the build-cache subtree (`rm -rf dist/json/build
dist/json/*/build`) to force downstream regeneration. The whole
`dist/**/build/` tree is gitignored cache state.

### Renaming a generated module name leaves orphan files in `dist/`

Sync writes new generated files but does not delete files that no
longer correspond to any source. If a DSL module's module name changes
from `hydra.foo.x` to `hydra.bar.x`, the next sync will write
`dist/json/.../hydra/bar/x.json` and `dist/haskell/.../Hydra/Bar/X.hs`
but the old `hydra/foo/x.json` and `Hydra/Foo/X.hs` will remain on
disk and stay tracked in git. The "Checking for new files..." block at
the end of sync only flags additions, never removals.

After any module-name rename, manually `git rm` the orphaned dist files
and verify with `git status` before committing. A regen commit that
ships both the new and the old files inflates the diff and leaves the
old modules as "phantom" code that still gets compiled by Stack but is
never referenced.

### Hand-written test adapters that import generated modules

`heads/<lang>/src/test/...` files that import a generated module
(e.g., `Hydra.Lib.Defaults.*`) cannot be built by `stack test` until
the generated modules exist on disk. After renaming the module name of
such a module, the build sequence is:

1. `stack build` -- verifies the library and execs compile against
   the renamed source. The test target is *not* configured at this
   step, so it cannot fail on the missing generated module.
2. `/sync-haskell` -- regenerates the dist files under the new
   module name, then runs `stack test` itself as part of its final
   verification phase.

Running `stack test` between steps 1 and 2 will fail at the test
adapter with `Could not find module 'Hydra.Lib.Defaults.X'`. This is
expected; defer the test run until after sync.

## Related documentation

- [Exporting modules to JSON](json-kernel.md) -- JSON format, export scripts, verification
- [Creating a new implementation](new-implementation.md) -- Using code generation for a new target language
