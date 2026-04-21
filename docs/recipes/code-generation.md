# Generating code with Hydra

A guide to generating source code in target languages from Hydra module definitions.

## Prerequisites

- Familiarity with Hydra's module system (see [Concepts](https://github.com/CategoricalData/hydra/wiki/Concepts))
- A working build environment for at least one host language (see the per-package READMEs under `packages/`)

## Overview

Hydra modules can be defined in any host language's DSL (today: Haskell)
and generated into any target language (Haskell, Java, Python, Scala,
Lisp dialects, plus schema-only targets like Coq, Rust, GraphQL, etc.).
There are two source representations for modules:

- **DSL modules** are human-authored source code, designed for readability
  and maintainability. They live under `packages/<pkg>/src/main/haskell/Hydra/Sources/`
  and are consumed directly by the `writeXxx` functions.
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

For Hydra's own kernel, the DSL modules are in Haskell, but this is not
a requirement. Any host language's DSL can be the source of truth for a
given set of modules.

## Per-package layout

Generated code is partitioned across per-package `dist/<lang>/<pkg>/`
trees rather than a single flat `dist/<lang>/` directory. Each package
(hydra-kernel, hydra-haskell, hydra-java, hydra-python, hydra-scala,
hydra-lisp, hydra-pg, hydra-rdf, hydra-coq, hydra-javascript, hydra-wasm,
hydra-ext) owns a range of namespaces, and the generated output for
those namespaces lands under that package's directory.

Each package's `package.json` may declare a `targetLanguages` field
restricting which target languages the package is regenerated to. For
example, `hydra-coq` and `hydra-javascript` are coder libraries
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
| Haskell | `Namespace/Module.hs` | `Hydra/Core.hs` |
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
| 3 (orchestrate) | `bin/sync.sh` | Matrix tool (`--hosts H,...`, `--targets T,...`) |
| 3 (orchestrate) | `bin/sync-all.sh` | Exhaustive run (every package × every target, with tests) |

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
bin/sync-packages.sh hydra-pg --target java

# Skip target-language tests (Phase 3)
bin/sync-all.sh --no-tests
```

### Phases

Each `sync-packages.sh` (and therefore each `sync-all.sh`) invocation
runs three phases in order:

1. **Phase 1 — DSL → JSON.** Runs
   `transform-haskell-dsl-to-json --all main` and `--all test` in a
   single Haskell-universe load. Produces (or updates) `dist/json/`.
   Idempotent.

2. **Phase 2 — assemble.** For each target language, either
   - calls `heads/<lang>/bin/assemble-all.sh` once when every package
     is in scope (batch mode: one `bootstrap-from-json` invocation
     handles all packages), or
   - loops per-package over `heads/<lang>/bin/assemble-distribution.sh <pkg>`
     for scoped runs.
   Skips any (package, target) combination outside the package's
   declared `targetLanguages`. Produces `dist/<lang>/<pkg>/`.

3. **Phase 3 — test.** For each target, invokes
   `heads/<lang>/bin/test-distribution.sh`. Fails fast on the first
   failing target.

### Warm-cache caching

Each phase has a freshness cache. When nothing has changed relative to
the last successful run, every step short-circuits and `sync-all` completes
in a few seconds.

| Phase | Cache | Skip condition |
|-------|-------|----------------|
| Phase 1 | `bin/lib/check-dsl-fresh.py` | Every DSL source file's hash matches the recorded digest at `dist/json/digest.main.json`. Skips stack startup + JSON regeneration entirely. |
| Phase 2 (batch) | `bin/lib/batch-cache.sh` | Every `dist/<lang>/<pkg>/digest.json`'s recorded input hashes match the current `dist/json/<pkg>/digest.json`. Skips stack startup + `bootstrap-from-json`. |
| Phase 2 (per-pkg) | `heads/haskell/bin/digest-check fresh` + Python pre-check | Per-package input hashes match and every recorded output file exists with its recorded hash. |
| Phase 2 (generator) | Stage 7 per-module DSL-hash skip inside `bootstrap-from-json` | Modules with unchanged DSL-source hashes are excluded from regeneration even when overall cache missed. |
| Phase 3 | `bin/lib/test-cache.sh` (`dist/<lang>/test-cache.json`) | Every generated source under `dist/<lang>/*` plus every hand-written test helper under `heads/<lang>/src/test/*` plus the runner script are byte-identical since the last successful run. Skips `stack test` / `gradle test` / `pytest` / `sbt test` / lisp runners entirely. |

Any single file change invalidates the relevant cache. For example,
editing a DSL source invalidates Phase 1 (regenerates JSON), which
invalidates Phase 2 for whichever packages own the changed namespace,
which invalidates Phase 3 for whichever targets consume those packages.
Targets not reached by the chain stay cached.

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
| `dist/json/hydra-javascript/src/main/json/` | JavaScript coder modules |
| `dist/json/hydra-wasm/src/main/json/` | WebAssembly coder modules |
| `dist/json/hydra-ext/src/main/json/` | Avro, Protobuf, GraphQL, Pegasus, etc. |

Each package directory contains a `manifest.json` listing its modules
and a `digest.json` recording content hashes for the freshness cache.

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
even indirectly. DSL-source-declared `moduleTermDependencies` and
`moduleTypeDependencies` must list every cross-module reference; missing
a dep here can cause `untyped term variable` or `no such element`
failures during generation.

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

## Related documentation

- [Exporting modules to JSON](json-kernel.md) -- JSON format, export scripts, verification
- [Creating a new implementation](new-implementation.md) -- Using code generation for a new target language
