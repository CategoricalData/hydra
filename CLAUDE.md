# LLM quickstart guide for Hydra

This document is designed to get an LLM assistant up to speed on the Hydra project
as quickly as possible. It summarizes the project's architecture, key resources,
common workflows, and pitfalls.

## What is Hydra?

Hydra is a functional programming language based on the LambdaGraph data model.
It explores an isomorphism between typed lambda calculus and labeled hypergraphs:
**programs are graphs, and graphs are programs.**

Hydra is self-hosting: the kernel is defined in Haskell-based DSLs and code-generated
into Haskell, Java, and Python. All three implementations are complete and pass the
common test suite. Version: **0.13.0** (as of 2026-02-05).

Key use cases: graph construction (TinkerPop, RDF, SHACL, GQL), data integration
(coders for Protobuf, Avro, JSON, YAML, GraphQL, PDL, CSV/TSV, RDF), and
computational graphs with deep support for polymorphism.

Hydra is used for data modeling, validation, and transforms at Microsoft.
Its closed-source predecessor Dragon was used at Uber.

## Project structure overview

```
hydra/
  hydra-haskell/    # Bootstrap implementation (Haskell). Source of truth.
  hydra-java/       # Complete Java implementation
  hydra-python/     # Complete Python implementation
  hydra-ext/        # Code generators, coders, demos, tools (Haskell)
  hydra-scala/      # Experimental Scala implementation
  hydra-rust/       # Early-stage Rust
  hydra-go/         # Early-stage Go
  hydra-javascript/  # Early-stage JavaScript
  docs/             # Documentation, recipes, Javadoc
  wiki/             # Local checkout of the GitHub wiki (separate repo; may not be present)
```

### The src/main vs src/gen-main pattern

This is the fundamental architectural pattern across all implementations:

- **`src/main/`** -- Hand-written, human-edited code. Edit freely.
- **`src/gen-main/`** -- Generated code. **Never manually edit** -- it will be overwritten.
- **`src/gen-test/`** -- Generated test code. Same rule: never manually edit.

Generated files have a header: "Note: this is an automatically generated file. Do not edit."

## Document index

This section links to every major document in the project with a brief description.

### Root-level files

| Document | Path | Description |
|----------|------|-------------|
| Main README | [README.md](README.md) | Project overview, use cases, links to all major docs |
| Changelog | [CHANGELOG.md](CHANGELOG.md) | Detailed version history; breaking changes, new features, bug fixes per release |
| License | [LICENSE](LICENSE) | Apache 2.0 license text |

### READMEs

| Document | Path | Description |
|----------|------|-------------|
| Hydra-Haskell | [hydra-haskell/README.md](hydra-haskell/README.md) | Bootstrap implementation. Build/test with Stack, GHCi REPL usage, code generation commands (`writeHaskell`, `writeJava`, `writePython`), DSL overview, self-hosting demo, core types reference |
| Hydra-Java | [hydra-java/README.md](hydra-java/README.md) | Complete Java implementation. Gradle build, test commands, code organization, Java code generation from hydra-ext, visitor pattern for ADTs, union type design |
| Hydra-Python | [hydra-python/README.md](hydra-python/README.md) | Complete Python implementation. Setup with uv, pytest, ruff, pyright. Code generation, validation of generated code |
| Hydra-Ext | [hydra-ext/README.md](hydra-ext/README.md) | Code generators, coders, demos, tools. Comprehensive coder reference (Java, Python, C++, Protobuf, GraphQL, JSON Schema, PDL, Scala, Avro, RDF, SHACL, Graphviz). Type mapping tables per target. Sync scripts. Language syntax models and domain models |
| Hydra-Scala | [hydra-scala/README.md](hydra-scala/README.md) | Experimental, on hold. Build with `sbt compile` |
| GenPG demo | [hydra-ext/demos/genpg/README.md](hydra-ext/demos/genpg/README.md) | End-to-end CSV-to-property-graph demo. Runs in Haskell or Python. LLM-assisted schema generation workflow. GraphSON output for TinkerPop-compatible databases |
| Bootstrapping demo | [hydra-ext/demos/bootstrapping/README.md](hydra-ext/demos/bootstrapping/README.md) | Self-hosting validation: 3 host languages x 3 target languages. JSON module interchange, code generation, baseline diffing |

### docs/ directory (checked into git)

| Document | Path | Description |
|----------|------|-------------|
| Hydra lexicon | [docs/hydra-lexicon.txt](docs/hydra-lexicon.txt) | **The single most important LLM reference.** Complete listing of all kernel types, terms, and ~180+ primitive function signatures. Auto-generated from the kernel graph |
| Promoting code | [docs/recipes/promoting-code.md](docs/recipes/promoting-code.md) | Detailed guide for converting raw Haskell to Hydra DSL modules. DSL translation reference, tips, checklist |
| Documentation style guide | [docs/documentation-style-guide.md](docs/documentation-style-guide.md) | Writing conventions for Hydra docs: sentence case headings, 120 char line length, active voice, backtick formatting rules, file naming |
| GitHub Pages index | [docs/index.html](docs/index.html) | HTML landing page for https://categoricaldata.github.io/hydra/ |

### docs/ -- guides and architecture

| Document | Path | Description |
|----------|------|-------------|
| This document | [CLAUDE.md](CLAUDE.md) | LLM orientation guide (this document) |
| Demos | [docs/demos.md](docs/demos.md) | Overview of all four demos: GenPG, Bootstrapping, Avro-to-property-graphs, Metered evaluation |
| Test suite architecture | [docs/test-suite-architecture.md](docs/test-suite-architecture.md) | How the common test suite is structured. TestGraph shared types, module-based organization, meta-level vs term-level DSLs, test case types (checking, inference, evaluation, formatting) |

### docs/recipes/ -- step-by-step developer guides

| Recipe | Path | Description |
|--------|------|-------------|
| Recipe index | [docs/recipes/index.md](docs/recipes/index.md) | Table of contents for all recipes, organized by category |
| Adding primitives | [docs/recipes/adding-primitives.md](docs/recipes/adding-primitives.md) | Add new primitive functions across all 3 implementations. File-by-file checklist. Covers `prim1`/`prim2`/`prim2Eval` registration, eval elements for higher-order primitives, DSL wrappers, tests |
| Extending Hydra Core | [docs/recipes/extending-hydra-core.md](docs/recipes/extending-hydra-core.md) | Add new type/term constructors (e.g., Either). The most complex recipe. 12+ steps covering the bootstrap problem, manual patching of generated files, inference, checking, rewriting, encoding/decoding. Also covers adding fields to existing records |
| Extending tests | [docs/recipes/extending-tests.md](docs/recipes/extending-tests.md) | Add tests to the common test suite. Choosing test modules, writing test cases with `checkWithType`/`inferWithType`/`primCase`, meta-level vs term-level DSL differences |
| JSON kernel | [docs/recipes/json-kernel.md](docs/recipes/json-kernel.md) | Export Hydra modules to JSON for language-agnostic access. `update-json-kernel.sh`, `update-json-main.sh`, `update-json-test.sh`, and `verify-json-kernel.sh`. JSON encoding format details |
| LLM-assisted development | [docs/recipes/llm-assisted-development.md](docs/recipes/llm-assisted-development.md) | Guidelines for using LLMs with Hydra. Lexicon reference, how to regenerate it, property graph generation demo with video walkthroughs |
| New implementation | [docs/recipes/new-implementation.md](docs/recipes/new-implementation.md) | 10-step guide for implementing Hydra in a new language: syntax model, language constraints, coder, serializer, primitives, test runner, DSLs |
| Promoting code | [docs/recipes/promoting-code.md](docs/recipes/promoting-code.md) | Convert raw Haskell to Hydra DSL modules. Incremental hybrid approach. DSL construct mapping (`<~` for let, `match` for cases, `project` for fields). Common pitfalls with lambdas in higher-order functions |
| Refactoring namespaces | [docs/recipes/refactoring-namespaces.md](docs/recipes/refactoring-namespaces.md) | Rename/move a Hydra namespace. 5-phase process across hydra-haskell, hydra-ext, hydra-python, hydra-java. Orphan file cleanup, decoder/encoder module moves |
| Refactoring | [docs/recipes/refactoring.md](docs/recipes/refactoring.md) | Create, rename, move, delete kernel elements and modules. Detailed examples: creating `hydra.hoisting`, changing `Graph.elements` from Map to List |
| Syncing Python | [docs/recipes/syncing-python.md](docs/recipes/syncing-python.md) | Regenerate Python from Haskell. 4 artifact categories, `sync-python.sh` script. Troubleshooting generation errors |

### Wiki (GitHub wiki -- separate repository)

The wiki is a separate Git repository. A local checkout may exist at `./wiki/`,
but these web links are the canonical references:

| Page | URL | Description |
|------|-----|-------------|
| Home | https://github.com/CategoricalData/hydra/wiki | Navigation hub linking to all wiki pages |
| Concepts | https://github.com/CategoricalData/hydra/wiki/Concepts | Core concepts: Type, Term, Element, Graph, Module, Flow monad, primitives, coders, adapters. Type system (System F with HM inference). 7 design principles. Algebraic Property Graphs |
| DSL guide (Haskell) | https://github.com/CategoricalData/hydra/blob/main/docs/dsl-guide.md | Comprehensive Haskell DSL reference. 4 DSL variants (direct, phantom-typed, meta, generated). Operators, precedence, import conventions. Library DSLs. Common errors |
| DSL guide (Java) | https://github.com/CategoricalData/hydra/blob/main/docs/dsl-guide-java.md | Java DSL: `hydra.dsl.Types`, `hydra.dsl.Terms`. Visitor pattern for unions. Flow monad usage. Primitive function packages |
| DSL guide (Python) | https://github.com/CategoricalData/hydra/blob/main/docs/dsl-guide-python.md | Python DSL: `hydra.dsl.types`, `hydra.dsl.terms`. Pattern matching via `match`/`isinstance`. `FrozenDict` for maps. Trailing underscores on reserved words |
| Implementation | https://github.com/CategoricalData/hydra/blob/main/docs/implementation.md | Detailed architecture guide. 21 kernel type modules, DSL system (3 levels, 34+ files), ~180+ primitive functions with `prim`/`primEval` registration. Cross-language coder architecture. Bootstrap process. Extension points |
| Code organization | https://github.com/CategoricalData/hydra/wiki/Code-organization | The `src/main/` vs `src/gen-main/` pattern explained per implementation. What goes where. Never edit generated files |
| Testing | https://github.com/CategoricalData/hydra/wiki/Testing | Common test suite (`hydra.test.testSuite`). Kernel tests vs generation tests. Test categories (primitives, inference, checking, formatting). Per-language test runners. TestGenerator abstraction |
| Benchmarking | https://github.com/CategoricalData/hydra/wiki/Benchmarking | Performance measurement across implementations. Python benchmark tool with CSV output. `hydra_path` for cross-language comparison. Known slow tests |
| Developers | https://github.com/CategoricalData/hydra/wiki/Developers | Source code organization guide. Kernel sources, extended sources, DSLs, primitives. Release processes for Hackage and Sonatype |
| Release process | https://github.com/CategoricalData/hydra/wiki/Release-process | Full release workflow: sync Haskell -> sync Java -> sync Python -> verify -> publish to Hackage/Sonatype/PyPI -> tag. Version file locations. `verify-release.sh` |
| Property graphs | https://github.com/CategoricalData/hydra/wiki/Property-graphs | Algebraic Property Graphs. Mapping annotation keys (`@label`, `@id`, `@key`, `@value`, `@outVertex`, `@inVertex`, etc.). Value patterns. Schema object definition |

## Architecture: how the pieces fit together

### Bootstrap architecture

```
Hydra-Haskell (source of truth)
  |
  |-- Hydra DSL sources define the kernel
  |     (hydra-haskell/src/main/haskell/Hydra/Sources/)
  |
  |-- Code generation produces:
  |     Haskell  -> hydra-haskell/src/gen-main/haskell/
  |     Java     -> hydra-java/src/gen-main/java/       (via hydra-ext)
  |     Python   -> hydra-python/src/gen-main/python/    (via hydra-ext)
  |
  |-- Native primitives implemented per-language:
        Haskell  -> hydra-haskell/src/main/haskell/Hydra/Lib/
        Java     -> hydra-java/src/main/java/hydra/lib/
        Python   -> hydra-python/src/main/python/hydra/lib/
```

### Core types

- **`Type`** -- Structure of data (literals, records, unions, functions, etc.)
- **`Term`** -- Data or computation (instances of types)
- **`Binding`** / **`Element`** -- Named binding (name + term + type scheme)
- **`Graph`** -- Collection of elements with environment, types, primitives, schema
- **`Module`** -- Namespace containing elements with dependencies

Defined in `hydra-haskell/src/main/haskell/Hydra/Sources/Kernel/Types/` and
generated into `Hydra.Core`, `Hydra.Graph`, `Hydra.Module`.

### Type system

Based on System F with Hindley-Milner inference. Supports universal quantification,
type abstraction/application, let-polymorphism. ~180+ primitive functions.

### Error handling and state

Hydra uses `Either (InContext OtherError) a` for computations that can fail.
Debug traces and metadata are carried via explicit `Context` parameters.

### Coders

Bidirectional transformations: `Coder { coderEncode, coderDecode }`.
Language coders live in `hydra-ext/src/main/haskell/Hydra/Ext/Staging/`.

## Demos

Four demos live under `hydra-ext/`. See [docs/demos.md](docs/demos.md) for descriptions and links
to the individual demo READMEs.

- **GenPG** -- CSV to GraphSON property graphs (Haskell, Python, Java; with LLM-assisted workflow)
- **Bootstrapping** -- Everything-to-everything code generation (9 paths, 249 modules)
- **Avro to property graphs** -- Avro JSON to GraphSON or SHACL RDF
- **Metered evaluation** -- Instrumented term reduction with primitive call counting

## Key source directories

### Hydra-Haskell (bootstrap)

| Purpose | Path |
|---------|------|
| DSL syntax definitions | `hydra-haskell/src/main/haskell/Hydra/Dsl/` |
| Kernel type definitions | `hydra-haskell/src/main/haskell/Hydra/Sources/Kernel/Types/` |
| Kernel term operations | `hydra-haskell/src/main/haskell/Hydra/Sources/Kernel/Terms/` |
| Native primitives | `hydra-haskell/src/main/haskell/Hydra/Lib/` |
| Primitive registration | `hydra-haskell/src/main/haskell/Hydra/Sources/Libraries.hs` |
| Code generation utilities | `hydra-haskell/src/main/haskell/Hydra/Generation.hs` |
| Test sources (common suite) | `hydra-haskell/src/main/haskell/Hydra/Sources/Test/` |
| Generated kernel | `hydra-haskell/src/gen-main/haskell/` |

### Hydra-Ext (code generators)

| Purpose | Path |
|---------|------|
| Language coders | `hydra-ext/src/main/haskell/Hydra/Ext/Staging/` |
| Domain models / syntax defs | `hydra-ext/src/main/haskell/Hydra/Ext/Sources/` |
| Demos | `hydra-ext/src/main/haskell/Hydra/Ext/Demos/` |
| Sync scripts | `hydra-ext/bin/` |

### Hydra-Java

| Purpose | Path |
|---------|------|
| Hand-written primitives | `hydra-java/src/main/java/hydra/lib/` |
| Utilities (Maybe, Either, etc.) | `hydra-java/src/main/java/hydra/util/` |
| Generated kernel | `hydra-java/src/gen-main/java/` |
| Generated tests | `hydra-java/src/gen-test/java/` |

### Hydra-Python

| Purpose | Path |
|---------|------|
| Hand-written primitives | `hydra-python/src/main/python/hydra/lib/` |
| DSL utilities | `hydra-python/src/main/python/hydra/dsl/` |
| Generated kernel | `hydra-python/src/gen-main/python/` |
| Generated tests | `hydra-python/src/gen-test/python/` |

## Build and test commands

### Haskell (hydra-haskell)

```bash
# Build
stack build

# Run tests
stack test

# Enter REPL
stack ghci hydra:lib

# REPL with test modules
stack ghci hydra:lib hydra:hydra-test
```

### Java (hydra-java)

```bash
# Build and test (from repo root)
./gradlew :hydra-java:test

# Or from hydra-java/
./gradlew build
./gradlew test

# Specific test
./gradlew test --tests "hydra.VisitorTest"
```

Requires: **Java 17+** (see [Hydra-Java README](hydra-java/README.md))

### Python (hydra-python)

```bash
cd hydra-python

# Setup
uv venv --python 3.12
source .venv/bin/activate
uv sync

# Run tests
pytest

# Lint/format
ruff check
ruff format
pyright
```

Requires: **Python 3.12+**

### Hydra-Ext (code generation)

```bash
cd hydra-ext

# Enter REPL (use extra memory for generation)
stack ghci --ghci-options='+RTS -K256M -A32M -RTS'

# In GHCi:
import Hydra.Ext.Generation
writeJava "../hydra-java/src/gen-main/java" kernelModules kernelModules
writePython "../hydra-python/src/gen-main/python" kernelModules kernelModules
writeHaskell "../hydra-haskell/src/gen-main/haskell" mainModules mainModules
```

## Sync scripts (the standard workflow)

After modifying Haskell sources, use these scripts to regenerate and test:

```bash
# Full sync: Haskell -> Ext -> Java -> Python (from repo root)
./bin/sync-all.sh              # or --quick to skip tests

# Verify release (from repo root)
./bin/verify-release.sh
```

Or run individual phases:

```bash
# Full Haskell self-regeneration (from hydra-haskell/)
./bin/sync-haskell.sh          # or --quick to skip tests

# Regenerate ext Haskell modules and JSON exports (from hydra-ext/)
./bin/sync-ext.sh

# Regenerate Java (from hydra-ext/)
./bin/sync-java.sh             # or --quick to skip tests

# Regenerate Python (from hydra-ext/)
./bin/sync-python.sh           # or --quick to skip tests
```

The order is: **Haskell first, then Ext, then Java and Python.** The `sync-all.sh`
script enforces this order and stops at the first error.

## DSL quick reference

### Haskell DSL operators

| Operator | Meaning | Precedence |
|----------|---------|------------|
| `>:` | Field definition (string key) | 0 |
| `<~` | Let binding | 0 |
| `<<~` | Either bind (monadic) | 0 |
| `@@` | Application (element definitions) | 1 |
| `~>` | Lambda | -- |
| `<.>` | Composition | 9 |

### Import conventions

```haskell
-- Type modules:
import qualified Hydra.Dsl.Types as T
import Hydra.Dsl.ShorthandTypes  -- unqualified: string, int32, list, etc.

-- Term modules (phantom-typed):
import Hydra.Dsl.Meta.Phantoms

-- Library DSLs:
import qualified Hydra.Dsl.Meta.Lib.Lists as Lists
import qualified Hydra.Dsl.Meta.Lib.Strings as Strings
import qualified Hydra.Dsl.Meta.Lib.Math as Math
import qualified Hydra.Dsl.Meta.Lib.Flows as Flows
import qualified Hydra.Dsl.Meta.Lib.Maybes as Maybes
import qualified Hydra.Dsl.Meta.Lib.Logic as Logic
```

### Application styles

- **Primitive functions / DSL helpers**: Applied by concatenation.
  `Strings.cat2 (string "foo") (string "bar")`
- **Element definitions**: Applied using `ref` and `@@`.
  `ref myAddDef @@ int32 1 @@ int32 2`
- **Passing primitives as arguments**: Use `unaryFunction` / `binaryFunction`.
  `Lists.foldl (binaryFunction Math.add) (int32 0) (var "numbers")`

## Common tasks for an LLM assistant

### Promoting code (raw Haskell -> DSL)

This is a frequent task. See [promoting-code.md](docs/recipes/promoting-code.md) for a detailed guide. Key points:

- Only pure code can be promoted (no I/O).
- Use `<~` for let bindings, `match` for pattern matching, `project` for field access.
- `unwrap`/`wrap` for newtypes.
- Hydra has no type classes; use `Lists.map`, `Flows.map`, etc.
- Proceed incrementally -- promote one function at a time.

### Adding primitives

See [adding-primitives.md](docs/recipes/adding-primitives.md). Must touch 6+ files
per primitive across all three implementations. Checklist:

1. Haskell: `Hydra/Lib/<Library>.hs`, `Hydra/Sources/Libraries.hs`, `Hydra/Dsl/Lib/<Library>.hs`
2. Java: `hydra/lib/<library>/<Name>.java`, **`hydra/lib/Libraries.java`**
3. Python: `hydra/lib/<library>.py`, `libraries.py`
4. Higher-order primitives also need eval elements in `Hydra/Sources/Eval/Lib/`
5. Tests: `Hydra/Sources/Test/Lib/<Library>.hs`

#### Primitive registration and implementation (Java)

Each Java primitive is a `PrimitiveFunction` subclass with three methods:
- `name()` -- the fully-qualified Hydra name (e.g., `hydra.lib.flows.withDefault`)
- `type()` -- the type scheme
- `implementation()` -- term-level evaluation logic

The primitive **must be registered** in
`hydra-java/src/main/java/hydra/lib/Libraries.java` in the appropriate
`*Primitives()` method. An unregistered primitive will cause test failures
with "unknown primitive" errors.

#### `prim` vs `prim2Eval` (higher-order primitives)

In `Libraries.hs`, primitives are registered with either `prim1`/`prim2`/`prim3`
(simple) or `prim1Eval`/`prim2Eval`/`prim3Eval` (higher-order). The `Eval`
variants have an additional "eval element" defined in
`hydra-haskell/src/main/haskell/Hydra/Sources/Eval/Lib/`, which provides a
term-level interpreter for the primitive. This is generated into
`hydra-java/src/gen-main/java/hydra/eval/lib/`.

**Both paths matter in Java**: The reducer (`Reduction.java`) calls
`prim.implementation.apply(args)` for all primitives, so even `prim2Eval`
primitives need a working `implementation()` in their Java `PrimitiveFunction`
class. The `implementation()` method must construct a term-level result (not
execute native code). See `hydra-java/src/main/java/hydra/lib/flows/Map.java`
for a good example using the `hydra.dsl.Terms` helpers (`wrap`, `unwrap`,
`lambda`, `app`, `flowState`, `project`, `variable`, `just`, `nothing`, etc.).

### Extending Hydra Core

See [extending-hydra-core.md](docs/recipes/extending-hydra-core.md). This is the most
complex operation due to the bootstrap problem. The code generator must understand
new constructors to generate itself. Requires manual patching of generated files
as an intermediate step.

### Refactoring

See [refactoring.md](docs/recipes/refactoring.md). When changing fundamental types,
update generated files (`gen-main`) FIRST so the project compiles, then update
source files.

## Critical pitfalls

1. **Never manually edit `src/gen-main/` or `src/gen-test/` files** (unless
   doing a bootstrap patch, which must be overwritten by regeneration afterward).

2. **The bootstrap problem**: When extending core types, you face a circular
   dependency. The solution is to manually patch generated files, rebuild,
   then regenerate to overwrite patches.

3. **Missing entries in `Meta.hs` enums** (`TermVariant`/`TypeVariant`) cause
   cryptic "No such field: X" errors during code generation.

4. **Two DSL levels**: Term-level (`Hydra.Dsl.Terms` creating `Term` values) vs.
   meta-level (`Hydra.Dsl.Meta.Phantoms` creating `TTerm a` phantom-typed values
   for modules). Mixing them is a common source of errors.

5. **Python naming**: Trailing underscores on Python reserved words: `T.list_()`,
   `Terms.lambda_()`, `Terms.list_()`. Uses `FrozenDict` instead of regular dicts.

6. **Java unions**: Use the visitor pattern. Prefer `PartialVisitor` when matching
   only a few variants.

7. **Memory for generation**: Python/Java generation needs extra stack.
   Use `stack ghci --ghci-options='+RTS -K256M -A32M -RTS'` or the sync scripts
   handle this automatically.

8. **Haskell must be consistent first**: Always ensure `stack test` passes in
   `hydra-haskell` before syncing Java or Python.

9. **Primitive registration**: A Java primitive class can exist in
   `hydra-java/src/main/java/hydra/lib/` but still be invisible at runtime if
   it isn't listed in `Libraries.java`. Always check registration when debugging
   "unknown primitive" or test failures for newly added primitives.

10. **Primitive `implementation()` must not throw**: The reducer calls
    `implementation()` for all primitives, even higher-order (`prim2Eval`) ones.
    A stub that throws `UnsupportedOperationException` will cause runtime
    failures. Implement it using `hydra.dsl.Terms` helpers to construct
    term-level results.

## Version tracking

The canonical version lives in the `VERSION` file at the repository root. Run `bin/bump-version.sh X.Y.Z`
to set the version and propagate it to all config files. See the [Release process](https://github.com/CategoricalData/hydra/wiki/Release-process) wiki page for the full list of versioned files and the release workflow.

## Debugging tips

### Java test failures

Run tests with `./gradlew :hydra-java:test`. For detailed failure info:

```bash
./gradlew :hydra-java:test --tests "*TestSuiteRunner*" --info 2>&1 | grep -A 20 "FAILED"
```

The test runner is at `hydra-java/src/test/java/hydra/TestSuiteRunner.java`.
It dispatches test cases by type (Evaluation, Inference, Checking, etc.) using
the visitor pattern. Evaluation tests call `Reduction.reduceTerm` and assert
the result matches the expected output.

### Tracing primitive dispatch

When a primitive test fails, the call chain is typically:
1. Test defines a term using `primitive _flows_xxx @@ arg1 @@ arg2`
2. `Reduction.reduceTerm` reduces the term
3. The reducer looks up the primitive in the graph by name
4. It calls `prim.implementation.apply(reducedArgs)` (line ~1277 of `Reduction.java`)
5. The result is further reduced

If step 3 fails (primitive not found), check `Libraries.java` registration.
If step 4 fails, check the `implementation()` method of the primitive class.

### Key files for primitive debugging

| Purpose | Path |
|---------|------|
| Haskell primitive registration | `hydra-haskell/src/main/haskell/Hydra/Sources/Libraries.hs` |
| Haskell eval elements | `hydra-haskell/src/main/haskell/Hydra/Sources/Eval/Lib/` |
| Haskell native implementations | `hydra-haskell/src/main/haskell/Hydra/Lib/` |
| Java primitive registration | `hydra-java/src/main/java/hydra/lib/Libraries.java` |
| Java primitive classes | `hydra-java/src/main/java/hydra/lib/<library>/` |
| Java generated eval elements | `hydra-java/src/gen-main/java/hydra/eval/lib/` |
| Java DSL term builders | `hydra-java/src/main/java/hydra/dsl/Terms.java` |
| Java Either utilities | `hydra-java/src/main/java/hydra/util/Either.java` |
| Java test runner | `hydra-java/src/test/java/hydra/TestSuiteRunner.java` |
| Java reducer | `hydra-java/src/gen-main/java/hydra/reduction/Reduction.java` |
| Haskell test definitions | `hydra-haskell/src/main/haskell/Hydra/Sources/Test/Lib/` |

## Quick orientation checklist

When starting a new session, consider:

1. Check `git status` and recent commits for context on what's being worked on.
2. If the task involves the kernel DSL, read the Hydra lexicon
   (`docs/hydra-lexicon.txt`) for the complete API surface.
3. If promoting code, read [promoting-code.md](docs/recipes/promoting-code.md).
4. If modifying kernel types/terms, read
   [extending-hydra-core.md](docs/recipes/extending-hydra-core.md).
5. If adding primitives, read [adding-primitives.md](docs/recipes/adding-primitives.md).
6. After any DSL changes: rebuild Haskell, then sync Java and Python.
