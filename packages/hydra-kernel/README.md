# Hydra-Kernel

The **Hydra kernel** — the code that fundamentally defines what Hydra is and how
it operates. Hydra's core data types (`Type`, `Term`, `Graph`, `Module`, etc.) and
its fundamental programming logic (inference, checking, reduction, rewriting,
validation, the coder framework, the primitive standard library) live here.
The kernel uses the Hydra language to define the Hydra language itself.

Every other Hydra package is peripheral, adding capabilities like Haskell or Java
support, RDF or property graph functionality, and so on. Every package depends on
`hydra-kernel`; every host language imports a generated form of it.

Hydra is a functional programming language based on the
[LambdaGraph](https://bit.ly/lg-kgc2024) data model, exploring an isomorphism
between typed lambda calculus and labeled hypergraphs. See the main Hydra
[README](https://github.com/CategoricalData/hydra) for project overview and use cases.

## What lives here

`packages/hydra-kernel/` is the **DSL source of truth** for the kernel. There is no
generated output in this package — only the hand-written DSL modules that describe
what the kernel *is*. Generated kernels for each host language live in `dist/<lang>/hydra-kernel/`.

### Type modules

[`src/main/haskell/Hydra/Sources/Kernel/Types/`](https://github.com/CategoricalData/hydra/tree/main/packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Types)
defines the core data structures:

- **`Core.hs`** — the central grammar: `Term`, `Type`, `Literal`, `Function`,
  `Elimination`, plus annotations and type schemes. The most important file in the
  kernel.
- **`Graph.hs`** — `Graph`, `Element`, `Primitive`, `TypedTerm` — the runtime
  containers for terms and types.
- **`Packaging.hs`** — `Module`, `ModuleName`, `Namespace`, module dependencies.
- **`Coders.hs`** — the bidirectional `Coder` framework that every per-language
  coder builds on, plus `Adapter` (which transforms both type and value).
- **`Errors.hs`** — the unified error taxonomy used across inference, validation,
  and code generation.
- **`Ast.hs`** — variant tags used by validators and serializers.
- **`Mantle.hs`** — type-system internals (`TypeScheme`, kind, class constraints).
- **Subdirectories** (`Error/`, `Json/`, `Show/`) — submodule groupings for
  error variants, JSON encoding, and pretty-printing.

### Term modules

[`src/main/haskell/Hydra/Sources/Kernel/Terms/`](https://github.com/CategoricalData/hydra/tree/main/packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Terms)
defines the algorithms:

- **`Inference.hs`** — Algorithm W (Hindley–Milner) with class constraints, type
  schemes, and Hydra-specific extensions for nominal terms.
- **`Checking.hs`** — type checking; runs after inference and validates results.
- **`Reduction.hs`** — beta/eta reduction, including tail-call detection used by
  the Python and Java coders (see [TCO implementation](https://github.com/CategoricalData/hydra/blob/main/docs/tco-implementation.md)).
- **`Rewriting.hs`** — free-variable analysis, substitution, term traversal.
- **`Analysis.hs`** — `isSelfTailRecursive` and related shared analyses.
- **`Validate/`** — module-level validation rules (`hydra.validate.*`).
- **`Sorting.hs`**, **`Topology.hs`** — Tarjan SCC over module/term dependency
  graphs.
- **`Dsls.hs`**, **`Generation.hs`** — the meta-level pieces that drive sync
  pipelines.
- **`Lexical.hs`**, **`Names.hs`**, **`Scoping.hs`**, **`Variables.hs`** —
  binder hygiene, name generation, scope handling.

### Library modules

[`src/main/haskell/Hydra/Sources/Kernel/Lib/`](https://github.com/CategoricalData/hydra/tree/main/packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Lib)
defines the **standard library** of primitive functions, organized by data type
(`Chars`, `Equality`, `Flows`, `Lists`, `Literals`, `Logic`, `Maps`, `Math`,
`Pairs`, `Sets`, `Strings`, ...). Roughly 180 primitives total. See
[`docs/hydra-lexicon.txt`](https://github.com/CategoricalData/hydra/blob/main/docs/hydra-lexicon.txt)
for the full signature list.

## Code organization

The kernel package contains DSL sources only. Generated kernels and runtime helpers
live in adjacent directories:

| Where | What |
|-------|------|
| `packages/hydra-kernel/src/main/haskell/Hydra/Sources/` | Kernel DSL sources (this package) |
| `heads/haskell/src/main/haskell/Hydra/Dsl/`             | Hand-written Haskell DSL helpers used to write the sources |
| `heads/haskell/src/main/haskell/Hydra/Lib/`             | Hand-written Haskell primitive implementations |
| `dist/haskell/hydra-kernel/`                            | Generated Haskell kernel (`Hydra/Core.hs`, `Hydra/Graph.hs`, etc.) |
| `dist/java/hydra-kernel/`                               | Generated Java kernel (`hydra.core.*`, `hydra.graph.*`, etc.) |
| `dist/python/hydra-kernel/`                             | Generated Python kernel |
| `dist/scala/hydra-kernel/`                              | Generated Scala kernel |
| `dist/{clojure,common-lisp,emacs-lisp,scheme}/hydra-kernel/` | Generated Lisp-dialect kernels |
| `dist/typescript/hydra-kernel/`                         | Generated TypeScript kernel |
| `dist/json/hydra-kernel/`                               | Generated JSON kernel — language-agnostic interchange |

For the broader `packages/heads/dist` pattern, see the
[Code organization wiki page](https://github.com/CategoricalData/hydra/wiki/Code-organization).

## How the kernel is regenerated

The kernel is generated from these DSL sources via `bin/sync.sh`. After editing a
file under `Hydra/Sources/Kernel/`, run:

```bash
bin/sync-haskell.sh        # regenerate Haskell kernel, run stack test
bin/sync.sh                # regenerate every target language's kernel and tests
```

See [Generating code with Hydra](https://github.com/CategoricalData/hydra/blob/main/docs/recipes/code-generation.md)
for the full pipeline and [The Hydra build system](https://github.com/CategoricalData/hydra/blob/main/docs/build-system.md)
for the cache model.

## Extending the kernel

Three of the most common kernel-modification tasks have dedicated recipes:

- **[Adding new type and term constructors](https://github.com/CategoricalData/hydra/blob/main/docs/recipes/extending-hydra-core.md)**
  — solving the bootstrap problem when you extend `Core.Term` or `Core.Type`.
- **[Adding new primitive functions](https://github.com/CategoricalData/hydra/blob/main/docs/recipes/adding-primitives.md)**
  — adding to `hydra.lib.*` and registering across all hosts.
- **[Refactoring the Hydra kernel](https://github.com/CategoricalData/hydra/blob/main/docs/recipes/refactoring.md)**
  — creating, renaming, moving, or deleting kernel elements or modules, including
  namespace refactoring.

For the DSL idioms used to write these sources, see the
[Haskell DSL guide](https://github.com/CategoricalData/hydra/blob/main/docs/dsl-guide.md).

## See also

- **[Concepts](https://github.com/CategoricalData/hydra/wiki/Concepts)** — the
  LambdaGraph data model and type system.
- **[Implementation overview](https://github.com/CategoricalData/hydra/blob/main/docs/implementation.md)**
  — architecture, type modules, the coder framework, primitives.
- **[`hydra-haskell` README](https://github.com/CategoricalData/hydra/blob/main/packages/hydra-haskell/README.md)**
  — the bootstrapping Haskell head that consumes this package.
- **[Hydra lexicon](https://github.com/CategoricalData/hydra/blob/main/docs/hydra-lexicon.txt)**
  — kernel types + primitive signatures in one file.
