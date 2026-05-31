# Hydra-Scala

Hydra-Scala is a complete Scala 3 implementation of the [Hydra](https://github.com/CategoricalData/hydra)
kernel. It supports the full code generation pipeline and passes the bootstrapping test suite,
producing output identical to the Haskell, Java, Python, and Lisp hosts for all target languages.

## Features

- **150 generated modules** from the Hydra kernel, all compiling cleanly under Scala 3.3.7
- **Bootstrapping host**: loads Hydra modules from JSON and generates Haskell, Java, Python, and Scala
- **231 primitive functions** across 12 categories (chars, equality, eithers, lists, literals,
  logic, maps, math, maybes, pairs, sets, strings)
- **Lazy evaluation support** via Scala's by-name parameters for `ifElse`, `maybe`, `fromMaybe`, etc.
  (other target languages thunk lazy primitive arguments instead, driven by per-parameter `isLazy`
  metadata — see [Lazy evaluation and thunking](../../docs/recipes/new-implementation.md#lazy-evaluation-and-thunking))

## Build

Requires **Scala 3.3.7** (LTS) and **sbt 1.10.x**.

```bash
# From packages/hydra-scala (where build.sbt lives)
cd packages/hydra-scala
sbt compile

# Run the bootstrapping entry point (generates code for a target language from JSON)
sbt "runMain hydra.bootstrap --target java --json-dir ../../dist/json/hydra-kernel/src/main/json"

# Start a Scala 3 REPL
sbt console
```

## Code generation (from Haskell)

To regenerate the Scala code from the Haskell source of truth:

```bash
# From the repo root
bin/sync-scala.sh             # Narrow self-sync (--hosts scala --targets scala)
bin/sync-scala.sh --no-tests  # Skip target-language tests
```

`sync-scala.sh` only refreshes `dist/scala/hydra-scala/` plus the baseline
`dist/scala/{hydra-kernel,hydra-pg,hydra-rdf}/` trees. If you need to compile
the full sbt project (which references `dist/scala/hydra-{haskell,java,python,lisp}/`
as well), use a wider sync:

```bash
bin/sync.sh --hosts scala --targets all                     # full Scala refresh
bin/sync.sh --hosts haskell,scala --targets haskell,scala
bin/sync.sh --hosts all --targets all                       # full matrix
```

## Code organization

In 0.15, Hydra's Scala code is split across three locations
(see [Code organization wiki page](https://github.com/CategoricalData/hydra/wiki/Code-organization) for the full picture):

- **This package** (`packages/hydra-scala/`) — Scala coder DSL sources and the sbt build
  - `src/main/haskell/Hydra/Sources/Scala/` — Scala coder DSL sources (written in Haskell)
  - `build.sbt` — sbt build configuration (points at the head and the generated kernel)

- **Scala head** ([`heads/scala/src/main/scala/`](https://github.com/CategoricalData/hydra/tree/main/heads/scala/src/main/scala))
  — hand-written Scala runtime
  - `hydra/Bootstrap.scala` — bootstrapping entry point (JSON → code generation)
  - `hydra/Generation.scala` — I/O wrapper for code generation with JSON parser
  - `hydra/lib/` — hand-written primitive implementations
    (`Libraries.scala`, `chars.scala`, `eithers.scala`, `equality.scala`, `lists.scala`,
    `literals.scala`, `logic.scala`, `maps.scala`, `math.scala`, `maybes.scala`,
    `pairs.scala`, `sets.scala`, `strings.scala`)

- **Generated Scala kernel** ([`dist/scala/hydra-kernel/src/main/scala/`](https://github.com/CategoricalData/hydra/tree/main/dist/scala/hydra-kernel/src/main/scala))
  - `hydra/core.scala`, `hydra/graph.scala`, `hydra/packaging.scala`, ... — generated kernel modules

## Design notes

### Collections

Hydra-Java ships custom `ConsList` / `PersistentMap` / `PersistentSet`
helpers to match Haskell's `[a]` / `Data.Map` / `Data.Set` semantics — see
the [Collection classes](../hydra-java/README.md#collection-classes) section
in the hydra-java README and issue #359. Hydra-Scala does **not** need
equivalent helpers: Scala 3's `Predef.{Seq, Map, Set}` already resolve to
`scala.collection.immutable.{List, Map, Set}`, all persistent and
structurally shared:

- `x +: xs` (cons) is O(1).
- `m.updated(k, v)` and `m.removed(k)` are effectively O(1) (HAMT, branching
  factor 32 ≈ `log₃₂ n`).
- `s + x` and `s - x` are likewise effectively O(1) via HAMT.
- `m1 ++ m2` (union) is O(n + m).

`hydra.lib.{lists, maps, sets}` are thin wrappers over these immutable
collections — no custom collection classes needed. The `keys`, `elems`,
`toList` functions sort their output to match Haskell's `Data.Map` /
`Data.Set` ordered-iteration semantics, which is required for cross-host
test parity but is not on a hot path during inference. Adding sorted-by-key
custom maps would not help: it would either duplicate Scala's HAMT (no win)
or replace it with a red-black tree (slower in practice — `log₂` vs `log₃₂`).

If asked again whether Scala needs persistent-collection helpers like the
Java head's: no.

## Future enhancements

Recommendations from [#233](https://github.com/CategoricalData/hydra/issues/233)
that haven't been adopted yet. Scala wasn't in the original priority
scope of that issue (the focus was Haskell/Java/Python), but the
language has several mechanisms that would make Hydra Scala code
substantially more idiomatic. Recorded here so the design intent
survives any future re-evaluation. These are deliberate non-goals
today, not bugs.

#### String interpolators for inline types and terms

```scala
val myType = type"{ name: String, age: Int32, email: Optional String }"
val myTerm = term"let x = 5 in x + y"
```

Scala interpolators are simpler to implement than Haskell QuasiQuoters
(no Template Haskell equivalent needed) but raise the same parser-design
question. Useful for inline definitions in tests and demos.

#### Given/using for context-bearing DSL operations

Hydra's `Flow` and graph operations carry implicit context (the graph,
the namespace, the trace stack). Scala 3's `given`/`using` would let
these thread invisibly through DSL code, replacing explicit context
parameters:

```scala
def transformPerson(p: TTerm[Person])(using Graph, Namespace): TTerm[Person] = ...
```

#### Infix notation for binary DSL operations

Scala's infix calling convention naturally supports method-as-operator
forms:

```scala
val applied = function @@ argument
val piped = source |> transform
val annotated = term @: annotation
```

Most useful for function application, composition, and pipeline
operators. Aligns with the Haskell DSL's `@@`/`<.>` operators.

#### Macros for compile-time term construction

Scala 3 inline + quoted macros could generate term/type definitions
from declarative class definitions, similar to the Template Haskell
recommendation for the Haskell DSL.

## Contributing

Hydra is an open-source project and welcomes contributors. Resources:

- **[Creating a new Hydra implementation](https://github.com/CategoricalData/hydra/blob/main/docs/recipes/new-implementation.md)** —
  step-by-step guide
- **[LambdaGraph Discord](https://bit.ly/lg-discord)** — community discussion
- **[Main README](https://github.com/CategoricalData/hydra)** — project overview
