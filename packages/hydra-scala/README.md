# Hydra-Scala

Hydra-Scala is a complete Scala 3 implementation of the [Hydra](https://github.com/CategoricalData/hydra)
kernel. It supports the full code generation pipeline and passes the bootstrapping test suite,
producing output identical to the Haskell, Java, Python, and Lisp hosts for all target languages.

## Features

- **~260 generated modules** from the Hydra kernel, all compiling cleanly under Scala 3.3.7
- **Bootstrapping host**: loads Hydra modules from JSON and generates Haskell, Java, Python, and Scala
- **The full `hydra.lib.*` primitive library** (see
  [docs/hydra-lexicon.txt](https://github.com/CategoricalData/hydra/blob/main/docs/hydra-lexicon.txt)
  for the current set of primitives and signatures)
- **Lazy evaluation support** via Scala's by-name parameters for `ifElse`, `cases`, `fromOptional`, etc.
  (other target languages thunk lazy primitive arguments instead, driven by per-parameter `isLazy`
  metadata — see [Lazy evaluation and thunking](https://github.com/CategoricalData/hydra/blob/main/docs/recipes/new-implementation.md#lazy-evaluation-and-thunking))

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

## Testing

```bash
# Hydra-kernel test suite against the assembled dist/scala distribution:
heads/scala/bin/test-distribution.sh

# Or, from packages/hydra-scala (tests resolve via build.sbt's unmanaged dirs):
sbt test

# Or the repo-level wrapper (pre-syncs unless --no-sync):
bin/test.sh scala
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
  - `src/main/scala/hydra/sources/scala/` — Scala coder DSL sources (written in native Scala; #509)
  - `build.sbt` — sbt build configuration (points at the head and the generated kernel)

- **Scala head** ([`heads/scala/src/main/scala/`](https://github.com/CategoricalData/hydra/tree/main/heads/scala/src/main/scala))
  — generation drivers only
  - `hydra/Bootstrap.scala` — bootstrapping entry point (JSON → code generation)
  - `hydra/Generation.scala` — I/O wrapper for code generation with JSON parser
  - `hydra/UpdateScalaJson.scala` — native Scala DSL → JSON driver
  - The hand-written primitive implementations and DSL runtime live in the overlay
    ([`overlay/scala/hydra-kernel/`](https://github.com/CategoricalData/hydra/tree/main/overlay/scala/hydra-kernel),
    namespace `hydra.overlay.scala.*`, #434/#501) and are copied into `dist/scala/` at assemble time

- **Generated Scala kernel** ([`dist/scala/hydra-kernel/src/main/scala/`](https://github.com/CategoricalData/hydra/tree/main/dist/scala/hydra-kernel/src/main/scala))
  - `hydra/core.scala`, `hydra/graph.scala`, `hydra/packaging.scala`, ... — generated kernel modules

## Design notes

### Collections

Hydra-Java ships custom `ConsList` / `PersistentMap` / `PersistentSet`
helpers to match Haskell's `[a]` / `Data.Map` / `Data.Set` semantics — see
the [Collection classes](https://github.com/CategoricalData/hydra/blob/main/packages/hydra-java/README.md#collection-classes) section
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

## Maven Central publishing

Per-package Scala artifacts publish to Maven Central under group `net.fortytwo.hydra.scala`
(#519; the Java artifacts use `net.fortytwo.hydra.java`), with sbt's `_3` cross-version
suffix on artifact ids, using sbt-sonatype + sbt-pgp. The publish set is 9 packages:
hydra-kernel, hydra-jvm, hydra-haskell, hydra-java, hydra-python, hydra-scala, hydra-lisp,
hydra-typescript, hydra-rdf. First published at 0.17.0.
(`hydra-pg` is excluded pending a fix for a type-variable threading bug in the generated Scala coder — see [#491](https://github.com/CategoricalData/hydra/issues/491).)

Each `dist/scala/<pkg>/` is a standalone sbt project with its own `build.sbt`, `project/plugins.sbt`,
and `project/build.properties`, generated by `bin/lib/generate-scala-package-build.py`.

To do a local dry-run (compiles and publishes to `~/.ivy2/local`):

```bash
heads/scala/bin/publish-sbt.sh       # publishLocal all 9 packages in dependency order
```

For the full publish procedure (signing + upload to Maven Central), see
[docs/release-workflow.md](https://github.com/CategoricalData/hydra/blob/main/docs/release-workflow.md).

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
