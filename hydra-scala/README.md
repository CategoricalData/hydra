# Hydra-Scala

Hydra-Scala is a complete Scala 3 implementation of the [Hydra](https://github.com/CategoricalData/hydra)
kernel. It supports the full code generation pipeline and passes the bootstrapping test suite,
producing output identical to the Haskell and Java hosts for all target languages.

## Features

- **145 generated modules** from the Hydra kernel, all compiling cleanly under Scala 3.3.7
- **Bootstrapping host**: loads Hydra modules from JSON and generates Haskell, Java, and Python
- **231 primitive functions** across 12 categories (chars, equality, eithers, lists, literals,
  logic, maps, math, maybes, pairs, sets, strings)
- **Lazy evaluation support** via Scala's by-name parameters for `ifElse`, `maybe`, `fromMaybe`, etc.

## Build

Requires **Scala 3.3.7** (LTS) and **sbt 1.10.x**.

```bash
# Compile all generated and hand-written code
sbt compile

# Run the bootstrapping entry point (generates code for a target language from JSON)
sbt "runMain hydra.bootstrap --target java --json-dir ../hydra-haskell/src/gen-main/json"

# Start a Scala 3 REPL
sbt console
```

## Code generation (from Haskell)

To regenerate the Scala code from the Haskell source of truth:

```bash
# From the hydra-ext directory
./bin/sync-scala.sh          # Full sync (build, generate, post-process, compile)
./bin/sync-scala.sh --quick  # Skip Scala compilation
```

Or include Scala in the full sync:

```bash
# From the repo root
./bin/sync-all.sh --targets hydra,java,python,scala
```

## Project structure

```
hydra-scala/
  src/
    main/scala/hydra/
      Bootstrap.scala        # Bootstrapping entry point (JSON → code generation)
      Generation.scala       # I/O wrapper for code generation with JSON parser
      lib/                   # Hand-written primitive implementations
        Libraries.scala      # Primitive registry (231 primitives)
        chars.scala          # Character primitives
        eithers.scala        # Either primitives (with lazy defaults)
        equality.scala       # Comparison and equality
        lists.scala          # List operations
        literals.scala       # Literal type conversions
        logic.scala          # Boolean logic (with lazy ifElse)
        maps.scala           # Map operations
        math.scala           # Arithmetic and math functions
        maybes.scala         # Option/Maybe primitives (with lazy defaults)
        pairs.scala          # Tuple operations
        sets.scala           # Set operations
        strings.scala        # String operations
    gen-main/scala/hydra/    # Generated kernel code (do not edit)
      core.scala             # Core types (Term, Type, Name, etc.)
      graph.scala            # Graph, Primitive, TermCoder
      module.scala           # Module, Namespace, Definition
      ...                    # ~145 modules total
  bin/
    break-long-lines.py      # Post-processor for generated code
  build.sbt                  # SBT build configuration
```

## Contributing

Hydra is an open-source project and welcomes contributors. Resources:

- **[Creating a new Hydra implementation](https://github.com/CategoricalData/hydra/blob/main/docs/recipes/new-implementation.md)** — step-by-step guide
- **[LambdaGraph Discord](https://bit.ly/lg-discord)** — community discussion
- **[Main README](https://github.com/CategoricalData/hydra)** — project overview
