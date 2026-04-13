# Hydra-Haskell

Hydra is a functional programming language based on the [LambdaGraph](https://bit.ly/lg-kgc2024) data model,
exploring an isomorphism between typed lambda calculus and labeled hypergraphs.
See the main Hydra [README](https://github.com/CategoricalData/hydra) for project overview and use cases.

This package contains the **Haskell coder DSL sources**: the type and term modules
that describe how to translate Hydra modules into Haskell source code.
The runnable Haskell head (primitives, DSL runtime, generation drivers, exec binaries,
test runners) lives under [`heads/haskell/`](https://github.com/CategoricalData/hydra/tree/main/heads/haskell).
Releases are available [on Hackage](https://hackage.haskell.org/package/hydra).

## Code organization

Hydra's 0.15 layout separates hand-written code (`packages/` and `heads/`) from generated
code (`dist/`). See the
[Code organization wiki page](https://github.com/CategoricalData/hydra/wiki/Code-organization)
for the full picture.

- **This package** (`packages/hydra-haskell/src/main/haskell/`) — the Haskell coder DSL sources
  - `Hydra/Sources/Haskell/` — Haskell `Syntax`, `Language`, `Coder`, `Serde`, `Operators`, `Testing`, `Utils`, `Environment` modules
  - `Hydra/Sources/All.hs` — aggregates kernel + Haskell + JSON modules for the sync pipeline

- **Kernel DSL sources** ([`packages/hydra-kernel/src/main/haskell/`](https://github.com/CategoricalData/hydra/tree/main/packages/hydra-kernel/src/main/haskell))
  - `Hydra/Sources/Kernel/Types/` — core types (Core, Graph, Packaging, Coders, ...)
  - `Hydra/Sources/Kernel/Terms/` — term-level logic (Inference, Checking, Reduction, ...)
  - `Hydra/Sources/Libraries.hs` — primitive function registration

- **Haskell head** ([`heads/haskell/src/main/haskell/Hydra/`](https://github.com/CategoricalData/hydra/tree/main/heads/haskell/src/main/haskell/Hydra))
  - `Dsl/` — DSL syntax definitions (hand-written)
  - `Lib/` — native primitive implementations
  - `Generation.hs` / `ExtGeneration.hs` / `Haskell/Generation.hs` — code-generation drivers
  - `Kernel.hs`, `Minimal.hs`, `Settings.hs`, `Tools/` — runtime helpers

- **Generated Haskell kernel** ([`dist/haskell/hydra-kernel/src/main/haskell/`](https://github.com/CategoricalData/hydra/tree/main/dist/haskell/hydra-kernel/src/main/haskell))
  - `Hydra/Core.hs`, `Hydra/Graph.hs`, etc. — generated from the kernel DSL sources

- **Generated ext-coder output** ([`dist/haskell/hydra-ext/src/main/haskell/`](https://github.com/CategoricalData/hydra/tree/main/dist/haskell/hydra-ext/src/main/haskell))
  - `Hydra/Java/Coder.hs`, `Hydra/Python/Coder.hs`, ... — generated from each language's DSL sources

The Haskell head is the **bootstrapping implementation**: the DSL sources in `packages/`
are compiled through it to generate code for Java, Python, Scala, Lisp, and other languages.

## Documentation

For comprehensive documentation about Hydra's architecture, type system,
and implementation details, see:

- **[Concepts](https://github.com/CategoricalData/hydra/wiki/Concepts)** —
  Core concepts: Type, Term, Graph, primitives, coders
- **[Implementation](https://github.com/CategoricalData/hydra/blob/main/docs/implementation.md)** —
  Detailed guide covering type modules, DSLs, primitives, coders, and the bootstrap process
- **[DSL Guide](https://github.com/CategoricalData/hydra/blob/main/docs/dsl-guide.md)** —
  Comprehensive guide to Hydra's domain-specific languages
- **[Code Organization](https://github.com/CategoricalData/hydra/wiki/Code-organization)** —
  The `packages/`, `heads/`, `dist/` layout
- **[Testing](https://github.com/CategoricalData/hydra/wiki/Testing)** —
  Common test suite and language-specific testing
- **[Developer Recipes](https://github.com/CategoricalData/hydra/blob/main/docs/recipes/index.md)** —
  Step-by-step guides for extending Hydra

This README focuses on practical instructions for building, testing, and generating code
with the Haskell head.

## Build

Haskell is Hydra's **bootstrapping language**, and the runnable head lives in
`heads/haskell/`. The DSL sources that describe each coder live in their respective
`packages/hydra-<lang>/` packages. Together they produce:

- **DSL syntax and runtime**:
  [`heads/haskell/src/main/haskell/Hydra/Dsl`](https://github.com/CategoricalData/hydra/tree/main/heads/haskell/src/main/haskell/Hydra/Dsl)
  and [`Hydra/Lib`](https://github.com/CategoricalData/hydra/tree/main/heads/haskell/src/main/haskell/Hydra/Lib)
- **Kernel DSL sources**:
  [`packages/hydra-kernel/src/main/haskell/Hydra/Sources`](https://github.com/CategoricalData/hydra/tree/main/packages/hydra-kernel/src/main/haskell/Hydra/Sources)
- **Haskell coder DSL sources** (this package):
  [`packages/hydra-haskell/src/main/haskell/Hydra/Sources/Haskell`](https://github.com/CategoricalData/hydra/tree/main/packages/hydra-haskell/src/main/haskell/Hydra/Sources/Haskell)
- **Primitive registration**:
  [`Libraries.hs`](https://github.com/CategoricalData/hydra/blob/main/packages/hydra-kernel/src/main/haskell/Hydra/Sources/Libraries.hs)
- **Generated Haskell kernel**:
  [`dist/haskell/hydra-kernel/src/main/haskell`](https://github.com/CategoricalData/hydra/tree/main/dist/haskell/hydra-kernel/src/main/haskell)

The DSL sources are also used to generate Java, Python, Scala, Lisp, and other
implementations, ensuring parity across each Hydra language variant.

### Build and REPL

First, install the [Haskell Tool Stack](https://docs.haskellstack.org/en/stable):

```bash
# On macOS
brew install haskell-stack

# Other platforms: see https://docs.haskellstack.org/en/stable/install_and_upgrade/
```

Then build and enter the GHCi REPL from the Haskell head directory:

```bash
cd heads/haskell
stack ghci hydra:lib
```

Note: The `hydra:lib` target is required to avoid loading test modules.
Running `stack ghci` without a target will load all components including test dependencies,
which you may not need.

## Test

Run all tests:

```bash
cd heads/haskell && stack test
```

For interactive testing with access to test utilities:

```bash
cd heads/haskell && stack ghci hydra:lib hydra:hydra-test
```

Then in the REPL, you can run individual tests:

```haskell
Test.Hspec.hspec Hydra.TestSuiteSpec.spec
```

See the [Testing wiki page](https://github.com/CategoricalData/hydra/wiki/Testing)
for details on Hydra's common test suite, which ensures parity across all Hydra language variants.

## Code generation

Hydra is **self-hosting**: it can generate its own source code from DSL definitions.
All generation is driven from the Haskell head in [`heads/haskell/`](https://github.com/CategoricalData/hydra/tree/main/heads/haskell).

### Regenerate everything

The simplest entry point is the top-level sync driver, which regenerates Haskell, Java,
Python, Scala, Lisp, and all ext coders in order:

```bash
./bin/sync-all.sh
```

To regenerate just one target, invoke the corresponding per-language script from
`heads/haskell/bin/`:

```bash
heads/haskell/bin/sync-haskell.sh
heads/haskell/bin/sync-java.sh
heads/haskell/bin/sync-python.sh
heads/haskell/bin/sync-scala.sh
heads/haskell/bin/sync-lisp.sh
heads/haskell/bin/sync-ext.sh
```

### Run the generation driver interactively

For one-off generation or exploration, enter the GHCi REPL from `heads/haskell/`:

```bash
cd heads/haskell
stack ghci hydra:lib
```

Then:

```haskell
import Hydra.Generation
import Hydra.Sources.All

-- Regenerate the Haskell kernel into dist/
writeHaskell "../../dist/haskell/hydra-kernel/src/main/haskell" mainModules mainModules

-- Regenerate the Python kernel
writePython  "../../dist/python/hydra-kernel/src/main/python"   mainModules mainModules

-- Regenerate the Java kernel
writeJava    "../../dist/java/hydra-kernel/src/main/java"       mainModules mainModules
```

For extension coders (Avro, Protobuf, GraphQL, ...), import `Hydra.ExtGeneration`
instead and use the same `write*` functions with the ext-specific module lists.
See [`packages/hydra-ext/README.md`](https://github.com/CategoricalData/hydra/blob/main/packages/hydra-ext/README.md)
for the full list of supported targets.

## Working with Hydra

### Core types

Some of the fundamental types in Hydra are:

- **`Type`** - Represents the structure of data (literals, records, unions, functions, etc.)
- **`Term`** - Represents data or computation (instances of types)
- **`TermDefinition`** / **`TypeDefinition`** — A named term or type definition
  (name + term/type scheme + optional description). Both are wrapped in `Definition`.
- **`Graph`** — A lexical environment binding names to terms, type schemes, primitives,
  and class constraints.
- **`Module`** — A namespace containing definitions with dependencies on other modules.

These are defined in
[Hydra/Sources/Kernel/Types](https://github.com/CategoricalData/hydra/tree/main/packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Types)
and code-generated into
[`Hydra.Core`](https://github.com/CategoricalData/hydra/blob/main/dist/haskell/hydra-kernel/src/main/haskell/Hydra/Core.hs),
[`Hydra.Graph`](https://github.com/CategoricalData/hydra/blob/main/dist/haskell/hydra-kernel/src/main/haskell/Hydra/Graph.hs), and
[`Hydra.Packaging`](https://github.com/CategoricalData/hydra/blob/main/dist/haskell/hydra-kernel/src/main/haskell/Hydra/Packaging.hs).

See [Concepts](https://github.com/CategoricalData/hydra/wiki/Concepts) for detailed explanations.

### Error handling and Context

Hydra uses `Either Error a` for computations that can fail (where `Error` is the
structured error type from `Hydra.Errors`). A `Context` value carrying trace messages
and metadata is threaded explicitly alongside the `Graph`.

```haskell
-- A function that may fail
myComputation :: Context -> Graph -> Either Error String
myComputation cx g = Right "result"

-- Execute it
case myComputation emptyContext myGraph of
  Right result -> putStrLn result
  Left err     -> print err
```

### Coders and adapters

**Coders** are bidirectional transformations:

```haskell
data Coder v1 v2 = Coder {
  coderEncode :: Context -> v1 -> Either Error v2,
  coderDecode :: Context -> v2 -> Either Error v1
}
```

**Adapters** also transform types, enabling schema evolution and language mapping.

See the
[Implementation wiki](https://github.com/CategoricalData/hydra/blob/main/docs/implementation.md#cross-language-compilation-coders)
for details.

### DSLs

Hydra provides multiple domain-specific languages for constructing types and terms:

**Untyped DSLs**
([Hydra/Dsl/Types.hs](https://github.com/CategoricalData/hydra/blob/main/heads/haskell/src/main/haskell/Hydra/Dsl/Types.hs),
[Hydra/Dsl/Terms.hs](https://github.com/CategoricalData/hydra/blob/main/heads/haskell/src/main/haskell/Hydra/Dsl/Terms.hs)):
```haskell
import qualified Hydra.Dsl.Types as Types
import qualified Hydra.Dsl.Terms as Terms

personType = Types.record [
  "name" >: string,
  "age" >: int32]

alice = Terms.record [
  "name" >: Terms.string "Alice",
  "age" >: Terms.int32 30]
```

**Phantom-typed DSLs**
([Hydra/Dsl/Meta/Phantoms.hs](https://github.com/CategoricalData/hydra/blob/main/heads/haskell/src/main/haskell/Hydra/Dsl/Meta/Phantoms.hs)) -
Compile-time type safety:
```haskell
import Hydra.Dsl.Meta.Phantoms

safeFn :: TTerm (Int -> String)
safeFn = lambda "x" (Strings.toUpper (var "x"))  -- Type-checked at compile time
```

**Library DSLs**
([Hydra/Dsl/Meta/Lib](https://github.com/CategoricalData/hydra/tree/main/heads/haskell/src/main/haskell/Hydra/Dsl/Meta/Lib)) -
Wrappers for primitive functions:
```haskell
import Hydra.Dsl.Meta.Lib.Lists as Lists
import Hydra.Dsl.Meta.Lib.Strings as Strings

example = Lists.map (Strings.toUpper) (list ["hello", "world"])
```

See the [DSL system section](https://github.com/CategoricalData/hydra/blob/main/docs/implementation.md#dsl-system)
in the Implementation wiki for comprehensive coverage.

### JSON and YAML serialization

Hydra provides JSON and YAML coders in `Hydra.Json.*` (and a YAML model via
`Hydra.Yaml.Model`), with DSL helpers under `Hydra.Dsl.Json.Model`.
The [JSON kernel recipe](https://github.com/CategoricalData/hydra/blob/main/docs/recipes/json-kernel.md)
covers exporting and loading kernel modules via JSON; for general-purpose JSON
encoding and decoding, see the `Hydra.Json.Encode` and `Hydra.Json.Decode` generated
modules.

## Self-hosting demonstration

Hydra-Haskell is a [self-hosting compiler](https://en.wikipedia.org/wiki/Self-hosting_(compilers)) -
it can generate its own source code.

Complete self-hosting cycle (from the repo root):

```bash
# Regenerate the Haskell kernel, run kernel tests, then run the test suite.
heads/haskell/bin/sync-haskell.sh
```

Or step by step from `heads/haskell/`:

```bash
cd heads/haskell
stack ghci hydra:lib
import Hydra.Sources.All
import Hydra.Generation
writeHaskell "../../dist/haskell/hydra-kernel/src/main/haskell" mainModules mainModules
let allModules = mainModules ++ testModules
writeHaskell "../../dist/haskell/hydra-kernel/src/test/haskell" allModules baseTestModules
:q
stack test
```

The generated code includes:
- All core types (Type, Term, Graph, Module, etc.)
- Type inference and checking
- Term reduction and rewriting
- Coders and adapters
- Primitive functions (signatures only; implementations are in Hydra/Lib)

What remains hand-written:
- `Hydra.Lib` - Native primitive implementations
- `Hydra.Sources` - DSL-based specifications (input to code generation)
- `Hydra.Dsl` - DSL syntax
- `Hydra.Generation` - I/O and generation utilities
- Test runners

See the
[Bootstrap process](https://github.com/CategoricalData/hydra/blob/main/docs/implementation.md#the-bootstrap-process)
section for details on extending Hydra.
For example:
- [Adding new primitive functions](https://github.com/CategoricalData/hydra/blob/main/docs/recipes/adding-primitives.md)
- [Extending Hydra Core](https://github.com/CategoricalData/hydra/blob/main/docs/recipes/extending-hydra-core.md)

## Troubleshooting

### Stack version warnings

If you see warnings like:
```
Stack has not been tested with GHC versions above 9.0, and using 9.10.2, this may fail
Stack has not been tested with Cabal versions above 3.4, but version 3.12.1.0 was found, this may fail
```

**Solution:** Update Stack to the latest version:
```bash
stack upgrade
# or on macOS with Homebrew:
brew upgrade stack
```

**Explanation:** These are compatibility warnings that appear when using newer GHC/Cabal versions.
If your builds complete successfully, the warnings are harmless -
Stack works fine with newer versions even before official testing.
Upgrading Stack eliminates the warnings.
