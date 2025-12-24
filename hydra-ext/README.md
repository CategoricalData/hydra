# Hydra-Ext

This subproject contains domain-specific extensions to Hydra, currently
including the coders and models listed below.
Most of these artifacts are written in "raw" Haskell, rather than in the Hydra
DSL.
There are also a number of demos and tools also written in Haskell, as well as
a few artifacts in Java and Python.

JavaDocs for Hydra-Ext can be found [here](https://categoricaldata.github.io/hydra/hydra-ext/javadoc),
and releases can be found on Maven Central [here](https://central.sonatype.com/artifact/net.fortytwo.hydra/hydra-ext).

## Code organization

Hydra-Ext uses the **src/main vs src/gen-main** separation pattern (see [Code organization wiki page](https://github.com/CategoricalData/hydra/wiki/Code-organization) for details).

- **`src/main/haskell/`** - Hand-written source code
  - `Hydra/Ext/Staging/` - Language coders (Java, Python, C++, Scala, etc.)
  - `Hydra/Ext/Sources/` - Domain models and language syntax definitions
  - `Hydra/Ext/Demos/` - Demonstration applications
  - `Hydra/Ext/Tools/` - Analysis and transformation utilities

- **`src/gen-main/haskell/`** - Generated Haskell APIs
  - Generated from domain models using `writeHaskell`
  - Provides typed Haskell interfaces to external formats

- **`src/gen-main/java/`** (not checked in) - Generated Java APIs
  - Can be generated using `writeJava "src/gen-main/java" hydraExtModules`

Note: Only generated Haskell is checked into version control for space reasons. Java can be generated on demand.

## Coders

Hydra-Ext provides coders for generating code in various target languages and formats. These coders vary in their capabilities and maturity levels.

### Coder maturity tiers

#### Full implementation (types + terms)

These coders generate complete, runnable code including type definitions and term-level implementations (functions, values, pattern matching):

| Coder | Status | Notes |
|-------|--------|-------|
| **Python** | Production-ready | Most complete implementation; generates full Python modules |
| **Java** | Production-ready | Types complete; term generation functional but Serde incomplete |

#### Type generation only

These coders generate type/schema definitions but not term-level code. They are suitable for defining data structures that will be populated by other means:

| Coder | Status | Notes |
|-------|--------|-------|
| **C++** | Stable | Generates header files with classes and enums |
| **GraphQL** | Stable | Generates GraphQL schema definitions |
| **JSON Schema** | Stable | Generates JSON Schema (2020-12 spec) |
| **Protobuf** | Stable | Proto3 format; no polymorphism support |
| **PDL (Pegasus)** | Stable | No polymorphism or cyclic types |
| **Scala** | Beta | Has known bugs; generates type aliases |

#### Specialized/data-oriented

These coders serve specific purposes beyond general-purpose code generation:

| Coder | Purpose | Notes |
|-------|---------|-------|
| **Avro** | Schema + data adapter | Bidirectional Avro ↔ Hydra conversion |
| **Property Graph** | Graph data mapping | Maps Hydra terms to PG vertices/edges |
| **SHACL** | RDF shape generation | Generates SHACL constraints from types |
| **GraphSON** | Graph serialization | TinkerPop graph format |
| **Graphviz** | Visualization | DOT format for graph visualization |
| **RDF** | Linked data | RDF triple serialization |

### Choosing a coder

- **For complete code generation**: Use Python or Java coders
- **For API definitions/schemas**: Use GraphQL, Protobuf, or JSON Schema
- **For data interchange**: Use Avro or JSON (via hydra-haskell)
- **For graph databases**: Use Property Graph or GraphSON
- **For semantic web**: Use RDF or SHACL

Code generation is similar to Haskell generation (see the [Hydra-Haskell README](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/README.md)).

### Java

[Hydra-Java](https://github.com/CategoricalData/hydra/tree/main/hydra-java) is a complete Hydra implementation.
You can update the Java image of the Hydra kernel with:

```haskell
-- Universe provides all modules for dependency resolution
-- modulesToGenerate specifies which modules to actually generate
writeJava "../hydra-java/src/gen-main/java" kernelModules kernelModules
```

For Java tests, use:

```haskell
let allModules = mainModules ++ testModules
writeJava "../hydra-java/src/gen-test/java" allModules baseTestModules
```

### Python

[Hydra-Python](https://github.com/CategoricalData/hydra/tree/main/hydra-python), like Hydra-Java and Hydra-Haskell, is a complete Hydra implementation.
You can update the Python image of the Hydra kernel with:

```haskell
-- Universe provides all modules for dependency resolution
-- modulesToGenerate specifies which modules to actually generate
writePython "../hydra-python/src/main/python" kernelModules kernelModules
```

For Python tests, use:

```haskell
let allModules = mainModules ++ testModules
writePython "../hydra-python/src/gen-test/python" allModules baseTestModules
```

Note: Python generation currently requires extra memory when generating the entire kernel (see [Issue #209](https://github.com/CategoricalData/hydra/issues/209)).
Instead of `stack ghci`, you can enter the REPL with `stack ghci --ghci-options='+RTS -K256M -A32M -RTS'`.

### C++

The C++ coder operates on schemas only, generating header files (`.h`) with class and enum definitions.
The coder does not support polymorphic models at this time; type parameters are replaced with their bounds or `std::string`.

```haskell
import qualified Hydra.Sources.Kernel.Types.Core as Core

-- Universe and modules to generate can be the same for simple cases
writeCpp "/tmp/cpp" [Core.module_] [Core.module_]
```

**Type mappings:**

| Hydra type | C++ representation |
|------------|-------------------|
| Record | Class with public fields and constructor |
| Union (unit variants only) | Enum class |
| Union (with values) | Base class with variant subclasses and visitor pattern |
| Either | `std::variant<L, R>` |
| Pair | `std::pair<A, B>` |
| List | `std::vector<T>` |
| Map | `std::map<K, V>` |
| Set | `std::set<T>` |
| Maybe | `std::optional<T>` |
| Literal (String) | `std::string` |
| Literal (Int32) | `int` |
| Literal (Int64) | `int64_t` |
| Literal (Float32) | `float` |
| Literal (Float64) | `double` |
| Literal (Boolean) | `bool` |
| Unit | `std::tuple<>` |
| Wrap | Class with `value` field |

Note: Union types with values use the visitor pattern for type-safe case analysis. Each variant becomes a subclass of the union's base class.

### Protobuf

Protobuf (Proto3) does not support polymorphic models, so type parameters are replaced with `string`. The generated `.proto` files follow the [Protobuf Style Guide](https://protobuf.dev/programming-guides/style).

```haskell
import qualified Hydra.Sources.Kernel.Types.Core as Core

-- Universe and modules to generate can be the same for simple cases
writeProtobuf "/tmp/protobuf" [Core.module_] [Core.module_]
```

**Type mappings:**

| Hydra type | Protobuf representation |
|------------|-------------------------|
| Record | Message with fields |
| Union (unit variants only) | Enum type |
| Union (with values) | Message with `oneof value` |
| List | `repeated` field |
| Maybe (scalar) | Wrapper type (e.g., `google.protobuf.StringValue`) |
| Wrap | Unwrapped to inner type |
| Literal (String, Int32, Int64, etc.) | Scalar types |
| Map | `map<K, V>` field |
| Set | `repeated` field (same as List) |
| Either | Helper message with `left`/`right` fields |
| Pair | Helper message with `first`/`second` fields |
| Unit | `google.protobuf.Empty` |

Note: Structural types like `Either<A, B>` and `Pair<A, B>` generate helper message types (e.g., `Either_Term_Term`, `Pair_Term_Term`) since Protobuf doesn't support nested `oneof` blocks.

### JSON Schema

The JSON Schema coder generates [JSON Schema](https://json-schema.org/) files (`.json`) following the 2020-12 specification.
This coder operates at the type level only, and can be used in connection with Hydra's built-in JSON coder at the term level.

```haskell
import qualified Hydra.Sources.Kernel.Types.Core as Core

-- Universe and modules to generate can be the same for simple cases
writeJsonSchema "/tmp/jsonschema" [Core.module_] [Core.module_]
```

**Type mappings:**

| Hydra type | JSON Schema representation |
|------------|---------------------------|
| Record | Object with properties and required fields |
| Union (unit variants only) | String enum |
| Union (with values) | Object with oneOf for variants |
| List | Array with items schema |
| Set | Array with items schema |
| Map | Object with additionalProperties schema |
| Maybe | Nullable type (adds `null` to type array) |
| Wrap | Unwrapped to inner type |
| Either | oneOf with `left`/`right` object variants |
| Pair | Object with `first`/`second` required fields |
| Literal (String) | `"type": "string"` |
| Literal (Boolean) | `"type": "boolean"` |
| Literal (Integer) | `"type": "integer"` |
| Literal (Float) | `"type": "number"` |

Note: Type references use JSON Schema `$ref` to `#/$defs/TypeName`.

### GraphQL

```haskell
import qualified Hydra.Sources.Kernel.Types.Core as Core

-- Universe and modules to generate can be the same for simple cases
writeGraphql "/tmp/graphql" [Core.module_] [Core.module_]
```

Because GraphQL does not support imports, the GraphQL coder will gather all of the dependencies of a given module together and map them to a single `.graphql` file.

**Type mappings:**

| Hydra type | GraphQL representation |
|------------|------------------------|
| Record | Object type with fields |
| Union (unit variants only) | Enum type |
| List | List type `[T!]!` |
| Maybe | Nullable type (no `!`) |
| Wrap | Object type with `value` field |
| Literal (String, Int, Float, Boolean) | Scalar types |
| Map | List of values (keys discarded) |
| Set | List type |
| Either | Object with optional `left`/`right` fields |
| Pair | Object with `first`/`second` fields |

Note: GraphQL doesn't natively support maps, sets, or sum types with values. These are approximated using the mappings above.

### PDL (Pegasus)

LinkedIn's PDL ([Pegasus Data Language](https://linkedin.github.io/rest.li/pdl_schema)) is supported in hydra-ext for historical reasons.
It is a fairly limited data language, supporting neither polymorphism nor cyclic type dependencies.
For example, modules like `hydra.core` cannot be expressed in PDL, because the `Term` and `Type` types reference each other.
The PDL coder generates PDL schema files (`.pdl`). 

```haskell
import qualified Hydra.Sources.Kernel.Types.Accessors as Accessors

-- Universe and modules to generate can be the same for simple cases
writePdl "/tmp/pdl" [Accessors.module_] [Accessors.module_]
```

**Type mappings:**

| Hydra type | PDL representation |
|------------|-------------------|
| Record | Record schema |
| Union (unit variants only) | Enum schema |
| Union (with values) | Union schema with aliased members |
| Either | Union with `left`/`right` aliased members |
| Pair | Record with `first`/`second` fields |
| List | Array schema |
| Set | Array schema |
| Map | Map schema (string keys assumed) |
| Maybe | Optional field |
| Wrap | Typeref to inner type |
| Literal (String) | `string` primitive |
| Literal (Int32) | `int` primitive |
| Literal (Int64) | `long` primitive |
| Literal (Float32) | `float` primitive |
| Literal (Float64) | `double` primitive |
| Literal (Boolean) | `boolean` primitive |
| Literal (Binary) | `bytes` primitive |

### Scala

The Scala coder generates Scala source files (`.scala`). Note that this coder has known bugs and may not work correctly for all types.

```haskell
import qualified Hydra.Sources.Kernel.Types.Accessors as Accessors

-- Universe and modules to generate can be the same for simple cases
writeScala "/tmp/scala" [Accessors.module_] [Accessors.module_]
```

**Type mappings:**

| Hydra type | Scala representation |
|------------|---------------------|
| Record | Type alias (case classes not yet supported) |
| Union | Type alias (sealed traits not yet supported) |
| List | `Seq[T]` |
| Set | `Set[T]` |
| Map | `Map[K, V]` |
| Maybe | `Option[T]` |
| Either | `Either[L, R]` |
| Pair | `Tuple2[A, B]` |
| Wrap | Type alias |
| Function | `A => B` |
| Literal (String) | `String` |
| Literal (Boolean) | `Boolean` |
| Literal (Int32) | `Int` |
| Literal (Int64) | `Long` |
| Literal (Float32) | `Float` |
| Literal (Float64) | `Double` |

Note: The Scala coder currently generates type aliases rather than full case class/sealed trait hierarchies. Term definitions are encoded as `val` or `def` declarations.

## Models

### Language syntax models

Hydra-Ext includes syntax models for various programming languages and data formats. These can be used to generate or parse code in the target language:

* [Avro Schema](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Sources/Avro/Schema.hs)
* [C++](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Sources/Cpp/Syntax.hs)
* [C#](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Sources/Csharp/Syntax.hs)
* [Cypher (OpenCypher)](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Sources/Cypher/OpenCypher.hs)
* [GraphQL](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Sources/Graphql/Syntax.hs)
* [Java](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Sources/Java/Syntax.hs)
* [JSON Schema](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Sources/Json/Schema.hs)
* [OWL](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Sources/Owl/Syntax.hs)
* [Parquet](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Sources/Parquet/Format.hs)
* [Pegasus (PDL)](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Sources/Pegasus/Pdl.hs)
* [Protobuf (Proto3)](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Sources/Protobuf/Proto3.hs)
* [Python](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Sources/Python/Syntax.hs)
* [RDF](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Sources/Rdf/Syntax.hs)
* [Scala](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Sources/Scala/Meta.hs)
* [SHACL](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Sources/Shacl/Model.hs)
* [SQL (ANSI)](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Sources/Sql/Ansi.hs)
* [TypeScript](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Sources/TypeScript/Model.hs)
* [XML Schema](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Sources/Xml/Schema.hs)

### Domain models

The following domain-specific models are also included:

* [Apache Atlas](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Sources/Other/Atlas.hs)
* [Azure DTLD](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Sources/Other/AzureDtld.hs)
* [Coq](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Sources/Other/Coq.hs)
* [Datalog](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Sources/Other/Datalog.hs)
* [GeoJSON](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Sources/Other/GeoJson.hs)
* [IANA Relations](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Sources/Other/IanaRelations.hs)
* [OSV](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Sources/Other/Osv.hs)
* [STAC Items](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Sources/Other/StacItems.hs)

All extensions are listed [here](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Sources/All.hs),
and the generated Haskell APIs for all of these models can be found [here](https://github.com/CategoricalData/hydra/tree/main/hydra-ext/src/gen-main/haskell).
For the sake of space, only generated Haskell is checked in to the repository, but Java APIs can be generated from GHCi (use `stack ghci`) as follows:

```haskell
-- Universe provides all modules for dependency resolution
-- modulesToGenerate specifies which modules to actually generate
writeJava "src/gen-main/java" (hydraExtModules <> kernelModules) hydraExtModules
```

The generated Haskell can be updated using:

```haskell
writeHaskell "src/gen-main/haskell" kernelModules hydraExtModules
```

## Tools

Miscellaneous tools include:
* **Analysis**: utilities for analyzing the Hydra kernel or other Hydra graphs
  * **Dependencies**: creates a property graph out of the dependency structure of a Hydra graph
  * **Summaries**: creates textual summaries of a Hydra graph, e.g. for training an LLM
* **AvroWorkflows**: transform Avro schemas and matching JSON data to one of multiple targets (RDF with SHACL, property graphs with schemas)
* **Csv**: utilities for working with CSV data
* **OsvToRdf**: transform [OSV](https://osv.dev) dumps to RDF
* **PropertyGraphToRdf**: like the name
* **Tabular**: utilities for working with tabular data

## Demos

* **GenPG**: uses an LLM to generate Hydra schemas and mappings based on tabular data sources. There is a demo video [here](https://drive.google.com/file/d/10HCElcG7n0tprOTdtX4bSa5yWYs08nV-/view?usp=sharing).
* **AvroToPropertyGraphs**: transforms a specific Avro schema and matching sample JSON to a property graph representation
* **MeteredEvaluation**: demonstrates term reduction with logging, e.g. for tracking usage or estimating cost

### GenPG

This demo provides an example of a hands-off graph generation workflow using an LLM for schema and transform design.
See the demo video linked above to understand how the demo works.
To run it yourself, you will need a small tabular dataset which illustrates the structure of your data.
You can find example datasets in
[src/main/python/hydra/demos/genpg/sales](https://github.com/CategoricalData/hydra/tree/main/hydra-ext/data/genpg/sales)
and [src/main/python/hydra/demos/genpg/health](https://github.com/CategoricalData/hydra/tree/main/hydra-ext/data/genpg/health);
the former is a reference dataset which is built into the workflow,
and the latter is a stand-in for your own domain-specific dataset.
Feel free to start with the provided `health` dataset, or insert your own from the beginning.

While in the hydra-ext directory, start by generating a prompt based on these two datasets:

```bash
python3 src/main/python/hydra/demos/genpg/generate_prompt.py data/genpg/sources/health > data/genpg/prompt.txt
```

Now, take this prompt (`data/genpg/prompt.txt`; note that this is checked in to the repository for visibility, but your command will have overwritten it) and
1. Copy it into your favorite LLM chat interface
([ChatGPT](https://chatgpt.com), [Claude](https://claude.ai), etc.; Claude was used in the video). Then,
2. Copy the LLM-generated files to their expected locations under `src/gen-main/haskell/Hydra/Ext/Demos/GenPG/Generated`.
Just overwrite the files which are already checked in at that location.
Feel free to use any file system integration available in your tool, if you want to avoid the tedium of manual copy-and-paste.

Finally, enter GHCi using:
```bash
stack ghci
```
You will need to have installed [Haskell Tool Stack](https://docs.haskellstack.org/en/stable) ("Stack") first,
as is also described in the Hydra-Haskell README.
Once in the REPL, there are two built-in demo routines you can use,
both of which run a Hydra transform to generate graph data in [GraphSON](https://github.com/apache/tinkerpop/blob/master/docs/src/dev/io/graphson.asciidoc),
a property graph serialization format.
You can then load these graphs into a TinkerPop-compatible graph database,
into [G.V()](https://gdotv.com) for visualization, etc.
The first function generates a graph based on the built-in reference dataset:
```haskell
generateExampleGraphSON
```
You will find the result at `data/genpg/sales.json`.
The other function generates a graph based on the `health` dataset,
or whatever dataset you have supplied instead:
```haskell
generateCopilotGraphSON
```
The result is written to `data/genpg/copilot.json`.

And that's it! What you have just done is to:
1. Teach the LLM how to write schemas and transforms in Hydra, using a specific reference dataset as an example.
2. Show the LLM a new dataset, and ask it to generate a schema and transform.
This is out of Hydra's control, so hopefully the LLM did a good job!
3. Use your shiny new Hydra schema and transform to generate a property graph. This phase of the demo is completely deterministic, fast, and scalable.

If you want to visualize your generated graph(s) in G.V(),
simply open the desktop interface and load the GraphSON files when you create a new graph playground.
You can re-load them with Gremlin commands like:

```gremlin
g.io("/path/to/hydra/hydra-ext/data/genpg/sales.json").read().iterate()
g.io("/path/to/hydra/hydra-ext/data/genpg/copilot.json").read().iterate()
```

Run a Gremlin query like `g.E()`, and off you go.

The Haskell sources of this demo are available [here](https://github.com/CategoricalData/hydra/tree/main/hydra-ext/src/main/haskell/Hydra/Ext/Demos/GenPG).

### AvroToPropertyGraphs

To run the `AvroToPropertyGraphs` demo, first enter `stack ghci`, then:

```haskell
import Hydra.Tools.AvroWorkflows
import Hydra.Demos.AvroToPropertyGraphs

-- Arguments
jsonLastMile = propertyGraphJsonLastMile examplePgSchema () ()
graphsonLastMile = propertyGraphGraphsonLastMile exampleGraphsonContext examplePgSchema () ()
aviationSchema = "src/test/avro/aviationdemo/AirplaneInfo.avsc"
aviationDataDir = "src/test/json/aviationdemo"
movieSchema = "src/test/avro/moviedemo/Review.avsc"
movieDataDir = "src/test/json/moviedemo"
outDir = "/tmp/avro-pg-demo/output"

-- Try a few combinations
transformAvroJsonToPg jsonLastMile aviationSchema aviationDataDir outDir
transformAvroJsonToPg graphsonLastMile aviationSchema aviationDataDir outDir
transformAvroJsonToPg jsonLastMile movieSchema movieDataDir outDir
transformAvroJsonToPg graphsonLastMile movieSchema movieDataDir outDir
```
