# Demos

Hydra includes several demos that illustrate different capabilities of the system.
Haskell source code for all demos lives in `demos/src/main/haskell/Hydra/Demos/`.
Runnable scripts, input data, and output files live in `demos/`.

## GenPG (CSV to property graph)

GenPG demonstrates end-to-end transformation of relational CSV data into a property graph
in [GraphSON](https://github.com/apache/tinkerpop/blob/master/docs/src/dev/io/graphson.asciidoc) 3.0
format, suitable for import into TinkerPop-compatible graph databases (JanusGraph, Amazon Neptune, etc.).

The demo is **translingual**: a single Hydra source module generates Haskell, Python, and Java implementations
with equivalent semantics.
Users define a database schema (CSV table structures), a graph schema (vertex and edge types),
and a declarative mapping between them using Hydra's phantom-typed meta-DSL.
The pipeline reads CSV files, decodes typed values, applies the mapping, and writes GraphSON output.

Two example datasets are included: a **Sales** domain (employees, departments, customers, products,
transactions) and a **Health** domain (doctors, patients, appointments, prescriptions).
An LLM-assisted workflow lets users generate schemas for new domains by feeding example CSV data
and reference schemas into a structured prompt.

Demo videos:
[Part 1](https://www.linkedin.com/posts/joshuashinavier_in-case-you-were-wondering-what-i-have-been-activity-7358601538463830017-U5YE) and
[Part 2](https://www.linkedin.com/posts/joshuashinavier_here-is-part-2-of-the-hydra-property-graph-activity-7358601988755910657-HnCh)
walk through the LLM-assisted workflow.

See the [GenPG README](../demos/genpg/README.md) for setup, usage, and code generation instructions.

## Bootstrapping (everything-to-everything code generation)

The bootstrapping demo validates Hydra's self-hosting capability.
All five complete implementations (Haskell, Java, Python, Scala, and Lisp) independently load
Hydra modules from a language-independent JSON representation and regenerate code for any
target language.
The default demo uses Haskell, Java, and Python as both hosts and targets,
producing 9 bootstrapping paths.
Scala and Lisp can also be included as hosts and targets.

The pipeline works in four steps:

1. **Export to JSON.** Hydra modules (kernel, extensions, eval lib, tests) are serialized to JSON
   with System F type annotations.
2. **Load from JSON.** A host language reads the JSON files and decodes them back into Hydra `Module` values.
3. **Generate code.** The loaded modules are passed to `writeHaskell`, `writeJava`, or `writePython`.
4. **Copy static resources.** Primitive library implementations, build files, and test runners
   are copied from the source tree.

The `bin/bootstrap-all.sh` script runs all paths and diffs the output against the canonical baselines
in the repository, reporting pass/diff/fail per path.

See the [Bootstrapping README](../demos/bootstrapping/README.md) for prerequisites,
usage, architecture details, and known limitations.

## ValidatePG (property graph validation)

ValidatePG validates property graphs against a schema, exercising every validation condition
in `hydra.pg.validation` (missing required properties, type mismatches, unknown labels,
wrong edge endpoints, etc.).

The demo is **translingual**: a single set of example data (a schema and twelve graphs encoded
as JSON via `hydra.encode.pg.model`) is consumed by drivers in Haskell, Java, and Python.
Each driver decodes the JSON, runs `hydra.pg.validation.validateGraph`, and prints the results.
An orchestrator script runs all three, compares their output, and displays a timing summary.

See the [ValidatePG README](../demos/validatepg/README.md) for setup and usage.

## SHACL (RDF data generation and validation)

This demo generates [SHACL](https://www.w3.org/TR/shacl/) shapes from Hydra's kernel types,
encodes kernel modules as conforming RDF data, and validates the data against the shapes
using [pyshacl](https://pypi.org/project/pyshacl/).
Intentionally non-conforming data is also generated and validated to demonstrate that the
shapes catch constraint violations (missing required properties, wrong datatypes, etc.).

The demo uses Hydra's own type system as the subject: all kernel type elements that the SHACL
language can represent (records, unions, literals, newtypes, named type references, collections)
become SHACL shapes. The kernel modules serialized as JSON in `dist/json/hydra-kernel/src/main/json/`
are decoded back into `Module` terms and encoded as RDF data conforming to those shapes.

The demo currently runs in Haskell. The SHACL coder and RDF serde are generated into Java and
Python as well, so translingual support is feasible.

See the [SHACL README](../demos/shacl/README.md) for setup and usage.

## Avro bidirectional coder

This demo exercises Hydra's bidirectional Avro coder, which converts between Avro schemas/data
and Hydra types/terms in both directions.
It runs five sub-demos:

1. **Forward pipeline.** Parses a real `.avsc` schema (`Review.avsc`) and example JSON data,
   converts them to Hydra types and terms via the forward adapter, and round-trips the terms
   back to JSON.
2. **Reverse pipeline.** Defines Hydra types programmatically (a `Person` record with a nested
   `Address`, optional email, and a list of tags), encodes them as an Avro schema, and serializes
   a sample term to Avro-conformant JSON.
3. **Round-trip.** Loads the `AirplaneInfo.avsc` schema (a complex real-world schema with nested
   records, enums, optional fields, and annotations), converts to Hydra types via the forward
   adapter, then converts back to an Avro schema via the reverse encoder and compares structure.
4. **Schema codec.** Constructs an Avro schema in memory, serializes it to a JSON string via
   `avroSchemaStringCoder`, parses it back, and verifies the round-trip.
5. **Avro to property graph.** Parses `Review.avsc` and example JSON data, converts through the
   forward adapter, extracts property graph elements from Hydra-specific annotations
   (`@vertexLabel`, `@edgeLabel`, `@vertexId`, `@outVertex`, `@inVertex`), and produces
   [GraphSON](https://github.com/apache/tinkerpop/blob/master/docs/src/dev/io/graphson.asciidoc)
   3.0 output with vertices, edges, and properties.

The underlying workflow engine is `Hydra.Tools.AvroWorkflows`, which parses Avro schemas,
decodes JSON data into typed Hydra terms, and applies the chosen last-mile transformation.

See the [Avro README](../demos/avro/README.md) for setup and usage.

## GraphQL JSON (querying Hydra modules with GraphQL)

This demo generates a GraphQL schema from Hydra kernel types, then uses it to query the
existing kernel JSON modules. It demonstrates the two parallel pipelines at the heart of
Hydra's data model: types map to a GraphQL schema via the GraphQL coder, while terms
(module instances) are already serialized to JSON via the term encoder + JSON encoder pipeline.
Because both pipelines derive from the same type definitions, the schema and data are
structurally aligned, and a standard GraphQL engine can resolve queries against the JSON.

The demo uses Haskell for schema generation and Python (with `graphql-core`) for query execution.
Five example queries are included: listing modules, finding dependents of a namespace,
inspecting module details, searching by description, and ranking modules by element count.
The architecture is naturally translingual -- the GraphQL coder is a generated function that
could run in any Hydra target language.

See the [GraphQL JSON README](../demos/graphql-json/README.md) for setup, usage,
and example queries.

## Metered evaluation

A small, self-contained demo of Hydra's instrumented evaluation mode.
It constructs a module with two test bindings (a string concatenation expression and a type description),
reduces the terms, and prints how many times each primitive function was called during evaluation.

This demonstrates how the `Flow` monad's trace threading can be repurposed for instrumentation
beyond error reporting -- for example, counting API calls that consume time or resources.

Entry point: `demos/src/main/haskell/Hydra/Demos/MeteredEvaluation.hs`.
