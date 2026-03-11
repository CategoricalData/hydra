# Demos

Hydra includes several demos that illustrate different capabilities of the system.
Haskell source code for all demos lives in `hydra-ext/src/main/haskell/Hydra/Ext/Demos/`.
Runnable scripts, input data, and output files live in `hydra-ext/demos/`.

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

There is a [demo video](https://drive.google.com/file/d/10HCElcG7n0tprOTdtX4bSa5yWYs08nV-/view?usp=sharing)
walking through the LLM-assisted workflow.

See the [GenPG README](../../hydra-ext/demos/genpg/README.md) for setup, usage, and code generation instructions.

## Bootstrapping (everything-to-everything code generation)

The bootstrapping demo validates Hydra's self-hosting capability.
All three complete implementations (Haskell, Java, Python) independently load Hydra modules from a
language-independent JSON representation and regenerate code for any target language.
This produces 9 bootstrapping paths (3 host languages x 3 target languages) covering 249 modules.

The pipeline works in four steps:

1. **Export to JSON.** Hydra modules (kernel, extensions, eval lib, tests) are serialized to JSON
   with System F type annotations.
2. **Load from JSON.** A host language reads the JSON files and decodes them back into Hydra `Module` values.
3. **Generate code.** The loaded modules are passed to `writeHaskell`, `writeJava`, or `writePython`.
4. **Copy static resources.** Primitive library implementations, build files, and test runners
   are copied from the source tree.

The `bootstrap-all.sh` script runs all paths and diffs the output against the canonical baselines
in the repository, reporting pass/diff/fail per path.

See the [Bootstrapping README](../../hydra-ext/demos/bootstrapping/README.md) for prerequisites,
usage, architecture details, and known limitations.

## Avro to property graphs

This demo transforms Avro-schema'd JSON data into either GraphSON property graphs or SHACL RDF.
It uses a swappable `LastMile` abstraction that decouples the core pipeline (Avro JSON to Hydra terms)
from the output format -- switching from GraphSON to RDF requires only changing a single argument.

The underlying workflow engine is `Hydra.Ext.Tools.AvroWorkflows`, which parses Avro schemas,
decodes JSON data into typed Hydra terms, and applies the chosen last-mile transformation.

Entry point: `hydra-ext/src/main/haskell/Hydra/Ext/Demos/AvroToPropertyGraphs.hs`.

## Metered evaluation

A small, self-contained demo of Hydra's instrumented evaluation mode.
It constructs a module with two test bindings (a string concatenation expression and a type description),
reduces the terms, and prints how many times each primitive function was called during evaluation.

This demonstrates how the `Flow` monad's trace threading can be repurposed for instrumentation
beyond error reporting -- for example, counting API calls that consume time or resources.

Entry point: `hydra-ext/src/main/haskell/Hydra/Ext/Demos/MeteredEvaluation.hs`.
