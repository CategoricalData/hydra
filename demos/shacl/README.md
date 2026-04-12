# SHACL demo - RDF data generation and validation

A demo that generates [SHACL](https://www.w3.org/TR/shacl/) shapes from Hydra kernel types,
encodes kernel modules as conforming RDF data, and validates the data against the shapes
using [pyshacl](https://pypi.org/project/pyshacl/).
Non-conforming data is also generated to demonstrate that the shapes catch constraint violations.

## Quick start

```bash
cd demos/shacl
./bin/run.sh
```

This will:

1. Build a graph from the Hydra kernel type modules
2. Generate SHACL shapes from those types (N-Triples)
3. Load kernel modules from JSON and encode each as RDF data (N-Triples)
4. Generate intentionally non-conforming RDF data
5. Validate the conforming data against the shapes (expect: pass)
6. Validate the non-conforming data against the shapes (expect: fail)

## What it demonstrates

- **SHACL shapes generation** from Hydra's type system via `hydra.ext.shacl.coder`
- **RDF encoding** of typed Hydra terms via the same coder's `encodeTerm`
- **Serialization** to N-Triples via `hydra.ext.rdf.serde`
- **Positive validation**: generated RDF conforms to generated shapes
- **Negative validation**: hand-crafted invalid RDF is rejected, with specific constraint
  violations reported (missing required properties, wrong datatypes, etc.)

## Pipeline

The SHACL coder provides two capabilities:

1. **Type encoding** (`encodeType`): converts Hydra types (records, unions, literals, newtypes,
   references to named types) into SHACL shapes with `sh:property`, `sh:datatype`, `sh:node`,
   `sh:minCount`/`sh:maxCount`, and `sh:xone` constraints.

2. **Term encoding** (`encodeTerm`): converts Hydra terms (record instances, union injections,
   lists, literals, maps, sets, optionals) into RDF descriptions.

The demo runs both on the Hydra kernel:

```
Kernel type modules (hydra.core, hydra.module, hydra.graph, ...)
        |
        |  encodeType (per type element)
        v
SHACL ShapesGraph (218 shapes)
        |
        |  shapesGraphToTriples + rdfGraphToNtriples
        v
shapes.nt (N-Triples)

Kernel module JSON files (hydra-haskell/src/gen-main/json/)
        |
        |  decode JSON -> Module -> Encode.Module.module_ -> encodeTerm
        v
RDF Descriptions (per module)
        |
        |  descriptionsToGraph + rdfGraphToNtriples
        v
data.nt (N-Triples)
```

## Data source

The demo uses Hydra's own kernel as both schema and data:

- **Types**: all kernel type elements across all kernel modules. Types that the SHACL language
  can represent (records, unions, literals, newtypes, named type references, lists, sets, maps,
  optionals) become shapes. Types it cannot represent (functions, polymorphic types, type
  applications) are skipped.

- **Data**: kernel modules serialized as JSON in `hydra-haskell/src/gen-main/json/`, decoded
  back into `Module` values and encoded as RDF. Each module becomes an RDF description with
  the `hydra.module.Module` type, containing its namespace, element list, dependencies,
  and description.

## Output files

| File | Description |
|------|-------------|
| `shapes.nt` | SHACL shapes graph (~218 shapes, ~2700 triples) |
| `data.nt` | Conforming RDF data (15 module instances, ~2300 triples) |
| `invalid.nt` | Non-conforming RDF data (3 invalid instances) |

## Non-conforming data

The demo generates three intentionally invalid RDF instances:

| Instance | Type | Violation |
|----------|------|-----------|
| `urn:invalid:module1` | `hydra.module.Module` | Missing required fields: `namespace`, `elements`, `termDependencies`, `typeDependencies` |
| `urn:invalid:ns1` | `hydra.module.Namespace` | Integer value where string is expected |
| `urn:invalid:qn1` | `hydra.module.QualifiedName` | Missing required field: `local` |

## Prerequisites

- Stack (for the Haskell driver)
- pyshacl (`pip install pyshacl`)

## Options

```bash
./bin/run.sh [OPTIONS]

  --hosts LANG,...     Run only specified hosts (default: haskell)
  --tag TAG            Append a tag to the run directory name
  --skip-validate      Skip pyshacl validation step
```

## Translingual support

The demo currently runs in Haskell only. The SHACL coder (`encodeType`, `encodeTerm`), the RDF
serde (`rdfGraphToNtriples`), and the SHACL/RDF model types are all generated into Java and Python,
so Java and Python drivers are feasible. To make the demo fully translingual:

1. **Regenerate Java and Python** from the updated SHACL coder source so that the `TypeVariable`
   support and `withType` fix propagate to those languages.
2. **Write Java and Python drivers** following the same pattern as the Haskell driver: load
   modules from JSON, call the coder, serialize to N-Triples.
3. **Port or promote `ShaclRdf`** (the shapes-to-RDF-triples serializer). This is currently
   hand-written Haskell. It could be promoted to a Hydra DSL module so it gets generated
   into all target languages automatically, or ported manually.

## Directory structure

```
demos/
  shacl/
    README.md                                   # This file
    bin/run.sh                                  # Orchestrator script
  src/main/
    haskell/Hydra/Ext/Demos/Shacl/
      Demo.hs                                   # Haskell driver
      ShaclRdf.hs                               # ShapesGraph -> RDF triples serializer
packages/hydra-rdf/
  src/main/
    haskell/Hydra/Ext/Sources/Shacl/
      Coder.hs                                  # SHACL coder (DSL source)
dist/haskell/hydra-rdf/
  src/main/
    haskell/Hydra/Ext/Shacl/
      Coder.hs                                  # SHACL coder (generated)
```
