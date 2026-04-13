# GraphQL JSON demo - querying Hydra modules with GraphQL

This demo generates a GraphQL schema from Hydra kernel types, then uses it to
query the existing kernel JSON modules. It demonstrates the two parallel pipelines
at the heart of Hydra's data model:

- **Types to schema**: Hydra type definitions are mapped to a GraphQL schema via the GraphQL coder
- **Terms to data**: Hydra module instances are serialized to JSON via the term encoder + JSON encoder pipeline

The generated GraphQL schema and the JSON data are structurally compatible, so a
standard GraphQL engine can resolve queries against the JSON directly.

## Directory structure

```
demos/
├── graphql-json/
│   ├── README.md                          # This file
│   ├── query.py                           # Python script that runs GraphQL queries
│   ├── queries/                           # GraphQL query files
│   │   ├── list-modules.graphql           # List all module namespaces
│   │   ├── dependents-of.graphql          # Find modules depending on a given namespace
│   │   ├── module-details.graphql         # Get details of a specific module
│   │   ├── search.graphql                 # Search modules by description keyword
│   │   └── element-counts.graphql         # Rank modules by number of elements
│   └── output/                            # Generated GraphQL schema files
│       └── hydra/
│           ├── module.graphql             # Schema for hydra.packaging types
│           └── util.graphql               # Schema for hydra.util types (Pair, Either, etc.)
demos/src/main/haskell/Hydra/Demos/
│   └── GraphqlJson.hs                    # Haskell demo module (schema generation)
```

The JSON data lives in the existing kernel at
`dist/json/hydra-kernel/src/main/json/hydra/` and does not need to be regenerated.

## Prerequisites

- GHCi with hydra-haskell loaded (for schema generation)
- Python 3.12+ with `graphql-core` installed

```bash
pip install graphql-core
```

## Usage

### Step 1: Generate the GraphQL schema

From the `heads/haskell/` directory, start GHCi and run the demo:

```bash
stack ghci hydra-haskell:lib --ghci-options='+RTS -K256M -A32M -RTS'
```

```haskell
import Hydra.Sources.Demos.GraphqlJson
demoGraphqlJson
```

This writes the schema files to `demos/graphql-json/output/hydra/`.

### Step 2: Run GraphQL queries

From the repo root:

```bash
python3 demos/graphql-json/query.py
```

## Example queries

### Find all modules that depend on hydra.rewriting

`queries/dependents-of.graphql`:
```graphql
query ($ns: String!) {
  dependentsOf(namespace: $ns) {
    namespace { value }
    description
  }
}
```

Output (45 modules):
```
hydra.adapt: Simple, one-way adapters for types and terms
hydra.annotations: Utilities for reading and writing type and term annotations
hydra.checking: Type checking and type reconstruction (type-of)...
hydra.inference: Type inference following Algorithm W...
...
```

### Get details of a specific module

`queries/module-details.graphql`:
```graphql
query ($ns: String!) {
  module(namespace: $ns) {
    namespace { value }
    description
    termDependencies { value }
    typeDependencies { value }
  }
}
```

Output:
```
Namespace: hydra.packaging
Description: A model for Hydra namespaces and modules
Term dependencies: ['hydra.graph']
Type dependencies: ['hydra.core']
```

### Search modules by description

`queries/search.graphql`:
```graphql
query ($q: String!) {
  search(query: $q) {
    namespace { value }
    description
  }
}
```

## Translingual potential

This demo currently uses Haskell for schema generation and Python for query
execution, but the architecture is naturally translingual. The GraphQL coder
(`moduleToGraphql`) is a generated function that can be produced for any Hydra
target language (Java, Python, Scala, etc.) via the standard sync scripts. The
JSON data is already language-independent. Any language with a GraphQL library
can load the schema and JSON and run queries, so all three steps (schema
generation, data loading, query execution) could run in Java, Python, or any
other supported language.

## How it works

The demo exercises two core Hydra capabilities in parallel:

1. **GraphQL coder** (`Hydra.Graphql.Coder`): converts Hydra type definitions
   into GraphQL SDL. Record types become GraphQL object types; unit-variant unions
   become enums; data-carrying unions become object types with nullable fields.
   Built-in Pair and Either type constructors are mapped to the named fallback types
   `hydra.util.Pair` and `hydra.util.Either`.

2. **JSON encoding pipeline** (`moduleToJson`): converts Hydra module instances to
   JSON using a two-step process: first a generated term encoder converts the typed
   Haskell value to a `Core.Term`, then the JSON encoder converts the `Term` to a
   JSON value. These JSON files are already present in the kernel at
   `dist/json/hydra-kernel/src/main/json/hydra/`.

Because both pipelines are derived from the same Hydra type definitions, the GraphQL
schema and the JSON data are structurally aligned. The Python query script bridges the
two by loading the schema and data, then resolving GraphQL queries against the JSON
objects.
