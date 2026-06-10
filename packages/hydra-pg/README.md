# Hydra-PG

`hydra-pg` is the translingual support package for the property graph data model in Hydra.
It is generated into Haskell, Java, and Python from a common set of DSL sources, and depends
on `hydra-kernel` and `hydra-rdf`.

For background on Hydra's approach to property graphs, see the
[Property graphs wiki page](https://github.com/CategoricalData/hydra/wiki/Property-graphs).

## What it provides

- **PG data model** — vertices, edges, properties, and graph schemas, with a coder that maps
  Hydra types and terms onto graph elements.
- **GraphSON coder** — emits TinkerPop-compatible GraphSON 3.0 from Hydra-PG graphs.
- **Cypher syntax model** — the openCypher grammar plus Cypher feature/function metadata.
- **GQL syntax model** — the ISO/IEC GQL grammar and a path-algebra syntax model.
- **TinkerPop syntax model** — Gremlin grammar plus TinkerPop feature metadata.
- **Graphviz coder and DOT serializer** — for rendering property graphs as diagrams.
- **PG-to-RDF mappings** — produces RDF descriptions of property-graph vertices and edges,
  for use with `hydra-rdf`.
- **PG validator** — checks property graphs against a schema; runs identically across
  Haskell, Java, and Python.

## Demos

Four demos exercise this package end-to-end:

- [demos/avro](https://github.com/CategoricalData/hydra/tree/main/demos/avro) — bidirectional Avro coder; one of its modes maps
  Avro data to a TinkerPop-annotated property graph and emits GraphSON.
- [demos/genpg](https://github.com/CategoricalData/hydra/tree/main/demos/genpg) — generates a property graph from relational CSV data
  and emits it as either GraphSON or RDF/SHACL N-Triples.
- [demos/pg-formats](https://github.com/CategoricalData/hydra/tree/main/demos/pg-formats) — shows three views of the TinkerPop "Modern"
  reference graph: Hydra-PG JSON, GraphSON 3.0, and JSON Schema.
- [demos/validatepg](https://github.com/CategoricalData/hydra/tree/main/demos/validatepg) — translingual property-graph validation,
  running the same Hydra validation logic in Haskell, Java, and Python.

## See also

- [HydraPop](https://github.com/CategoricalData/HydraPop) — translingual extensions
  for Apache TinkerPop, built on top of this package.
