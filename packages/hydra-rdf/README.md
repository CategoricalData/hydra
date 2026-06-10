# Hydra-RDF

`hydra-rdf` is the translingual RDF support package for Hydra. It is generated into
Haskell, Java, and Python from a common set of DSL sources, and depends only on `hydra-kernel`.

For background on how Hydra relates to RDF and SHACL, see the
[RDF wiki page](https://github.com/CategoricalData/hydra/wiki/RDF).

## What it provides

- **RDF 1.1 syntax model** — IRIs, literals, blank nodes, triples, graphs, datasets.
- **SHACL syntax model and coder** — maps Hydra types to SHACL shapes and Hydra terms to
  RDF descriptions (the data triples that conform to those shapes).
- **OWL 2 syntax model** — full coverage of OWL 2 classes, properties, individuals, and axioms.
  Syntax only; there is no OWL coder yet.
- **ShEx syntax model.**
- **XML Schema syntax model.**
- **N-Triples serializer** (`rdfGraphToNtriples`) — the only output format currently supported.
  Reading (parsing) of any RDF format is not yet supported.

## Demos

Three demos exercise this package end-to-end:

- [demos/shacl](https://github.com/CategoricalData/hydra/tree/main/demos/shacl) — generates SHACL shapes from the Hydra kernel types,
  encodes kernel modules as N-Triples, and validates the data against the shapes.
- [demos/avro](https://github.com/CategoricalData/hydra/tree/main/demos/avro) — bidirectional Avro coder; the included
  AvroWorkflows tool transforms Avro-schema'd JSON data into SHACL RDF.
- [demos/genpg](https://github.com/CategoricalData/hydra/tree/main/demos/genpg) — generates a property graph from relational CSV data
  and emits it as either GraphSON or RDF/SHACL N-Triples.

## N-Triples output

The serializer follows the [N-Triples 1.1 specification](https://www.w3.org/TR/n-triples/).
Literals preserve all Unicode verbatim and escape only `"`, `\`, LF, and CR. IRIs emit any
of the spec-disallowed characters (controls and `<>"{}|^`\\`) as `\uXXXX` UCHAR escapes;
all other code points pass through. Earlier versions replaced any non-ASCII character with
`?`; that bug was fixed in [#363](https://github.com/CategoricalData/hydra/issues/363).
