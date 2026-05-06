# hydra-pg-dsl

Java DSL helpers for Hydra property graphs.

Provides fluent builders and static factories for constructing values of
`hydra.pg.model.*` (Vertex, Edge, Graph, GraphSchema, …) and
`hydra.pg.query.*` (Query, SelectQuery, …) from Java code.

Contents:
- `hydra.pg.dsl.Graphs` — static factory: `vertex`, `edge`, `vertexType`, `edgeType`, `graph`, `schema`.
- `hydra.pg.dsl.Queries` — static factory: `apply`, `query`.
- `hydra.pg.dsl.{Vertex,Edge,VertexType,EdgeType}Builder` — fluent property-graph element builders.
- `hydra.pg.dsl.{Element,ElementType}Builder` — abstract bases for the above.
- `hydra.pg.Merging` — utility for merging multiple vertex/edge types into one merged type.

## Maven coordinates

```
net.fortytwo.hydra:hydra-pg-dsl:0.16.0
```

## Dependencies

- `net.fortytwo.hydra:hydra-pg` — the Hydra property-graph package
  (`hydra.pg.{model,query}`)

No third-party dependencies. This binding is unusual in that respect — most
`bindings/java/*` packages wrap a third-party library. `hydra-pg-dsl`
provides pure Java DSL surface for a Hydra package and depends only on Hydra.

## Building locally

```sh
gradle :hydra-pg-dsl:build
```

## See also

- The Hydra `bindings/` philosophy: [docs/implementation.md](../../../docs/implementation.md) principle 7.
- `hydra-pg`: the Hydra property-graph model + coders package.
- `hydra-neo4j`: a sibling binding that consumes `hydra-pg-dsl` (FromCypher uses
  `hydra.pg.dsl.Queries` to build query values).
