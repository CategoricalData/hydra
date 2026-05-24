# hydra-neo4j

Java binding for Neo4j-flavored graph query languages: openCypher and GQL.

Contents:
- ANTLR grammar for **openCypher** (Neo4j's spec) under `src/main/antlr/org/neo4j/Cypher.g4`.
  The ANTLR plugin generates `org.neo4j.{CypherLexer, CypherParser}` at build time.
- ANTLR grammar for **GQL** (ISO/IEC 39075:2024) under
  `src/main/antlr/net/fortytwo/hydra/gql/parser/GQL.g4`. Generates
  `net.fortytwo.hydra.gql.parser.{GQLLexer, GQLParser}` — a neutral
  Hydra-scoped namespace, since GQL is an ISO standard, not Neo4j-owned.
- `hydra.cypher.CypherReader` — wrapper around the generated Cypher parser.
- `hydra.cypher.FromCypher` — converts parsed Cypher ASTs to Hydra's `hydra.pg.query.*`
  model (the language-independent property-graph query AST).
- `hydra.gql.GQLReader` — wrapper around the generated GQL parser. (No FromGQL yet.)

## Maven coordinates

```
net.fortytwo.hydra:hydra-neo4j:0.16.0
```

## Dependencies

- `net.fortytwo.hydra:hydra-pg-dsl` — Java DSL helpers for hydra-pg (transitively
  pulls hydra-pg and hydra-kernel)
- `org.antlr:antlr4-runtime` — ANTLR parser runtime
- `org.apache.commons:commons-text` — used for string-literal escaping in `FromCypher`

## Building locally

```sh
gradle :hydra-neo4j:build
```

ANTLR generates lexer/parser sources under `build/generated-src/antlr/main/` before
the Java compile step. No checked-in generated code.

## Status (2026-05-06)

**Compiles cleanly** via `(cd heads/java && ./gradlew :hydra-neo4j:compileJava)`. ANTLR generates
each grammar's classes from its `@header { package ...; }` directive — Cypher
classes land in `org.neo4j`, GQL classes in `net.fortytwo.hydra.gql.parser`.

## Note on naming

The binding is named "neo4j" because openCypher is Neo4j's project. GQL is
included here for now (a single binding for two related-but-distinct property-graph
query languages) but its generated classes live in the **neutral** namespace
`net.fortytwo.hydra.gql.parser`, not `org.neo4j`, since GQL is an ISO standard
(ISO/IEC 39075:2024) and not Neo4j-owned. If a separate `bindings/java/hydra-gql/`
binding is created later, the GQL grammar + reader can move there with no
package renames needed.

## See also

- The Hydra `bindings/` philosophy: handwritten host-language adapters that wire Hydra
  packages to external libraries, kept separate from `heads/` runtimes (which stay
  third-party-free) and from `packages/` (which contain DSL-based module definitions).
- [`docs/implementation.md`](../../../docs/implementation.md) principle 7.
