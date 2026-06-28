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

  > **TODO (#521): Gremlin-text parsing is currently unavailable.** The Hydra ↔ Gremlin bytecode
  > mapping and the pg ↔ TinkerGraph bridge work, but parsing Gremlin *query text* (via TinkerPop's
  > `GremlinQueryParser`) is disabled: its ANTLR runtime (4.9.1, ATN format v3) conflicts with the
  > ANTLR ≥4.10 required by the co-located Neo4j Cypher/GQL grammars, and the two parsers cannot
  > share a classpath. The `RoundTripTest` round-trip tests are `@Disabled` pending a fix. See
  > [#521](https://github.com/CategoricalData/hydra/issues/521) (recommended fix: vendor `Gremlin.g4`
  > and regenerate the parser against a modern ANTLR so Gremlin and Neo4j can coexist).
- **Graphviz coder and DOT serializer** — for rendering property graphs as diagrams.
- **PG-to-RDF mappings** — produces RDF descriptions of property-graph vertices and edges,
  for use with `hydra-rdf`.
- **PG validator** — checks property graphs against a schema; runs identically across
  Haskell, Java, and Python.
- **Neo4j model** (`hydra.neo4j.model`) — a Neo4j-flavored property-graph data model and schema
  (constraints and graph types), distinct from the TinkerPop-shaped PG model above, with element-level
  validation in `hydra.validate.neo4j` (see below).

## Neo4j model

The `hydra.neo4j.model` module is a property-graph data model shaped to match **Neo4j**, as a
counterpart to the TinkerPop-shaped `hydra.pg.model`.
It exists because Neo4j's data model is not quite TinkerPop's: it is closer to a profile of the
ISO/IEC GQL data model, and several structural differences make a faithful round-trip impossible to
express in the TinkerPop-shaped types.
A bidirectional mapping between the two models (planned) lets graph data and schemas move between the
TinkerPop and Neo4j worlds — the interoperability that Apache TinkerPop's now-removed `neo4j-gremlin`
module used to provide, rebuilt at the data-model layer rather than against a vendor runtime API.

### What it covers

The module defines both an **instance level** — the data a Neo4j graph holds — and a **schema
level** — Neo4j's constraints and graph types.

Instance level:

- `Element` — a bare union over `Node` and `Relationship` (the two kinds of graph element).
- `Node` — an `id`, a **set** of labels, and a map of properties.
- `Relationship` — an `id`, properties, a single `type`, and `start`/`end` node ids.
- `Value` — the general container for a Neo4j value, with variants for every kind the Neo4j type
  system recognizes: scalars (`boolean`, `integer`, `float`, `string`, `bytes`), spatial and temporal
  values (`Point`, `LocalDate`, `LocalTime`, `OffsetTime`, `LocalDateTime`, `ZonedDateTime`,
  `IsoDuration`), collections (`list`, `map`), the structural values (`node`, `relationship`, `path`),
  and `null`.
- `Path` and `Segment` — a path as a start node followed by a sequence of segments.

Schema level:

- `Constraint` — Neo4j's four constraint types, by their documentation names: `PropertyUniquenessConstraint`,
  `PropertyExistenceConstraint`, `PropertyTypeConstraint`, and `KeyConstraint`. A `ConstraintDefinition`
  pairs a constraint with an optional name.
- `ValueType` — the Neo4j property-type expression language used by property type constraints and graph
  type property declarations: the atomic types, plus `LIST`, `VECTOR`, and closed unions.
- `GraphType` — a Neo4j graph type (its holistic schema feature), made of `NodeElementType` and
  `RelationshipElementType` element types, each carrying its identifying/implied labels (or its type and
  endpoint labels) and its constraints.

### Design considerations

**Alignment with the Neo4j Java driver.**
The model's types mirror those of the Neo4j Java driver (`org.neo4j.driver.*`) rather than the prose
of the Neo4j documentation.
The motivation is that the host-language adapters that bridge this model to a live Neo4j must convert
to and from the driver's own types, so structural alignment with the driver keeps that adapter thin
and largely mechanical.
For the same reason, type names follow the driver's accessor return types (`LocalDate`, `OffsetTime`,
`ZonedDateTime`, `IsoDuration`, `Point`), and field names follow the driver (`start`/`end` for a
relationship's endpoints, `key` for a map key, `elementId` semantics for `id`).

**This is Neo4j, not GQL.**
Where Neo4j and GQL diverge in terminology, the model follows Neo4j: a property's name part is a
`Key` (the driver's term), not a "property name" (GQL's term).

**`Value` is a general container, like a `Term`.**
`Value` can represent anything the driver's value type can hold — including maps, structural values,
and `null` — not only the values that are legal as a *stored property*.
This mirrors how Hydra's `Term` is the general instance-data container, with types (rather than a
narrower term type) constraining what is valid in a given context.
The restriction to storable property values is therefore a schema/validation concern, not a
restriction baked into `Value`.

**Faithful where Neo4j is faithful; simpler where Neo4j is simpler.**
A node carries a *set* of labels (Neo4j allows zero or more), whereas a TinkerPop vertex has a single
label; a relationship has exactly one `type` and is always stored directed, so the instance model has
no direction field (undirected traversal is a query-level notion).
Where the model can remove redundancy without losing Neo4j's expressiveness it does — for example, a
`Segment` records only the relationship and the node it arrives at, since a segment's start node is the
previous segment's end (or the path's start).

**Every constraint is a list of properties plus a requirement.** Neo4j's four constraint types differ
only in their requirement (uniqueness, existence, type, or key) and in how many properties they range
over; modeling each as a record with a `properties` list and the requirement-specific data keeps the
set uniform. Constraints attach directly to a graph type's element types rather than floating free,
because every Neo4j constraint is scoped to exactly one label or relationship type — the element type
it lives on supplies that scope, so no separate scope field is needed. And because Neo4j has no untyped
property declaration (every property in a graph type carries a `:: TYPE`), an element type is described
entirely by its constraints, with no separate property list.

### Validation

The `hydra.validate.neo4j` module validates a Neo4j graph against a graph type, under a configurable
`ValidationProfile` (the same profile/result framework as the property-graph validator
`hydra.validate.pg`). `validateNode` and `validateRelationship` check one element against a `GraphType`;
`validateGraph` validates a whole graph (a list of nodes and relationships) and reports all violations,
each tagged with the offending element's id.

It exists because Neo4j has no such operation: Neo4j enforces constraints and graph types only at
**write time**, one element at a time, failing fast. This validator does the **retroactive,
client-side, whole-graph** check Neo4j lacks — reporting every violation at once — which is the same
niche neosemantics SHACL fills for RDF, but against Neo4j's native model and translingually.

What it checks, following Neo4j's actual semantics:

- **Matching is open-world by default.** A node is validated against every node element type whose
  identifying label it carries (a node has a label *set*, so it may match several); a relationship is
  validated against the element types whose type matches *and* whose endpoint labels match the
  relationship's endpoints (relationship types may be overloaded — `LIKES` may connect `Person→Movie`
  and `Person→Person`, so the endpoint labels select which element type applies). An element matching no
  element type is **valid** (just unconstrained), as in Neo4j's open `GRAPH TYPE`. The `strictNeo4jProfile`
  opts into closed-world validation, where a non-matching element is an error.
- **Per-property constraints** — property existence (a required property is present), property type (a
  present property's value has the declared type), and the existence aspect of key constraints.
- **Endpoint patterns** — a relationship whose type is declared but whose endpoints match no declared
  pattern is reported (`noMatchingPattern`).

The validator is intentionally **pure and client-side**: it checks only what is determinable from the
data it is handed, performs no effects, and never queries the database. This is the point — it does the
offline, pre-write, whole-graph check Neo4j's write-time enforcement cannot.

Uniqueness and the uniqueness aspect of key constraints are therefore **not yet checked** (their error
variants exist but are unproduced — a placeholder for future work). They compare values across elements,
so checking them needs either the whole graph in memory or, for incremental validation against a live
database, a query back to the server — which requires *effects* the pure validator deliberately lacks.
An effectful, server-querying variant can be added later to produce those findings.

Because Neo4j's `Value` and `ValueType` are concrete (unlike the property-graph validator, which
delegates value checking to a caller-supplied function over a polymorphic value type), the
value-against-type check is performed directly in the validator, with no caller callback. Like the rest
of the package, the validator is translingual: the same logic runs in every host language — see the
[Neo4j validation demo](https://github.com/CategoricalData/hydra/tree/main/demos/neo4j-validation),
which runs it in Java and Python against a live Neo4j via the Neo4j client driver.

### Mapping to the property-graph model

The `hydra.neo4j.pg` module maps graph *data* between this Neo4j model and Hydra's TinkerPop-shaped
property-graph model (`hydra.pg.model`): per-element (`vertexToNode` / `nodeToVertex`,
`edgeToRelationship` / `relationshipToEdge`) and whole-graph (`graphToNeo4j`, mapping a PG `Graph` to Neo4j
nodes and relationships; and `neo4jToGraph`, mapping Neo4j nodes and relationships to PG vertices and
edges, building the endpoint-label resolver from the node list internally as the validator's
`validateGraph` does). `neo4jToGraph` returns the vertices and edges as lists for the caller to assemble
into a `Graph` — a `Graph`'s id-keyed maps need an `Ord` instance on the id type that a translingual
function cannot impose on a polymorphic value type. This is the data-model-layer rebuild of the
interoperability Apache TinkerPop lost when `neo4j-gremlin` was removed.

The mapping is **not an isomorphism and not invertible** — the two models do not carry the same
information — so the design is about characterizing exactly where and how each direction loses or
constrains. Four mismatches, each with a deliberate resolution:

- **Element id** (parametric vs. `ElementId`) and **property value** (parametric vs. the closed `Value`)
  are bridged by **caller-supplied conversions**, bundled in a `Neo4jMapping` record (four partial
  functions: `encodeId` / `decodeId` / `encodeValue` / `decodeValue`, each `... -> Either string ...`).
  This follows `hydra.pg.mapping.Schema`, which already parameterizes ids/values this way. (These four
  fields are what a generalized `Coder` would express once
  [#518](https://github.com/CategoricalData/hydra/issues/518) frees the kernel `Coder` from its
  inference-context coupling; until then they are spelled out, with plain `string` errors.)
- **Relationship-type overloading + case.** A Neo4j relationship type may be overloaded across endpoint
  patterns (`LIKES` connects `Person→Movie` and `Person→Person`), but a Hydra PG edge label is unique per
  `(out-label, label, in-label)`. So Neo4j → PG expansion is **schema-conditional**: a type with a single
  `RelationshipElementType` becomes the plain recased label (`LIKES → likes`), while an overloaded type is
  disambiguated by endpoint labels (`LIKES` from a `Person` to a `Movie` → `personLikesMovie`), using
  Hydra's built-in `hydra.formatting.convertCase` (UPPER_SNAKE ↔ camelCase). PG → Neo4j simply recases
  the edge label (`likes → LIKES`) and does **not** un-expand — round trips are not expected to be the
  identity.
- **Multi-label nodes.** Neo4j nodes carry a label *set*; Hydra vertices carry a single label. Hydra PG
  is strictly more constrained, so some valid Neo4j graphs have no valid Hydra image. For now,
  `nodeToVertex` **fails on any multi-label node** (a deliberate first-pass simplification; a future
  version may salvage some multi-label graphs).

Like the validator, the mapping is pure and translingual. With mutually-inverse caller conversions and
single-label nodes, the structural parts (labels, property walk, endpoint wiring) are lossless; all
remaining loss is delegated to — and characterized by — the caller's id/value conversions.

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
