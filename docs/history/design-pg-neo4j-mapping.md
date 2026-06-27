# Proposal: a mapping between `hydra.pg.model` (TinkerPop) and `hydra.neo4j.model`

Status: proposal / in-flight design (not built). For #510.

## Goal

A mapping between Hydra's two property-graph instance models — the TinkerPop-shaped
`hydra.pg.model` (`Vertex`/`Edge`) and the Neo4j-shaped `hydra.neo4j.model`
(`Node`/`Relationship`) — so graph *data* can move between the TinkerPop and Neo4j worlds. This is the
data-model-layer rebuild of the interoperability Apache TinkerPop lost when `neo4j-gremlin` was removed.

## It is not an isomorphism, and not invertible

The two models do not carry the same information, and the mapping is **not** designed to round-trip
cleanly — we should not expect `Neo4j → PG → Neo4j` (or the reverse) to return the original. The two
directions are different transformations with different inputs, and that is fine.

There are three sources of asymmetry:

1. **User-supplied conversions** for ids and values absorb the parametric-`v`-vs.-concrete mismatch
   (mismatches 1–2), following `hydra.pg.mapping.Schema`'s precedent.
2. **Relationship-type overloading** (mismatch 3): one Neo4j type expands into several PG edge labels,
   *conditionally on the schema*, and the case convention differs. The PG → Neo4j direction does not try
   to un-expand; it simply recases. So this is lossy/non-invertible by design.
3. **Hydra PG is strictly more constrained than Neo4j** (mismatch 4): some valid Neo4j graphs have **no**
   valid Hydra PG image at all. The clearest case is **multi-label nodes** — Neo4j nodes carry a label
   *set*, Hydra vertices carry a single label. **For the first version, the mapping simply fails on any
   multi-label node.** This is a deliberate simplification (a smarter future version could salvage some
   multi-label graphs), and it conveniently removes every hard case below.

The two models, field by field:

| Concept | TinkerPop `hydra.pg.model` | Neo4j `hydra.neo4j.model` |
|---------|----------------------------|----------------------------|
| element id | `v` (parametric) | `ElementId` (wraps string) |
| node/vertex classifier | **single** `VertexLabel` | **set** `set<NodeLabel>` |
| edge/rel classifier | `EdgeLabel` (single) | `RelationshipType` (single) |
| endpoints | `out : v`, `in : v` | `start : ElementId`, `end : ElementId` |
| property value | parametric `v` | concrete closed `Value` |
| property key | `PropertyKey` (wraps string) | `Key` (wraps string) |

`Vertex v`/`Edge v` are parameterized by a value type `v` (ids and property values); `Node`/
`Relationship` fix the id to a string-`ElementId` and the value to the closed `Value`. So the mapping is
between `Vertex v` / `Edge v` *for a particular `v`* and `Node` / `Relationship`, mediated by two user
conversions (ids and values).

(Meta-properties and multi-properties are **not** in scope: Hydra's property-graph model does not support
them — a property key maps to a single value — so there is no mismatch to handle.)

## The four mismatches and how each is resolved

1. **Element id: parametric `v` vs. `ElementId` — user-supplied id conversion pair.**
   The caller provides `encodeId : v → Either string ElementId` and `decodeId : ElementId → Either string
   v`, the same kind of parameter `hydra.pg.mapping.Schema` supplies as a `Coder` for vertex/edge ids.
   Targeting `ElementId` (not the bare string it wraps) keeps it symmetric with the value pair and saves
   the mapping a wrap/unwrap step. Any loss/failure is the caller's, surfaced via `Either`.

2. **Property value: parametric `v` vs. closed `Value` — user-supplied value conversion pair.**
   The caller provides `encodeValue : v → Either string Value` and `decodeValue : Value → Either string
   v`. Property maps are mapped key-by-key (`PropertyKey` ↔ `Key`, both string wrappers). Any failure is
   the caller's, surfaced via `Either`.

3. **Relationship-type overloading + case — schema-conditional expansion (Neo4j → PG only).**
   In Neo4j a relationship type may be *overloaded* across endpoint patterns (`LIKES` connects
   `Person→Movie` *and* `Person→Person` — the fact the validator handles via `noMatchingPattern`). In
   Hydra PG an `EdgeType`/`EdgeLabel` is **unique per (out-label, label, in-label)**, so an overloaded
   type must expand. Neo4j types are UPPER_SNAKE_CASE (`LIKES`); PG edge labels are camelCase (`likes`).
   The mapping uses Hydra's built-in `hydra.formatting.convertCase`
   (`CaseConvention = camel | pascal | lowerSnake | upperSnake`), never hand-rolled munging.
   - **Neo4j → PG.** *Conditional on the schema:* if the relationship's type has a **single**
     `RelationshipElementType` in the `GraphType`, the edge label is just `convertCase upperSnake camel`
     of the type (`LIKES → likes`). If the type is **overloaded** (multiple element types), disambiguate
     using the endpoint labels: `LIKES` from a `Person` to a `Movie` → `personLikesMovie`
     (`decapitalize(convertCase pascal camel "Person") ++ capitalize(convertCase upperSnake camel "LIKES")
     ++ capitalize(convertCase pascal camel "Movie")`). Because multi-label nodes are rejected (mismatch
     4), the endpoint labels are single and the disambiguation is unambiguous.
   - **PG → Neo4j.** Simply `convertCase camel upperSnake` of the edge label (`likes → LIKES`,
     `personLikesMovie → PERSON_LIKES_MOVIE`). **No un-expansion is attempted** — the mapping does not try
     to recover `LIKES` from `personLikesMovie`. Round trips therefore drift (`LIKES → personLikesMovie →
     PERSON_LIKES_MOVIE`), by design.

4. **Hydra PG strictly more constrained: multi-label nodes — fail.**
   Neo4j nodes carry a label *set*; Hydra vertices carry a single `VertexLabel`. There is no general,
   lossless way to represent an arbitrary Neo4j multi-label node as a Hydra vertex (and, combined with
   overloading, a multi-label node could satisfy several edge patterns at once with no single PG edge to
   represent it). **For v1, `neo4jToGraph` fails on any node with more than one label**, with a
   characterized error. This is the honest expressiveness gap: Hydra PG is stricter, so some valid Neo4j
   graphs have no valid Hydra image, and the mapping reports that rather than silently corrupting.
   Single-label nodes map directly: `NodeLabel ↔ VertexLabel` (just the wrapped string).
   *Future:* not every multi-label graph is unrepresentable; a later version could pick a label
   deterministically (e.g. first alphabetically) and let validation decide, but v1 keeps it simple.

## Proposed function signatures

**Nodes/vertices** map with just the `Neo4jMapping v` (`nodeToVertex` fails on multi-label nodes):

```
vertexToNode :: Neo4jMapping v -> Vertex v -> Either string Node
nodeToVertex :: Neo4jMapping v -> Node     -> Either string (Vertex v)   -- fails if |labels| > 1
```

**Relationships/edges**: Neo4j → PG needs the endpoint labels (for the schema-conditional expansion) and
the `GraphType` (to know whether the type is overloaded); PG → Neo4j needs neither (plain recase). The
**whole-graph** functions are the primary surface — they build the endpoint-label resolver from the node
list internally (as `validateGraph` does) and carry the `GraphType`:

```
-- whole-graph (primary surface)
graphToNeo4j :: Neo4jMapping v -> Graph v -> Either string ([Node], [Relationship])
neo4jToGraph :: Neo4jMapping v -> GraphType -> [Node] -> [Relationship] -> Either string (Graph v)

-- per-edge (lower-level)
edgeToRelationship :: Neo4jMapping v -> Edge v -> Either string Relationship
   -- ^ PG -> Neo4j: just convertCase camel->upperSnake; no schema needed
relationshipToEdge :: Neo4jMapping v -> GraphType -> NodeLabel -> NodeLabel -> Relationship -> Either string (Edge v)
   -- ^ Neo4j -> PG: GraphType decides single vs. overloaded; start/end labels for disambiguation
```

Note the asymmetry in the per-edge signatures: `edgeToRelationship` (PG → Neo4j) takes no `GraphType`,
while `relationshipToEdge` (Neo4j → PG) needs both the `GraphType` and the (single) endpoint labels.

Internally the functions use: the mapping's `encodeId`/`decodeId` for ids/endpoints,
`encodeValue`/`decodeValue` over property maps, and `hydra.formatting.convertCase` for the edge-label ↔
relationship-type case and schema-conditional expansion.

## Characterization

- **Nodes.** `vertexToNode` is total (given a total `encodeId`/`encodeValue`). `nodeToVertex` is
  **partial**: it fails on multi-label nodes (the expressiveness gap), and on id/value-decode failures.
- **Edges.** PG → Neo4j is total and trivial (recase). Neo4j → PG is total once the node side has
  succeeded (single-label endpoints guaranteed), schema-conditionally expanding overloaded types.
- **Not invertible, by design.** `Neo4j → PG → Neo4j` drifts (overloaded types become their expanded,
  recased forms; e.g. `LIKES → personLikesMovie → PERSON_LIKES_MOVIE`). We do not claim or test round-
  trip identity. The useful guarantees are *forward* ones: a graph that maps successfully maps to a
  well-formed graph in the other model, and a Neo4j graph that *cannot* map (multi-label nodes) is
  reported, not corrupted.

The connection to validation: a Neo4j graph that `neo4jToGraph` rejects is one Hydra PG cannot represent
— the mapping failure and a (stricter) PG-schema validation failure are two views of the same gap.

## Resolved decisions

- **Multi-label nodes (resolved).** v1 **fails** `neo4jToGraph` on any node with more than one label.
  No comma-joining, no label-set encoding, no PG-side label constraint — single-label nodes only, so
  `NodeLabel ↔ VertexLabel` is a direct wrap/unwrap. (A future version may salvage some multi-label
  graphs; out of scope now.)
- **No round-trip recovery (resolved).** PG → Neo4j is `convertCase camel upperSnake`; it does not un-
  expand `personLikesMovie` back to `LIKES`. The mapping is deliberately non-invertible.
- **Bundle the mapping (resolved).** A `Neo4jMapping v` record; functions take a single `Neo4jMapping v`
  (paralleling `hydra.pg.mapping.Schema`). **For now**, because the kernel `Coder` is unusable (see #518),
  `Neo4jMapping` specifies the conversions as **pairs of partial functions** (not `Coder`s) with **plain
  `string` errors** (no error type parameter):
  ```
  Neo4jMapping v = record {
    encodeId    : v -> Either string ElementId,
    decodeId    : ElementId -> Either string v,
    encodeValue : v -> Either string Value,
    decodeValue : Value -> Either string v
  }
  ```
  This is the *shape* #518's fixed `Coder` will have. Once #518 lands, `Neo4jMapping` collapses to
  `{ idCoder : Coder v ElementId, valueCoder : Coder v Value }` — same content, re-bundled.
- **Coder shape (resolved — do NOT reuse the kernel `Coder` as it currently stands).** The kernel `Coder`
  is over-specialized: it pins both the input context (to `InferenceContext`) and the error type (to the
  concrete `Error`), neither of which belongs in a generic bidirectional transformation. Tracked as
  **[#518](https://github.com/CategoricalData/hydra/issues/518)**, whose preferred fix is the context-
  free, error-parametric form `forall v1 v2 e. { encode : v1 -> Either e v2, decode : v2 -> Either e v1 }`.
  **Sequencing note:** #518 is a natural prerequisite — build the mapping on the fixed `Coder` rather than
  a throwaway local type.

## Open questions for discussion

1. **Edge-label expansion shape.** For overloaded types, is `out ++ Type ++ In` (`personLikesMovie`) the
   right form, or `Type` first, or a delimiter? (Only affects the Neo4j → PG label *spelling*, since we
   don't parse it back.)
2. **Where it lives.** A new module — `hydra.neo4j.mapping` or `hydra.pg.neo4j.mapping`? Pure,
   translingual, no effects, like the validator.
3. **Sequencing vs. #518.** Build after #518 fixes `Coder` (clean, reuses the kernel `Coder` —
   preferred), or build now against a local context-free coder and switch later?
4. **Forward laws to property-test.** Which guarantees do we commit to — e.g. "a graph that maps
   successfully yields a well-formed target graph", "single-label round-trips preserve labels/ids/values
   under inverse coders", "multi-label nodes are always rejected"?
