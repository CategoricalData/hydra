# Subterm graph: property-graph encoding

**Status: Draft** · Part of the [Hydra specification](../index.md)

This document specifies how a Hydra [term graph](../term-graph.md) is encoded as a property graph.
The encoding is implemented by `hydra.pg.subtermGraph.encodeTermGraph`
(`packages/hydra-pg/src/main/haskell/Hydra/Sources/Pg/SubtermGraph.hs`).
It is a building block of the property-graph work tracked in
[#298](https://github.com/CategoricalData/hydra/issues/298) and may be revised as that work proceeds.
The key words MUST, MUST NOT, SHOULD, and MAY are to be interpreted as described in RFC 2119.
Named provisions follow the convention in [index.md](../index.md#provisions);
their names are rooted at `hydra.pg.subtermGraph`.

## Status

This document describes the encoding as first implemented in #738.
It is tested by `hydra.pg.test.subtermGraph`, part of the `hydra.pg.test.testSuite` suite of the hydra-pg package.
A conforming implementation MUST produce, for every input term graph, a property graph equal to that
produced by the rules below.

## Inputs and output

The input is a `TermGraph`, as built by `graphToTermGraph` from a typed Hydra graph.
Its structure (nodes, `subterm`, `attribute` and `reference` links, node ids, and leaves)
is defined in [term-graph.md](../term-graph.md); this document does not repeat it.

The output is a `hydra.pg.model.Graph Term`: vertices and edges are keyed by id, and ids and property values are Hydra terms.

**HYDRA-PG-SUBTERM-GRAPH-ENCODE-TERM-GRAPH-TOTAL** (requirement).
`encodeTermGraph` MUST be total and pure.
Failures belong to the preceding `graphToTermGraph` step (infer, then `graphToTermGraph`, then `encodeTermGraph`).

**HYDRA-PG-SUBTERM-GRAPH-ENCODE-TERM-GRAPH-ROOTS** (requirement).
The output MUST be the union of the encodings of every root node in the input, each rooted at its binding name.

## Node ids

A node is identified by a `TermNodeId`: a root binding name and a subterm path from that root.
The *path text* of a path is its printing by `hydra.core.print.paths.subtermPath`.

**HYDRA-PG-SUBTERM-GRAPH-TERM-NODE-ID-TERM** (requirement).
The id of a node MUST be the string literal `<root>` if the path is empty, and `<root>#<path text>` otherwise.
Distinct nodes therefore have distinct ids.

## Vertices

A *leaf* is as defined in [term-graph.md](../term-graph.md#leaves).

**HYDRA-PG-SUBTERM-GRAPH-VERTEX-PER-NODE** (requirement).
The encoding MUST contain one vertex for each root node, and one for each non-leaf node reachable by `subterm` links.
It MUST NOT contain a vertex for a leaf node that is not a root.

**HYDRA-PG-SUBTERM-GRAPH-VERTEX-FIELDS** (requirement).
A vertex's id MUST be the node id.
Its label MUST be the node's type printed by `hydra.core.print.core.type`.
Its properties MUST be exactly those given in [Properties](#properties).

## Properties

Let `<path>` be the path text of a node, and `<childPath>` the path text of one of its children.

**HYDRA-PG-SUBTERM-GRAPH-PROPERTY-SOURCES** (requirement).
A vertex's properties MUST be exactly the following, and no others:

| Source                                    | Key               | Value |
|-------------------------------------------|-------------------|-------|
| each `attribute` link of the node         | `<path>@<tag>`    | the [attribute value](#attributes) |
| each leaf child, via a `subterm` link     | `<childPath>`     | the child's term |
| each primitive `reference` link           | `<childPath>`     | the primitive's name, as a string literal |

Here `<childPath>` for a reference is the node's path extended by the link's step.

**HYDRA-PG-SUBTERM-GRAPH-PROPERTY-KEY-UNIQUE** (requirement).
Attribute keys MUST carry the `@<tag>` suffix, because several attributes can attach at one node and so
share a path.

## Attributes

**HYDRA-PG-SUBTERM-GRAPH-ATTRIBUTE-VALUE** (requirement).
The tag and value of each attribute variant MUST be as follows.
"String" means a string literal; "printed type" means the output of `hydra.core.print.core.type`.

| Variant                      | Tag                         | Value |
|------------------------------|-----------------------------|-------|
| `casesTypeName`              | `casesTypeName`             | the name, as a string |
| `injectTypeName`             | `injectTypeName`            | the name, as a string |
| `lambdaDomainGiven`          | `lambdaDomainGiven`         | the given domain type, printed |
| `lambdaParameter`            | `lambdaParameter`           | the parameter name, as a string |
| `letBindingTypeSchemeGiven`  | `letBindingTypeSchemeGiven` | `<name>: <type scheme>`, the type scheme printed by `hydra.core.print.core.typeScheme` |
| `literal`                    | `literal`                   | the literal itself |
| `projectFieldName`           | `projectFieldName`          | the name, as a string |
| `projectTypeName`            | `projectTypeName`           | the name, as a string |
| `recordTypeName`             | `recordTypeName`            | the name, as a string |
| `typeApplicationType`        | `typeApplicationType`       | the type argument, printed |
| `typeLambdaParameter`        | `typeLambdaParameter`       | the parameter name, as a string |
| `unwrapTypeName`             | `unwrapTypeName`            | the name, as a string |
| `wrapTypeName`               | `wrapTypeName`              | the name, as a string |

## Edges

**HYDRA-PG-SUBTERM-GRAPH-EDGE-PER-REFERENCE** (requirement).
The encoding MUST contain one edge for each `reference` link that resolves to a lambda binder or a let binding,
and no edge for any other link.

**HYDRA-PG-SUBTERM-GRAPH-EDGE-FIELDS** (requirement).
An edge's `out` MUST be the id of the referencing node, and its `in` the id of the binder node.
Its label MUST be the path text of the reference, that is, the referencing node's path extended by the link's step.
Its id MUST be the string `<root>#<path text of the reference>`, where `<root>` is the root of the referencing node.
Its properties MUST be empty.

**HYDRA-PG-SUBTERM-GRAPH-EDGE-ID-UNIQUE** (requirement).
Edge ids MUST be unique within the output.
Because the id includes the root and a full path from it, two references never share an id.

## Example

Consider a graph with root bindings `x = 42`, `y = [x]` and `z = [x]`.

- `x` is a root whose only link is a literal attribute.
  It yields a vertex with id `x` and the literal `42` as its `@literal` property.
- `y` has one subterm child, the variable `x`, which is a reference rather than a node.
  It yields a vertex with id `y` and one edge from `y` to `x`, with id `y#<path>`.
- `z` is encoded the same way, with edge id `z#<path>`.
  The two edges have the same label but distinct ids.

The output has three vertices and two edges.
The variable is wrapped in a list on purpose; see the bare-variable-root limitation in [term-graph.md](../term-graph.md#limitations).

## Limitations

- **Bare-variable roots.** See [term-graph.md](../term-graph.md#limitations): no edge is produced for a root binding whose entire term is a variable.
- **Primitive references.** A reference to a primitive has no target node, so it is recorded as a property, not an edge.
  Queries that follow edges will not see primitive uses.

## Conformance

`hydra.pg.test.subtermGraph` (`packages/hydra-pg/src/main/haskell/Hydra/Sources/Pg/Test/SubtermGraph.hs`)
tests the node-id and tag helpers, and the vertex and edge counts of end-to-end encodings.
It currently covers the provisions on node ids, vertices, edges and edge-id uniqueness.
The property-key and attribute-value tables are not yet covered by tests.
