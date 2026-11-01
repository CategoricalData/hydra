# Term graphs

**Status: Draft** · Part of the [Hydra specification](index.md)

A **term graph** presents a typed Hydra term as a graph of its subterms.
It is the common input of the encodings that turn Hydra terms into other graph models.
Those encodings are the property-graph encoding ([pg/subterm-graph.md](pg/subterm-graph.md)),
and, planned, the RDF and SHACL coder and the GraphViz coder.
This document defines the structure those encodings consume; each encoding defines its own output.
The key words MUST, MUST NOT, SHOULD, and MAY are to be interpreted as described in RFC 2119.
Named provisions follow the convention in [index.md](index.md#provisions).

The types are defined in `hydra.core.paths` (see the [lexicon](../hydra-lexicon.txt) for their full descriptions),
and the term graph is built by `hydra.core.subterms.graphToTermGraph`.

## Structure

A `TermGraph` has one field, `roots`, mapping each binding name of the graph to a `TermNode`.
A `TermNode` has three fields:

| Field   | Meaning |
|---------|---------|
| `term`  | the subterm as written |
| `type`  | the subterm's type (a binding's forall type at a root) |
| `links` | the node's outgoing `TermLink`s |

A `TermNode` is identified by a `TermNodeId`: the name of its root binding plus the `SubtermPath` from that root.
A root node's id has an empty path.
A `letBinding` step appears in a path only for an actual nested let inside a binding's term.

## Links

A `TermLink` is one of three kinds.

| Kind        | Meaning |
|-------------|---------|
| `subterm`   | an immediate subterm, carried inline as a child node, reached by a `SubtermStep` |
| `attribute` | a non-term constituent of the node's term, such as a literal or a type name |
| `reference` | a variable or primitive occurrence, reached by a `SubtermStep` and resolved to a target |

The structure is a tree: children are inline, and sharing is expressed only by `reference` links.

### Attributes

The `TermAttributeLink` variants, and what each carries, are:

| Variant                      | Carries |
|------------------------------|---------|
| `casesTypeName`              | the name of the union type eliminated by a case statement |
| `injectTypeName`             | the name of the union type of an injection |
| `lambdaDomainGiven`          | the given domain type of a lambda |
| `lambdaParameter`            | the parameter name of a lambda |
| `letBindingTypeSchemeGiven`  | the name of a let binding with its given type scheme |
| `literal`                    | the literal value of a literal term |
| `projectFieldName`           | the field name of a projection |
| `projectTypeName`            | the record type name of a projection |
| `recordTypeName`             | the record type name of a record term |
| `typeApplicationType`        | the type argument of a type application |
| `typeLambdaParameter`        | the type-variable parameter of a type lambda |
| `unwrapTypeName`             | the wrapper type name of an unwrap |
| `wrapTypeName`               | the wrapper type name of a wrapped term |

### References

A `TermReference` is one of three kinds:

| Kind        | Target |
|-------------|--------|
| `lambda`    | the id of the node at the binding lambda (`boundByNode`) |
| `let`       | the id of the node at the bound term (`bindingNode`) |
| `primitive` | a primitive function, which has a name and a type but no node |

An encoding that maps references to edges therefore has a node to point to for `lambda` and `let` references only.

## Leaves

A node is a **leaf** if it has no `subterm` links and no `reference` links, that is, if all of its links are attributes.
Literals and unit are leaves.
Variables and primitives are never nodes; they appear as `reference` links on their parent.

## Limitations

**HYDRA-CORE-SUBTERMS-GRAPH-TO-TERM-GRAPH-BARE-VARIABLE-ROOT** (observed behavior).
`graphToTermGraph` emits a `reference` link only for a variable that occurs as a child of a term.
A root binding whose entire term is a variable has no parent in the traversal, so no link is emitted for it.
