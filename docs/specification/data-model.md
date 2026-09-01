<!-- NOTE: hand-authored spec chapter (not a generated module page). Early rough draft under #579.
     Written as a dense collection of falsifiable requirements about the core data model: each should
     be something a conforming implementation can be tested against and fail. Pure exposition is left
     to the informative wiki Concepts page and cross-linked, not repeated. The exhaustive per-variant
     catalogs of hydra.core / hydra.graph are GENERATED module pages (#723) this chapter closes over.
     Formalism: paper #197; inference realignment #377. Requirement names + wording need human review. -->

# The data model

**Status: Draft** · Part of the [Hydra specification](index.md)

> ⚠️ **Rough draft.** This chapter is an early draft. The requirement names and the exact wording of
> individual requirements need close human review — some are known to be imprecise or provisional, and
> both the identifiers and the claims they carry may change. Do not treat any requirement here as
> settled.

This chapter states the normative structure of Hydra's core data model: the closed sets of term and
type constructs, their well-formedness conditions, and the graph view of a term. It is the conformance
surface for the *core representation* — the requirements a conforming implementation is measured
against. For an informative, first-principles introduction to the same model, see the
[Concepts](https://github.com/CategoricalData/hydra/wiki/Concepts) wiki page; this chapter does not
repeat that exposition. The exhaustive per-variant reference lives in the generated
[`hydra.core`](modules/hydra.core.md) and [`hydra.graph`](modules/hydra.graph.md) module pages, whose
variant sets this chapter closes over.

**Provisions.** Non-derivable conditions — behavioral or semantic claims that cannot simply be read off
a definition's signature or variant set — are stated as named **provisions**, shown in bold at the head
of the claim (e.g. **[HYDRA-DM-VOID-BOTTOM-ONLY]**). Each is a **requirement** (MUST) or a
**recommendation** (SHOULD); see the provisions convention in [index.md](index.md#provisions) for the
naming and rendering scheme. Chapter provisions here use the `HYDRA-DM-…` namespace. Facts that *are*
mechanically derivable from the source (a signature, an arity, the members of a union) are stated
without a provision name.

## 1. Terms and types

Hydra's core model is two constructs defined in `hydra.core`, the **term** (`hydra.core.Term`) and the
**type** (`hydra.core.Type`).

- The `Term` union is **closed**: its variants are exactly those enumerated in §§4–10 (`variable`,
  `lambda`, `application`, `let`, `typeLambda`, `typeApplication`, `literal`, `list`, `set`, `map`,
  `optional`, `pair`, `either`, `unit`, `record`, `project`, `inject`, `cases`, `wrap`, `unwrap`,
  `annotated`), and no others.
- The `Type` union is **closed**: its variants are exactly those enumerated in §§5–10 (`literal`,
  `list`, `set`, `map`, `optional`, `pair`, `either`, `unit`, `void`, `record`, `union`, `wrap`,
  `function`, `forall`, `variable`, `application`, `effect`, `annotated`), and no others.
- A conforming implementation represents every variant of each union and no others; an encoding that
  cannot round-trip any one variant does not conform.

A term is not intrinsically paired with a type. The relationship is established by the type system in
a graph context in two directions: a term is **checked** against a type, and a type is **inferred** for
a term. Inference is total up to well-typedness: for every well-typed term in a graph, inference yields
a type scheme and elaborates the term into a fully-typed System-F term (§9); a term for which no type
can be inferred is ill-typed and non-conforming.

**[HYDRA-DM-TERM-UNIVERSAL] Every type has a term encoding, including `Type` and `Term` themselves.**
Consequently any Hydra artifact — term, type, schema, or graph — is representable as a term, hence
serializable by the term wire format ([json-format.md](json-format.md)) with no residue. An
implementation that cannot encode its own types and terms as terms does not conform.

## 2. Names

A **name** (`hydra.core.Name`) is a dot-separated qualified identifier. Its namespace is every segment
but the last; the final segment is the local name. A name resolves to at most one definition in a
graph. The lexical grammar of names is in [syntax.md](syntax.md); the packaging rules that constrain
them (uniqueness, the module-prefix rule, resolution) are in [validation.md](validation.md). A
`variable` term (§4) and a nominal term's type name (§8) are both names resolved against the graph;
an unresolved name is non-conforming.

## 3. Graphs: `hydra.core` in combination with `hydra.graph`

A **graph** (`hydra.graph.Graph`) is the environment in which terms are given meaning. It records:
`boundTerms` (the named term definitions — the elements), `boundTypes` / `schemaTypes` (named type
definitions and the schema), `primitives` (built-in functions, `hydra.graph.Primitive`),
`typeVariables` / `lambdaVariables` / `classConstraints` (the typing environment in scope), and
`metadata`. Every free `variable` in a bound term resolves to another `boundTerm`, a `lambdaVariable`,
or a `primitive`; a graph with a dangling reference is non-conforming.

**[HYDRA-DM-GRAPH-FROM-TERM] A term determines a graph.** Each binding is a node; a reference from one
binding's body to another binding's name is a link between nodes; a term's other constituents are the
node's remaining structure. The graph does **not** preserve the original `let` nesting: building it
flattens nested bindings into a single namespace, so two terms that differ only in how their bindings
are nested yield the same graph. What the graph does preserve is the reference structure and each
binding's term content; the term is recoverable only up to that flattening (equivalently, up to
lambda-lifting the bindings into one scope).

Hydra also maps a `Graph` to a **term graph** (nodes are subterms, sharing explicit, corresponding to
`letrec`) and a schema to a type graph (#716). These are the canonical graph views the property-graph
(`hydra-pg`) and RDF encodings map out of. The formal development of the model is the LambdaGraph paper
(#197); the alignment of inference and checking to it is #377.

## 4. The lambda calculus core

These term variants form the calculus:

- `variable` — a reference to a name bound by an enclosing `lambda`, an enclosing `let`, or a graph
  definition (§2). A `variable` not so bound is non-conforming.
- `lambda` — abstraction over exactly one parameter; its type is a `function` type (§9). The parameter
  is in scope in the body and nowhere else.
- `application` — application of a function term to one argument term. It is well-typed only if the
  function's type is a `function` type whose domain the argument's type satisfies.
- `let` — one or more **mutually recursive** bindings (each a name and a term) over a body. A binding
  may reference any binding of the same `let`, including itself. A `let` with zero bindings is
  non-conforming; two bindings with the same name are non-conforming.
- `typeLambda` / `typeApplication` — the System-F operators (§9): type abstraction binds a type
  variable over a term; type application supplies a type argument.

## 5. Literals

A `literal` term carries a value of one of the primitive types (`hydra.core.Literal` /
`hydra.core.LiteralType`): `boolean`, `string`, `binary`, and the numeric families. The numeric types
are exactly: the integer types (`int8`, `int16`, `int32`, `int64`, `uint8`, `uint16`, `uint32`,
`uint64`, `bigint`), the floating-point types (`float32`, `float64`, `bigfloat`), and arbitrary-scale
`decimal`. A literal's value MUST inhabit its stated literal type (e.g. an `int8` literal outside
[-128, 127] is non-conforming). Integer types other than `bigint` wrap in two's complement; `bigint`
and `decimal` are unbounded. **[HYDRA-DM-DECIMAL-SCALE-DISTINCT]** Two `decimal` values with equal numeric
value but different scale are distinct and unequal
([ordering-and-equality.md](ordering-and-equality.md)). The literal grammar and the IEEE-754
special-value encoding are in [syntax.md](syntax.md) / [json-format.md](json-format.md).

## 6. Collections

The homogeneous containers, each with a matching type variant:

- `list<t>` — an ordered, possibly-repeating sequence; every element has type `t`.
- `set<t>` — an unordered collection of **distinct** elements of type `t`. `t` MUST be comparable
  (not a `function` type); a `set` over a non-comparable element type is non-conforming
  ([validation.md](validation.md)).
- `map<k,v>` — a finite association of keys of type `k` to values of type `v`; keys are distinct. `k`
  MUST be comparable.
- `optional<t>` — zero or one element of type `t` (`none`, or one present value).

Distinctness of set elements and map keys is by structural equality.
**[HYDRA-DM-COLLECTION-CANONICAL-ORDER]** Sets and maps iterate in the canonical total order defined in
[ordering-and-equality.md](ordering-and-equality.md); an implementation that iterates in any other
order does not conform.

## 7. Products and sums

The anonymous (structural) products and sums — anonymous because, unlike §8, they carry no type name:

- `pair<t1,t2>` — the **binary product**: one term of type `t1` and one of type `t2`.
- `either<t1,t2>` — the **binary sum**: exactly one of a `t1` (left) or a `t2` (right), never both and
  never neither.
- `unit` — the **nullary product**: a single value with no components; its type has exactly one
  inhabitant.
- `void` — the **nullary sum**: the uninhabited bottom type. **[HYDRA-DM-VOID-UNINHABITED]** No term has
  type `void`; it has no term variant, since it cannot be constructed. **[HYDRA-DM-VOID-BOTTOM-ONLY]** `void`
  may appear only in bottom position — not inside a function codomain's constituents, nor inside a
  list/set/map/pair/either/optional element, nor as a record field or union variant type
  ([validation.md](validation.md)).

The record generalizes `pair` to n **named** fields; the union generalizes `either` to n **named**
variants (§8).

## 8. Nominal types

Records, unions, and wrappers are **nominal**: the term itself carries the **name** of the type it
introduces or eliminates, and is well-formed only against the structure the graph binds to that name.
**[HYDRA-DM-NOMINAL-NAME-REQUIRED]** A `record`, `inject`, `project`, `cases`, `wrap`, or `unwrap` term
carries its type name as a *required* constituent: the grammar does not permit omitting it, and it is
present even in untyped terms. This is not the same as the *optional type annotations* any term may
carry (§9): annotations are syntactically omittable, whereas a nominal type name is never omittable. A
coder or serialization that drops a nominal term's type name does not conform.

- **Records** — type `record` (a named product of fields, each a name and a type); terms `record` and
  `project`. **[HYDRA-DM-RECORD-FIELDS-EXACT]** A `record` term names its type and supplies a term for
  exactly the type's fields — a missing or extra field is non-conforming. A `project` names the type
  and a field that must be declared in it.
- **Unions** — type `union` (a named sum of variants, each a name and a type); terms `inject` and
  `cases`. An `inject` names the type and one declared variant. **[HYDRA-DM-CASES-EXHAUSTIVE]** A `cases`
  term names the type and provides handlers; it must cover every variant of the named union unless it
  carries a default. A handler or injection naming an undeclared variant is non-conforming.
- **Wrappers** — type `wrap` (a name over a single underlying type, a distinct nominal identity);
  terms `wrap` and `unwrap`, which name the wrapper and add/remove the layer.

**[HYDRA-DM-NOMINAL-KIND-MATCH]** A nominal term whose named type resolves to the wrong kind (e.g. a `record`
term over a name bound to a `union`) is non-conforming ([validation.md](validation.md)).

## 9. Functions and polymorphism

- `function` (type) — the type of every `lambda`: a domain type and a codomain type.
- `forall` (type) — universal quantification binding a type variable over a type; the sole source of
  polymorphism. A type `variable` occurs conformingly only within the scope of a `forall` (or graph
  type binding) that binds it.
- type-level `application` — application of a type to a type argument.
- `effect<t>` (type) — the type of an effectful computation producing a `t`; the effect model is
  specified in its own chapter.

Inference is Hindley-Milner over System F: it computes a type scheme — a `forall` over the free type
variables together with their class constraints ([classes.md](classes.md)) — and inserts the
`typeLambda` / `typeApplication` operators (§4) so the elaborated term carries its full System-F
structure. **[HYDRA-DM-INFERENCE-DETERMINISTIC]** Two inferences of the same term in the same graph yield the
same type scheme, up to renaming of bound type variables.

**[HYDRA-DM-ONE-GRAMMAR]** Hydra's untyped and typed lambda calculi share a single term grammar; the only
difference is the presence of type annotations, which are syntactically optional. (Nominal type names
are not annotations and are never optional — DM-NOMINAL-NAME-REQUIRED.) **[HYDRA-DM-TYPED-WHERE-EXPECTED]**
Some operations expect fully-typed, post-inference terms. Because the operational pipeline does not by
itself guarantee that a term is fully typed, this expectation is a conformance condition enforced by
validation ([validation.md](validation.md)) in the contexts that require it — not by the grammar,
which permits annotations to be absent.

## 10. Annotations

An `annotated` term or type (`hydra.core.AnnotatedTerm`) pairs a value with a metadata map from names
to terms. The falsifiable content:

- **[HYDRA-DM-ANNOTATION-TRANSPARENT]** Annotations are transparent to meaning: a value and its annotated
  form have the same type, the same reduction behavior, and compare equal under structural equality
  and the total order. An implementation for which an annotation changes any of these does not conform.
- **[HYDRA-DM-ANNOTATION-PRESERVED]** Annotations are preserved across the wire and across transformations
  that do not deliberately rewrite them: documentation and provenance survive code generation.
- **[HYDRA-DM-ANNOTATION-ROUNDTRIP]** Annotations round-trip through the textual syntax, including when
  stacked ([syntax.md](syntax.md)). Nested annotations are permitted; the kernel discourages an
  annotation directly wrapping another, but that is a SHOULD-class convention
  ([validation.md](validation.md) §4.2), not a well-formedness rule — an externally-authored nested
  annotation still conforms.

## 11. Relationship to the rest of the specification

- [Textual syntax](syntax.md) — how terms and types are written and printed.
- [Constraint classes](classes.md), [Ordering and equality](ordering-and-equality.md) — class-constraint
  and comparison semantics.
- [Validation](validation.md) — the conformance relation and the packaging rules (the well-formedness
  claims above are enforced there).
- [JSON format](json-format.md), [Serialization](serialization.md) — the wire representation.
- Generated [`hydra.core`](modules/hydra.core.md) / [`hydra.graph`](modules/hydra.graph.md) module
  pages — the exhaustive per-variant catalog (#723).
- Type inference and elaboration, reduction semantics — *(planned; consume #377's rules)*.
