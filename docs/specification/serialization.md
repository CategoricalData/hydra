<!-- NOTE: hand-authored spec chapter (not a generated module page). Drafted under #674 as the
     normative definition of serializability, the property that gates encode/decode generation,
     JSON interchange, and printable membership. The isSerializable code (#690) and the printable
     class (#693) implement against this chapter. -->

# Serialization

**Status: Draft** · Part of the [Hydra specification](index.md)

Serialization is the encoding of Hydra values as transportable data and their decoding back.
This chapter defines **serializability** — the property of a *type* that determines whether its
values can be encoded and decoded — and the encode/decode contract that follows from it.
Serializability is the shared foundation of three surfaces specified elsewhere: the
[JSON interchange format](json-format.md), the encode/decode functions generated for every
serializable type, and the membership of the [`printable`](classes.md) capability.

## Serializability is a property of types

Serializability is determined at the level of a **type**, not a particular value.
A type is serializable when its structure admits a total encoder and decoder; if it is, *every*
value of that type can be encoded, and any encoded form can be decoded back.

A type is **serializable** exactly when its transitive type structure contains none of the
following type variants:

- **function** (`t1 → t2`) — a function value is opaque; there is no structural form to encode.
- **effect** (`effect<t>`) — an effect describes a deferred host interaction; it is not a value
  that can be reduced to transportable data.
- **void** — the uninhabited type. A total encoder cannot be written over a `void`-typed position,
  because there is no value there to encode. (Because `void` is uninhabited, no *value* ever
  occupies such a position, so this exclusion never affects a real serialization; it is required
  for the predicate to be exactly the set of types over which a total encoder exists.)

"Transitively" means the check follows the type's dependencies: a record is serializable only if
every field type is serializable; a union only if every variant type is; a list, set, optional,
map, or pair only if its element (and key/value) types are; a wrapped or named type only if the
type it wraps or names is. A type built entirely from literals, records, unions, and the built-in
collection types over serializable components is serializable.

This is the property computed by `hydra.predicates.isSerializable`.

## The encode/decode contract

For every serializable type `T`, Hydra generates a pair of functions:

- `encode : T → Term` — encodes a value as a `hydra.core.Term`.
- `decode : Term → T` — decodes a `Term` back to a value, with the possibility of failure on a
  malformed or ill-typed `Term`.

These functions are **derived structurally** from the type: there is one encoding scheme, applied
uniformly. Two values of two different serializable types are encoded by the same structural rules;
serialization is not customized per type. (A *type* may separately supply a bespoke textual form
through the [`printable`](classes.md) capability, but that is a distinct, overlaid mechanism; the
structural encode/decode defined here is uniform.)

**Round-trip.** Decoding the result of encoding a value yields the original value:
`decode (encode x) = x` for every value `x` of a serializable type. This round-trip is a property
of the generated pair and is verified by the conformance test suite; it is not separately enforced
by the type system. (See the design rationale for why encode and decode are authored together as a
bidirectional [`Coder`](https://github.com/CategoricalData/hydra/wiki/Design#coders-bundle-encode-and-decode)
rather than as two independent functions.)

## Relationship to the JSON interchange format

The [JSON format](json-format.md) is the concrete interchange encoding built on this foundation:
a serializable type is exactly a type for which JSON encode/decode is generated, and the JSON coder
encodes a value's `Term` form. Where a Hydra surface form has a JSON counterpart — numbers, strings,
binary payloads — Hydra **inherits the JSON syntax** and the corresponding kernel primitives *are*
the JSON serializers (see [Design principles](index.md#4-design-principles) and the
[`print`/`parse` primitives](primitives/literals.md)). Serializability is the type-level gate; the
JSON format is one rendering of the encoded structure.

## Relationship to `printable`

A type's *serializability* is exactly its membership in the [`printable`](classes.md) capability:
because a serializable type has generated `encode`/`decode`, it also has a **derived** textual
`print`/`parse` — the structural default — with per-type bespoke codecs overlaid where a type
supplies one. Consequently the exclusions above carry over: **function types are not printable**
(they are not serializable), while `Term` — an ordinary serializable data type — *is* printable.
`\x. add 1 x` is printable because its type is `Term`, not because its type is a function type.

## Conformance

An implementation generates encode/decode for exactly the serializable types (as defined above) and
no others, and its generated pairs satisfy the round-trip property under the hydra-kernel test suite.
Both conformance modes ([kernel and generation](index.md#3-conformance)) apply: the interpreter
evaluates the encode/decode terms directly; a compiled implementation runs the natively generated
encoders and decoders. The serializable-type set is determined by the type structure alone and is
therefore identical on every conforming host.
