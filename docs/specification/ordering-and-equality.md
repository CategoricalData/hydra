<!-- NOTE: hand-authored spec chapter (not a generated module page). DRAFT under #417, for
     review: this is the normative anchor for the host-independence batch (per-host
     comparator fixes and the special-value conformance tests implement against it). -->

# Ordering and equality

Every Hydra value can be tested for equality and compared under a total order.
This page defines both: it is the value-level semantics behind the `equality` and `ordering`
constraint classes ([classes.md](classes.md)) and the primitives of `hydra.lib.equality` and
`hydra.lib.ordering`, and it is the order in which Hydra's maps and sets store and iterate
their keys and elements.
Named **provisions** (`HYDRA-ORD-…`, in bold at the head of a claim) follow the provisions convention
in [index.md](index.md#provisions).

## Structural comparison

**[HYDRA-ORD-EQUALITY-STRUCTURAL]** Equality is structural: two values are equal exactly when they are
built from the same constructors applied to equal components, and the order is the corresponding
structural order.
Concretely, when the type of the values being compared is a record type, comparison proceeds
field by field, in declaration order.
When it is a union type, injections compare by variant first, and two injections into the
same variant compare by payload.
Which cross-variant order applies depends on whether the comparison has access to the union's
declared field order (see [Injections and variant order](#injections-and-variant-order) below):
a schema-aware comparison uses declared order (a variant declared earlier comparing less than
one declared later), while a comparison of bare `hydra.core.Term` injections, which carry only
the variant name, uses variant-name (lexicographic) order.
A value of a wrapper type compares as its wrapped value.
These rules cover every type defined as a record, union, or wrapper — whether in user schemas
or in `hydra.core` itself — so no construct needs its own comparison rule: a lambda, an
application, or any other term compares structurally like any other value of the union type
`hydra.core.Term`.
Structural equality is not semantic equivalence: values that are equivalent in some other
sense are not necessarily equal.
For example, given the binding `a := "foo"`, the terms `a` and `"foo"` *reduce* to the same
term, but they are unequal — one is a variable and the other a literal.
Equality compares values as constructed, not their normal forms; to compare by result,
reduce first.
The only types requiring individual definitions are the built-in ones — the literal types and
the built-in type constructors (lists, maps, sets, optionals, pairs, eithers, unit) — which
are not defined as record or union types in `hydra.core`; they are specified below.

Three global principles:

- Comparison agrees with equality: `compare x y` returns the `Comparison` value `equalTo` —
  `Comparison` is the enum `lessThan` / `equalTo` / `greaterThan`, not a primitive — exactly
  when `equal x y` is `true`, for every type.
  **[HYDRA-ORD-TOTAL-ORDER]** For each type, `compare` is a total order — total, transitive, and
  antisymmetric — and `equal` is the induced equivalence.
- Comparisons are defined only between values of the same type; a Hydra program is typed, so
  cross-type comparison does not arise.
- Annotations are transparent: equality never consults metadata, and two values differing
  only in annotations are equal.

Because maps and sets iterate in ascending order, the order defined here is observable in
every map- and set-valued result, and is part of the collection type contract.

## Injections and variant order

Cross-variant comparison of union values has two regimes, because the declared order of a
union's variants lives in the union's `Type`, not in an injected value.

An injection is represented at the term level as `hydra.core.Injection { typeName, field }`,
where `field` carries only the injected variant's **name** — never an ordinal or a reference
back to the union `Type` where the declared field order lives.
Hydra's `compare` (the `ordering` class; `hydra.lib.ordering.compare`) has the pure signature
`x -> x -> Comparison`: it is given the two values and nothing else, with no `Type`, schema, or
`Graph` context.
Consequently a comparison of two bare `hydra.core.Term` values that are injections **cannot**
recover declared variant order at runtime; the only variant key available is the name.

The rule is therefore:

- **Bare `hydra.core.Term` injections compare by variant name, lexicographically.**
  Given `Term.inject` values into different variants, the one whose variant name is
  lexicographically smaller compares less; same-variant injections compare by payload.
  This is deliberate: it keeps `compare` a pure, total, schema-free operation with no lookup
  cost, and it is the order Hydra's maps and sets of `Term` values actually iterate in.
  It is a consequence of representation, not a semantic claim that name order is "the" order of
  the union — for a union declaring variants `["zebra", "apple"]`, bare-`Term` injections
  compare `apple < zebra`, the opposite of declared order.
- **Schema-aware comparison uses declared order.** Where a comparison is performed with the
  union's `Type` in hand (a typed context that knows the variant list), declared order applies:
  a variant declared earlier compares less than one declared later.
  Hydra does not currently expose a schema-aware `compare` primitive — `hydra.lib.ordering.compare`
  is the bare-`Term` regime above — so declared-order comparison is a property of typed tooling
  that has the `Type`, not of the runtime `ordering` class.

This distinction applies only to `hydra.core.Term` injections (arbitrary user-schema union
values represented uniformly as `Term`).
`hydra.core.Term` and `hydra.core.Literal` themselves — and any other union compared *as a
value of its own generated type* on a host — have real per-variant representations (distinct
constructors in the host language), so their cross-variant order is the declared order of their
variants, fixed at code-generation time, exactly as the record/union/wrapper rules above state.

Every host must implement the bare-`Term` regime identically: `Term.inject` cross-variant
comparison is by variant name (lexicographic) on every host, with no host substituting declared
order, print-order, or a native enum ordinal.

## Literal types

**Booleans.** `false` is less than `true`.

**Integers.** Each of the nine integer types (int8, int16, int32, int64, uint8, uint16,
uint32, uint64, bigint) compares by numeric value.
Distinct integer types are distinct types; there are no cross-width comparisons.

**Floating-point types.** The float32 and float64 value spaces are the wire format's: IEEE
754 binary32/binary64 values with a single NaN (payloads are not distinguished) and with
negative zero distinct from positive zero.
Equality is value identity: `equal NaN NaN` is `true`, and `equal -0.0 0.0` is `false`.
The order is the extended total order:

> −∞ < negative finite values < −0.0 < +0.0 < positive finite values < +∞ < NaN

Finite values compare numerically; NaN compares greater than every other value and equal to
itself.
This is IEEE 754 §5.10 `totalOrder`, restricted to Hydra's single-NaN value space.
IEEE 754's own comparison predicates — under which NaN is unequal to itself and unordered —
are deliberately not provided; see
[Floating-point values](primitives/index.md#floating-point-values) for the consequences,
including the inverted `isNaN`-idiom warning.

**Decimals.** A decimal value is an integer coefficient with a scale — the count of digits
after the decimal point — and two decimals are equal exactly when both agree: `1.10` (scale
2) and `1.1` (scale 1) denote the same number but are distinct, unequal values
(the scale-distinctness provision [HYDRA-DM-DECIMAL-SCALE-DISTINCT](data-model.md)).
The order is by numeric value first; numerically equal decimals of different scale are
ordered by scale, smaller scale first, so `1.1` < `1.10` < `1.100`.
There are no non-finite or signed-zero decimal values.

**Strings.** Strings compare lexicographically by Unicode code point.
A proper prefix is less than any string it prefixes.
No collation, normalization, or case folding is applied: `"a"` < `"b"` < `"á"` because
U+0061 < U+0062 < U+00E1.

**Binary values.** Binary values compare lexicographically by byte, with a proper prefix
less than any value it prefixes.

## Built-in type constructors

**Unit.** The unit type has a single value, equal to itself.

**Optionals.** `none` is less than every `given x`; `given x` and `given y` compare as `x`
and `y`.

**Eithers.** Every `left x` is less than every `right y`; two values with the same side
compare by payload.

**Pairs.** Pairs compare lexicographically: by first component, then by second.

**Lists.** Lists compare lexicographically element by element; a proper prefix is less than
any list it prefixes.
The empty list is the least list.

**Sets.** A set is identified with the ascending sequence of its elements, and two sets
compare as those sequences (lexicographically, prefix least).
Consequently two sets are equal exactly when they contain equal elements.

**Maps.** A map is identified with its ascending-key sequence of bindings, and two maps
compare as those sequences, each binding comparing by key first, then by value.
Consequently two maps are equal exactly when they have equal keys bound to equal values.

## Consequences for collections

Any value with an `ordering` instance can serve as a map key or set element, including
floating-point special values: NaN is an ordinary key under the extended total order, and
`-0.0` and `0.0` are distinct keys.
Sets never contain two equal elements; inserting an equal element replaces nothing and adds
nothing.
Map insertion with an equal key replaces the binding.
Iteration order is ascending under the order defined here, on every host.

## Conformance

Host implementations must route every equality and ordering operation through the semantics
defined here; in particular, no implementation may lower `equal` or `compare` to a host
language's native comparison operator on floating-point values, whose IEEE semantics differ
at NaN and signed zero.
The cross-host conformance suite exercises equality, comparison, sorting, and map/set keying
through the special values (NaN, ±0.0, ±∞) on every host.
It also exercises cross-variant `hydra.core.Term` injection comparison
([Injections and variant order](#injections-and-variant-order)): every host must order
`Term.inject` values by variant name (lexicographic), so a union whose variants are declared
out of alphabetical order (e.g. `["zebra", "apple"]`) compares its injections `apple < zebra`
identically on every host.
