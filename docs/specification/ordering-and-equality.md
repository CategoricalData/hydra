<!-- NOTE: hand-authored spec chapter (not a generated module page). DRAFT under #417, for
     review: this is the normative anchor for the host-independence batch (per-host
     comparator fixes and the special-value conformance tests implement against it). -->

# Ordering and equality

Every value of a comparable type can be tested for equality and compared under a total order.
Which types are comparable is defined in [classes.md](classes.md#comparable-types): roughly, every type with
no function, effect, or quantified type inside it.
This page defines both: it is the value-level semantics behind the `equality` and `ordering`
constraint classes ([classes.md](classes.md)) and the primitives of `hydra.core.lib.equality` and
`hydra.core.lib.ordering`, and it is the order in which Hydra's maps and sets store and iterate
their keys and elements.
Named **provisions** (`HYDRA-ORD-…`, in bold at the head of a claim) follow the provisions convention
in [index.md](index.md#provisions).

## Structural comparison

**[HYDRA-ORD-EQUALITY-STRUCTURAL]** Equality is structural: two values are equal exactly when they are
built from the same constructors applied to equal components, and the order is the corresponding
structural order.
Concretely, when the type of the values being compared is a record type, comparison proceeds
field by field, in declaration order.
**[HYDRA-ORD-VARIANT-DECLARED-ORDER]** When it is a union type, injections compare by variant first, in the
order the union declares its variants (a variant declared earlier compares less than one declared later), and two
injections into the same variant compare by payload.
A value of a wrapper type compares as its wrapped value.
These rules cover every type defined as a record, union, or wrapper — whether in user schemas
or in `hydra.core.model` itself — so no construct needs its own comparison rule: a lambda, an
application, or any other term compares structurally like any other value of the union type
`hydra.core.model.Term`.
This is comparison of *terms*, one level up from the values they denote: a lambda term is comparable,
but a function value is not, and a function type is not comparable
([classes.md](classes.md#comparable-types)).
Structural equality is not semantic equivalence: values that are equivalent in some other
sense are not necessarily equal.
For example, given the binding `a := "foo"`, the terms `a` and `"foo"` *reduce* to the same
term, but they are unequal — one is a variable and the other a literal.
Equality compares values as constructed, not their normal forms; to compare by result,
reduce first.
The only types requiring individual definitions are the built-in ones — the literal types and
the built-in type constructors (lists, maps, sets, optionals, pairs, eithers, unit) — which
are not defined as record or union types in `hydra.core.model`; they are specified below.

Three global principles:

- Comparison agrees with equality: `compare x y` returns the `Comparison` value `equalTo` —
  `Comparison` is the enum `lessThan` / `equalTo` / `greaterThan`, not a primitive — exactly
  when `equal x y` is `true`, for every type.
  **[HYDRA-ORD-TOTAL-ORDER]** For each type, `compare` is a total order — total, transitive, and
  antisymmetric — and `equal` is the induced equivalence.
- Comparisons are defined only between values of the same comparable type; a Hydra program is typed, so
  cross-type comparison and comparison of functions or effects do not arise.
- Annotations are transparent: equality never consults metadata, and two values differing
  only in annotations are equal.

Because maps and sets iterate in ascending order, the order defined here is observable in
every map- and set-valued result, and is part of the collection type contract.

## Values and terms

The rules above apply to values at every level, and the levels must not be confused.
An *object-level* value of a union type compares in declared variant order, however it is represented:

- A host type generated from the union has one constructor per variant, in declared order, so the host's own
  comparison already follows declared order.
- An interpreter holds the same value as a `hydra.core.model.Term` injection, which carries only the variant's
  name. The interpreter's equality and ordering primitives, and its maps, sets, and sorts, therefore compare with
  the graph in hand: they look the union up by its type name and use the variant's declared position.
  A union name that does not resolve is an error, not a fallback to another order.

A `hydra.core.model.Term` value is also a value in its own right: it is a value of the union type
`hydra.core.model.Term`, and its injection constituent is the record `hydra.core.model.Injection`, whose `field` is
the record `hydra.core.model.Field` with a `name`.
Code that manipulates terms as data compares them by these record and union rules, so two injection *terms* into
different variants compare by their variant names.
This is the same structural rule applied one level up, not a second order for the same values.

**[HYDRA-ORD-ANNOTATIONS-DEEP]** Annotations are ignored at every depth of an object-level value, not only at its
top level.
Consequently a map key or set element is stored with its annotations removed: two keys that differ only in
annotations are the same key, and neither annotation survives in the collection.

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

Any value of a comparable type can serve as a map key or set element, including
floating-point special values: NaN is an ordinary key under the extended total order, and
`-0.0` and `0.0` are distinct keys.
Sets never contain two equal elements; inserting an equal element replaces nothing and adds
nothing.
Map insertion with an equal key replaces the binding; the stored key carries no annotations
([Values and terms](#values-and-terms)).
Iteration order is ascending under the order defined here, on every host.

## Conformance

Host implementations must route every equality and ordering operation through the semantics
defined here; in particular, no implementation may lower `equal` or `compare` to a host
language's native comparison operator on floating-point values, whose IEEE semantics differ
at NaN and signed zero.
The cross-host conformance suite exercises equality, comparison, sorting, and map/set keying
through the special values (NaN, ±0.0, ±∞) on every host.
It also exercises object-level union values whose variants are declared out of name order: every host's
interpreter must compare, sort, and key them in declared order, and must ignore annotations nested inside compared
values ([Values and terms](#values-and-terms)).
