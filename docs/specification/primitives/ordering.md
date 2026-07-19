<!-- NOTE: this page will be automatically generated from the primitive definitions in
     packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Lib/*.hs (generator not yet
     built). Hand-authored draft under #417; the specifications here are the normative TARGET
     and may deliberately diverge from the currently-frozen definitions.
     Conventions (notation, laziness, badges, floating point) are defined in index.md. -->

# hydra.lib.ordering

Ordering of values.
Every type admitted by the `ordering` constraint has a total order, defined per type constructor
in [ordering-and-equality.md](../ordering-and-equality.md).
<!-- [PENDING]: ordering-and-equality.md defines the total order over Hydra values (integers
     numerically; strings lexicographically by code point; lists element-wise; records by
     field in declaration order; union variants by declaration order; floats per the extended
     total order; ...) together with structural equality. Part of the host-independence batch:
     it is the normative anchor the per-host comparator fixes implement against. -->
For floating-point values the order is the extended total order of
[Floating-point values](index.md#floating-point-values), under which `compare x y` is
`equalTo` exactly when `hydra.lib.equality.equal x y` is `true`.

#### compare — **Draft**

`∀t. (ordering t) ⇒ t → t → Comparison`

Usage: `compare x y`

Compare two values of an ordered type.
Returns `lessThan` if `x` precedes `y`, `greaterThan` if `y` precedes `x`, and `equalTo`
otherwise (the values of [Comparison](../types/util.md#comparison)), under the total order
defined for the type `t` in [ordering-and-equality.md](../ordering-and-equality.md).
`compare x y` is `equalTo` exactly when `hydra.lib.equality.equal x y` is `true`.
Defined for all pairs of values of any type admitted by the `ordering` constraint,
including all floating-point values.

Since: 0.18 (moved from `hydra.lib.equality.compare`)

#### gt — **Draft**

`∀t. (ordering t) ⇒ t → t → boolean`

Usage: `gt x y`

Check whether `x` strictly follows `y` in the type's total order:
`gt x y` is `true` exactly when `compare x y` is `greaterThan`.

Since: 0.18 (moved from `hydra.lib.equality.gt`)

#### gte — **Draft**

`∀t. (ordering t) ⇒ t → t → boolean`

Usage: `gte x y`

Check whether `x` follows or equals `y` in the type's total order:
`gte x y` is `true` exactly when `compare x y` is not `lessThan`.

Since: 0.18 (moved from `hydra.lib.equality.gte`)

#### lt — **Draft**

`∀t. (ordering t) ⇒ t → t → boolean`

Usage: `lt x y`

Check whether `x` strictly precedes `y` in the type's total order:
`lt x y` is `true` exactly when `compare x y` is `lessThan`.

Since: 0.18 (moved from `hydra.lib.equality.lt`)

#### lte — **Draft**

`∀t. (ordering t) ⇒ t → t → boolean`

Usage: `lte x y`

Check whether `x` precedes or equals `y` in the type's total order:
`lte x y` is `true` exactly when `compare x y` is not `greaterThan`.

Since: 0.18 (moved from `hydra.lib.equality.lte`)

#### max — **Draft**

`∀t. (ordering t) ⇒ t → t → t`

Usage: `max x y`

Return the greater of two values in the type's total order; returns `y` when
`compare x y` is `equalTo`.

Since: 0.18 (moved from `hydra.lib.equality.max`)

#### min — **Draft**

`∀t. (ordering t) ⇒ t → t → t`

Usage: `min x y`

Return the lesser of two values in the type's total order; returns `y` when
`compare x y` is `equalTo`.

Since: 0.18 (moved from `hydra.lib.equality.min`)
