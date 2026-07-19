<!-- NOTE: this page will be automatically generated from the primitive definitions in
     packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Lib/*.hs (generator not yet
     built). Hand-authored draft under #417; the specifications here are the normative TARGET
     and may deliberately diverge from the currently-frozen definitions.
     Conventions (notation, laziness, badges, floating point) are defined in index.md. -->

# hydra.lib.equality

Equality of values.
Equality is structural: two values are equal exactly when they are the same value of the same
type, as defined in [ordering-and-equality.md](../ordering-and-equality.md).
<!-- [PENDING]: ordering-and-equality.md is the supplementary spec defining structural
     equality AND the total order over Hydra values, per type constructor;
     part of the host-independence batch. -->
For floating-point values equality is value identity — see
[Floating-point values](index.md#floating-point-values).

#### equal — **Draft**

`∀t. (equality t) ⇒ t → t → boolean`

Usage: `equal x y`

Check whether two values are equal.
Returns `true` if `x` and `y` are the same value, and `false` otherwise, per the structural
definition of value equality in [ordering-and-equality.md](../ordering-and-equality.md).
Equality is reflexive, symmetric, and transitive at every type, including floating-point types:
`equal NaN NaN` is `true` and `equal -0.0 0.0` is `false`.

Since: 0.15

#### notEqual — **Draft**

`∀t. (equality t) ⇒ t → t → boolean`

Usage: `notEqual x y`

Check whether two values are unequal.
`notEqual x y` is `hydra.lib.logic.not (equal x y)`; this defining equation is the
specification, and the default implementation.

Since: 0.18

#### identity — **Deprecated**

`∀t. t → t`

Deprecated since: 0.18. Use: `hydra.lib.functions.identity`.

#### compare — **Deprecated**

`∀t. (ordering t) ⇒ t → t → Comparison`

Deprecated since: 0.18. Use: `hydra.lib.ordering.compare`.

#### gt, gte, lt, lte — **Deprecated**

`∀t. (ordering t) ⇒ t → t → boolean`

Deprecated since: 0.18. Use: `hydra.lib.ordering.gt`, `gte`, `lt`, `lte`.

#### max, min — **Deprecated**

`∀t. (ordering t) ⇒ t → t → t`

Deprecated since: 0.18. Use: `hydra.lib.ordering.max`, `min`.
