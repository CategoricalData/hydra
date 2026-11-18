<!-- NOTE: this page is hand-written. The other module pages in this directory are generated from the
     primitive definitions in packages/hydra-kernel/src/main/hydra/hydra/core/lib/*.hydra.
     Conventions (notation, laziness, badges, floating point) are defined in index.md. -->

# hydra.core.lib.equality

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
`notEqual x y` is `hydra.core.lib.logic.not (equal x y)`; this defining equation is the
specification, and the default implementation.

Since: 0.17.6

#### Moved primitives

`compare`, `gt`, `gte`, `lt`, `lte`, `max`, and `min` moved to [`hydra.core.lib.ordering`](ordering.md) in 0.17.6;
`identity` moved to `hydra.core.lib.functions`.
