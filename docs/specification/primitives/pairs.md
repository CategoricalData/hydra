<!-- NOTE: this page will be automatically generated from the primitive definitions in
     packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Lib/Pairs.hs (generator not yet
     built). Hand-authored draft under #417; the specifications here are the normative TARGET
     and may deliberately diverge from the currently-frozen definitions.
     Conventions (notation, laziness, badges, floating point) are defined in index.md. -->

# hydra.lib.pairs

Pairs: the two-component product type.
This module provides the two projections and the bifunctor map.

#### bimap — **Draft**

`∀t1,t2,t3,t4. (t1 → t3) → (t2 → t4) → pair<t1, t2> → pair<t3, t4>`

Usage: `bimap f g p`

Map functions over both components of a pair.
`bimap f g p` is the pair whose first component is `f (first p)` and whose second component is
`g (second p)`; this defining equation is the specification, and the default implementation.
This is the bifunctor map for pairs.

Since: 0.15

#### first — **Draft**

`∀t1,t2. pair<t1, t2> → t1`

Usage: `first p`

Get the first element of a pair.
Returns the first component of `p`.

Since: 0.15

#### second — **Draft**

`∀t1,t2. pair<t1, t2> → t2`

Usage: `second p`

Get the second element of a pair.
Returns the second component of `p`.

Since: 0.15
