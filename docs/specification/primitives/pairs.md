<!-- Note: this is an automatically generated file. Do not edit. -->

# hydra.core.lib.pairs

<!-- Note: this is an automatically generated file. Do not edit. -->

Pairs: the two-component product type. This module provides the two projections, the bifunctor map, and the constructor.

## hydra.core.lib.pairs.bimap

Map over both elements of a pair.

`∀a,b,c,d.(a → c) → (b → d) → (a, b) → (c, d)`

*Since: 0.15.*

bimap(f, g, p) returns a new pair (f(first(p)), g(second(p))). The bifunctor map for pairs.

Total. Corresponds to Haskell's Data.Bifunctor.bimap :: (a -> c) -> (b -> d) -> (a, b) -> (c, d).

**[HYDRA-CORE-LIB-PAIRS-BIMAP-MAPS-BOTH-ELEMENTS-INDEPENDENTLY] (requirement) **bimap applies the first function to the pair's first element and the second function to the pair's second element, producing a new pair of the two results, for any element and function types.

## hydra.core.lib.pairs.first

Get the first element of a pair.

`∀a,b.(a, b) → a`

*Since: 0.15.*

first(p) returns the first component of the pair p.

Total. Corresponds to Haskell's fst :: (a, b) -> a.

**[HYDRA-CORE-LIB-PAIRS-FIRST-RETURNS-FIRST-COMPONENT] (requirement) **first returns the first component of the pair unchanged, for any element type and value.

## hydra.core.lib.pairs.pair

Construct a pair from two values.

`∀a,b.a → b → (a, b)`

*Since: 0.17.6.*

pair(x, y) returns the pair (x, y).

The type-free pair constructor, useful point-free (e.g. zipWith(pair, xs, ys)).

Total. Corresponds to Haskell's (,) :: a -> b -> (a, b).

**[HYDRA-CORE-LIB-PAIRS-PAIR-CONSTRUCTS-FROM-TWO-VALUES] (requirement) **pair x y constructs the pair (x, y), preserving both components exactly as given, for any value.

## hydra.core.lib.pairs.second

Get the second element of a pair.

`∀a,b.(a, b) → b`

*Since: 0.15.*

second(p) returns the second component of the pair p.

Total. Corresponds to Haskell's snd :: (a, b) -> b.

**[HYDRA-CORE-LIB-PAIRS-SECOND-RETURNS-SECOND-COMPONENT] (requirement) **second returns the second component of the pair unchanged, for any element type and value.
