<!-- Note: this is an automatically generated file. Do not edit. -->

# hydra.core.lib.pairs

Pairs: the two-component product type. This module provides the two projections, the bifunctor map, and the constructor.

## bimap

Map over both elements of a pair.

`∀a, b, c, d.bimap(f: a → c, g: b → d, p: (a, b)) → (c, d)`

**Parameters**

- `f` (`a → c`): the function applied to the first element
- `g` (`b → d`): the function applied to the second element
- `p` (`(a, b)`): the pair to map over

**Returns** `(c, d)`

*Since: 0.15.*

bimap(f, g, p) returns a new pair (f(first(p)), g(second(p))). The bifunctor map for pairs.

Total. Corresponds to Haskell's Data.Bifunctor.bimap :: (a -> c) -> (b -> d) -> (a, b) -> (c, d).

**[HYDRA-CORE-LIB-PAIRS-BIMAP-MAPS-BOTH-ELEMENTS-INDEPENDENTLY] (requirement)** bimap applies the first function to the pair's first element and the second function to the pair's second element, producing a new pair of the two results, for any element and function types.

## first

Get the first element of a pair.

`∀a, b.first(p: (a, b)) → a`

**Parameters**

- `p` (`(a, b)`): the pair whose first element is returned

**Returns** `a`

*Since: 0.15.*

first(p) returns the first component of the pair p.

Total. Corresponds to Haskell's fst :: (a, b) -> a.

**[HYDRA-CORE-LIB-PAIRS-FIRST-RETURNS-FIRST-COMPONENT] (requirement)** first returns the first component of the pair unchanged, for any element type and value.

## pair

Construct a pair from two values.

`∀a, b.pair(x: a, y: b) → (a, b)`

**Parameters**

- `x` (`a`): the first element
- `y` (`b`): the second element

**Returns** `(a, b)`

*Since: 0.17.6.*

pair(x, y) returns the pair (x, y).

The type-free pair constructor, useful point-free (e.g. zipWith(pair, xs, ys)).

Total. Corresponds to Haskell's (,) :: a -> b -> (a, b).

**[HYDRA-CORE-LIB-PAIRS-PAIR-CONSTRUCTS-FROM-TWO-VALUES] (requirement)** pair x y constructs the pair (x, y), preserving both components exactly as given, for any value.

## second

Get the second element of a pair.

`∀a, b.second(p: (a, b)) → b`

**Parameters**

- `p` (`(a, b)`): the pair whose second element is returned

**Returns** `b`

*Since: 0.15.*

second(p) returns the second component of the pair p.

Total. Corresponds to Haskell's snd :: (a, b) -> b.

**[HYDRA-CORE-LIB-PAIRS-SECOND-RETURNS-SECOND-COMPONENT] (requirement)** second returns the second component of the pair unchanged, for any element type and value.
