<!-- Note: this is an automatically generated file. Do not edit. -->

# hydra.core.lib.eithers

<!-- Note: this is an automatically generated file. Do not edit. -->

The either type is Hydra's error/alternative type: a value of either<t1, t2> is either left(x), conventionally carrying an error or alternative of type t1, or right(y), carrying a success value of type t2.

The either type is a right-biased monad: right (the unit), map, apply, and bind operate on the right side, and the left type is held fixed through a computation, so a chain of steps shares a common error type.

This module provides the full monad row -- right, map, apply, bind, and compose -- together with the dual constructor left, elimination (cases), predicates, and traversal.

The fundamental eliminator is cases; every other primitive in this module can be derived from it.

## hydra.core.lib.eithers.apply

Applicative apply for either: combine a function under either and an argument under either.

`∀x,y,z.either<x, y → z> → either<x, y> → either<x, z>`

*Since: 0.17.6.*

apply(ef, ex) returns right(f(x)) when ef is right(f) and ex is right(x).

If either argument is a left, the result is that left; when both arguments are left, the first (function-side) left is returned — first error wins, and ex is not consulted once ef is known to be a left.

apply(ef, ex) is bind(ef, \f -> map(f, ex)); this defining equation is the specification, and the default implementation.

Total. Corresponds to Haskell's (<*>) :: Either a (b -> c) -> Either a b -> Either a c.

## hydra.core.lib.eithers.bimap

Map over both sides of an either value.

`∀x,y,z,w.(x → z) → (y → w) → either<x, y> → either<z, w>`

*Since: 0.15.*

bimap(f, g, e) applies f to the contained value if e is a Left, or g if e is a Right; the result retains the same Left/Right variant.

Total. Corresponds to Haskell's Data.Bifunctor.bimap :: (a -> c) -> (b -> d) -> Either a b -> Either c d.

**[HYDRA-CORE-LIB-EITHERS-BIMAP-MAPS-BOTH-SIDES-INDEPENDENTLY] (requirement) **bimap applies the first function if the either is a left and the second function if it is a right, preserving the left/right variant of the result.

## hydra.core.lib.eithers.bind

Bind (flatMap) for either: if Right, apply the function; if Left, return unchanged.

`∀x,y,z.either<x, y> → (y → either<x, z>) → either<x, z>`

*Since: 0.15.*

bind(e, f) is the monadic bind for either with a fixed Left type: if e is Right v, the result is f(v); if e is Left x, the result is Left x with the Left type preserved.

Used to chain computations that may fail with a common error type.

Total. Corresponds to Haskell's (>>=) :: Either a b -> (b -> Either a c) -> Either a c.

**[HYDRA-CORE-LIB-EITHERS-BIND-CHAINS-ON-RIGHT-SHORT-CIRCUITS-ON-LEFT] (requirement) **bind applies the continuation function when the either is a right, propagating the continuation's own result (which may itself be a left or right); when the either is a left, bind returns that left unchanged without invoking the continuation.

## hydra.core.lib.eithers.compose

Kleisli composition for either.

`∀x,y,z,w.(x → either<w, y>) → (y → either<w, z>) → x → either<w, z>`

*Since: 0.17.6.*

compose(f, g, x) is bind(f(x), g); this defining equation is the specification, and the default implementation.

If either f(x) or the second stage produces a left, the result is that left.

Total. Corresponds to Kleisli composition (>=>) specialised to Either with a fixed Left type.

## hydra.core.lib.eithers.either

Eliminate an either value by applying one of two functions.

`∀x,y,z.(x → z) → (y → z) → either<x, y> → z`

*Deprecated since: 0.18.*

either(f, g, e) returns f(x) if e is Left x and g(y) if e is Right y.

The fundamental eliminator for the either type; every other primitive in this namespace can be derived from it.

Total. Corresponds to Haskell's either :: (a -> c) -> (b -> c) -> Either a b -> c.

Deprecated since 0.18. Use: cases.

Note that the argument order also changed in the replacement: cases takes the scrutinee first.

**[HYDRA-CORE-LIB-EITHERS-EITHER-ELIMINATES-BY-VARIANT] (requirement) **either applies the first function if given a left value and the second function if given a right value, returning that function's result directly.

## hydra.core.lib.eithers.foldList

Left-fold over a list with an Either-returning function, short-circuiting on Left.

`∀x,y,z.(x → y → either<z, x>) → x → list<y> → either<z, x>`

*Since: 0.17.6.*

foldList(f, acc0, xs) folds f over xs from the left, threading an accumulator of type a, where each application may fail with Left e: it iterates while f returns Right, propagates Left on the first failure, and returns Right (final accumulator) if all elements were processed.

Corresponds to a short-circuiting variant of Haskell's foldM specialised to Either.

Renamed from hydra.lib.eithers.foldl, to distinguish the monadic fold from the plain list fold hydra.lib.lists.foldl.

**[HYDRA-CORE-LIB-EITHERS-FOLD-LIST-FOLDS-LEFT-SHORT-CIRCUITING-ON-LEFT] (requirement) **foldList folds the given function over the list from the left, threading an accumulator through successive right results; the first left result returned by the step function short-circuits the whole fold to that left, and folding the empty list returns right of the initial accumulator unchanged.

## hydra.core.lib.eithers.isLeft

Check whether an either is a Left value.

`∀x,y.either<x, y> → boolean`

*Since: 0.15.*

True if the argument is a Left variant, false if a Right.

Total. Corresponds to Haskell's Data.Either.isLeft :: Either a b -> Bool.

**[HYDRA-CORE-LIB-EITHERS-IS-LEFT-TESTS-LEFT-VARIANT] (requirement) **isLeft returns true exactly when the either is a left value, and false for a right value.

## hydra.core.lib.eithers.isRight

Check whether an either is a Right value.

`∀x,y.either<x, y> → boolean`

*Since: 0.15.*

True if the argument is a Right variant, false if a Left.

Total. Corresponds to Haskell's Data.Either.isRight :: Either a b -> Bool.

**[HYDRA-CORE-LIB-EITHERS-IS-RIGHT-TESTS-RIGHT-VARIANT] (requirement) **isRight returns true exactly when the either is a right value, and false for a left value.

## hydra.core.lib.eithers.left

Construct a left (error/exceptional) either value.

`∀x,y.x → either<x, y>`

*Since: 0.17.6.*

left(x) is Left x. The dual constructor to right, injecting into the Left (error/exceptional) side of an either.

Total. Corresponds to Haskell's Left :: a -> Either a b.

**[HYDRA-CORE-LIB-EITHERS-LEFT-CONSTRUCTS-LEFT-VARIANT] (requirement) **left wraps a value as the left (error/exceptional) variant of an either.

## hydra.core.lib.eithers.lefts

Extract all Left values from a list of either values.

`∀x,y.list<either<x, y>> → list<x>`

*Since: 0.15.*

lefts(xs) returns a list containing every Left value in xs, in original order, with Right values discarded.

Total. Corresponds to Haskell's Data.Either.lefts :: [Either a b] -> [a].

**[HYDRA-CORE-LIB-EITHERS-LEFTS-EXTRACTS-LEFT-VALUES-PRESERVING-ORDER] (requirement) **lefts returns the list of every left value in the input list, in original order, discarding right values; given a list of only right values, or an empty list, the result is the empty list.

## hydra.core.lib.eithers.map

Map a function over the Right side of an either (standard functor map).

`∀x,y,z.(x → y) → either<z, x> → either<z, y>`

*Since: 0.15.*

map(f, e) returns Right (f y) if e is Right y, or Left x unchanged if e is Left x.

The functor instance for either; treats the Right variant as the focus and leaves the Left variant alone.

Total. Corresponds to Haskell's fmap :: (a -> b) -> Either e a -> Either e b.

**[HYDRA-CORE-LIB-EITHERS-MAP-MAPS-RIGHT-PRESERVES-LEFT] (requirement) **map applies the given function to the contained value when the either is a right, and leaves a left value completely unchanged.

## hydra.core.lib.eithers.mapList

Map a function returning either over a list, collecting results or short-circuiting on Left.

`∀x,y,z.(x → either<z, y>) → list<x> → either<z, list<y>>`

*Since: 0.15.*

mapList(f, xs) applies f to each element of xs. If every application returns Right, the result is Right of the list of contained values, in original order. The first application that returns Left short-circuits the whole result to that Left.

Total. Corresponds to Haskell's traverse :: (a -> Either e b) -> [a] -> Either e [b].

**[HYDRA-CORE-LIB-EITHERS-MAP-LIST-TRAVERSES-COLLECTING-OR-SHORT-CIRCUITING] (requirement) **mapList applies the given either-returning function to each element of the list; if every application returns a right, the result is a right of the list of contained values in original order, and the first application returning a left short-circuits the whole result to that left; folding the empty list returns a right of the empty list.

## hydra.core.lib.eithers.mapOptional

Map a function returning either over an optional, or return Right none if none.

`∀x,y,z.(x → either<z, y>) → optional<x> → either<z, optional<y>>`

*Since: 0.15.*

mapOptional(f, m) returns Right none if m is none; otherwise applies f to the contained value and returns the result with Right wrapped around given.

Total. Corresponds to Haskell's traverse :: (a -> Either e b) -> Maybe a -> Either e (Maybe b).

**[HYDRA-CORE-LIB-EITHERS-MAP-OPTIONAL-TRAVERSES-OPTIONAL-COLLECTING-OR-SHORT-CIRCUITING] (requirement) **mapOptional returns a right of none when the input optional is none; when the input is given a value, it applies the given function to that value and lifts a right result back into right of given, or propagates a left result unchanged.

## hydra.core.lib.eithers.mapSet

Map a function returning either over a set, collecting results or short-circuiting on Left.

`∀x,y,z.(ordering x, ordering y) ⇒ (x → either<z, y>) → set<x> → either<z, set<y>>`

*Since: 0.15.*

mapSet(f, s) applies f to each element of s in unspecified order. If every application returns Right, the result is Right of the set of contained values (deduplicated by the result type's ordering); the first application returning Left short-circuits the whole result to that Left.

Requires an 'ordering' constraint on the input and output element types.

Total. Corresponds to Haskell's traverse-style operation specialised to Set.

**[HYDRA-CORE-LIB-EITHERS-MAP-SET-TRAVERSES-SET-COLLECTING-OR-SHORT-CIRCUITING] (requirement) **mapSet applies the given either-returning function to each element of the set; if every application returns a right, the result is a right of the set of contained values (deduplicated by the result type's ordering), and the first application returning a left short-circuits the whole result to that left; mapping the empty set returns a right of the empty set.

## hydra.core.lib.eithers.partition

Partition a list of either values into lefts and rights.

`∀x,y.list<either<x, y>> → (list<x>, list<y>)`

*Since: 0.17.6.*

partition(xs) returns a pair (lefts, rights) where lefts contains every Left value from xs in original order and rights contains every Right value from xs in original order.

Total. Corresponds to Haskell's Data.Either.partitionEithers :: [Either a b] -> ([a], [b]).

Renamed from hydra.lib.eithers.partitionEithers.

**[HYDRA-CORE-LIB-EITHERS-PARTITION-SPLITS-INTO-LEFTS-AND-RIGHTS] (requirement) **partition returns a pair of (every left value in the input, every right value in the input), each preserving original relative order; an all-left, all-right, or empty input list degrades to the corresponding (full, empty) / (empty, full) / (empty, empty) pairing.

## hydra.core.lib.eithers.right

Construct a right (success/normal) either value.

`∀x,y.y → either<x, y>`

*Since: 0.17.6.*

right(x) is Right x. The dual constructor to left, injecting into the Right (success/normal) side of an either.

Total. Corresponds to Haskell's Right :: b -> Either a b.

**[HYDRA-CORE-LIB-EITHERS-RIGHT-CONSTRUCTS-RIGHT-VARIANT] (requirement) **right wraps a value as the right (success/normal) variant of an either.

## hydra.core.lib.eithers.rights

Extract all Right values from a list of either values.

`∀x,y.list<either<x, y>> → list<y>`

*Since: 0.15.*

rights(xs) returns a list containing every Right value in xs, in original order, with Left values discarded.

Total. Corresponds to Haskell's Data.Either.rights :: [Either a b] -> [b].

**[HYDRA-CORE-LIB-EITHERS-RIGHTS-EXTRACTS-RIGHT-VALUES-PRESERVING-ORDER] (requirement) **rights returns the list of every right value in the input list, in original order, discarding left values; given a list of only left values, or an empty list, the result is the empty list.
