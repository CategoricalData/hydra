<!-- Note: this is an automatically generated file. Do not edit. -->

# hydra.core.lib.optionals

The optional type is Hydra's presence/absence type: a value of optional<t> is either given(x), carrying a value x of type t, or none, carrying nothing.

The optional type is a monad, and this module provides the full monad row -- given (the unit), map, apply, bind, and compose -- together with elimination (match), predicates, and traversal.

## apply

Applicative apply for optionals: combine an optional function and an optional argument.

`∀x, y.apply(mf: optional<x → y>, mx: optional<x>) → optional<y>`

**Parameters**

- `mf` (`optional<x → y>`): the optional function
- `mx` (`optional<x>`): the optional argument

**Returns** `optional<y>`

*Since: 0.15.*

apply(mf, mx) returns given(f x) when mf is given(f) and mx is given(x), and none if either is none.

The applicative apply for optionals; threads a function-in-context with a value-in-context.

Total. Corresponds to Haskell's (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b.

**[HYDRA-CORE-LIB-OPTIONALS-APPLY-APPLIES-FUNCTION-WHEN-BOTH-GIVEN] (requirement)** apply returns given of the function applied to the value when both the function and the argument are given, and none when either side is none.

## bind

Monadic bind for optionals.

`∀x, y.bind(m: optional<x>, f: x → optional<y>) → optional<y>`

**Parameters**

- `m` (`optional<x>`): the optional value to bind
- `f` (`x → optional<y>`): the continuation to apply to the contained value

**Returns** `optional<y>`

*Since: 0.15.*

bind(m, f) returns f(x) when m is given(x), and none when m is none.

The monadic bind for optionals; used to chain computations that may be absent.

Total. Corresponds to Haskell's (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b.

**[HYDRA-CORE-LIB-OPTIONALS-BIND-CHAINS-ON-GIVEN-PROPAGATES-NONE] (requirement)** bind applies the continuation to the contained value when the optional is given, returning the continuation's own result; when the optional is none, bind returns none without invoking the continuation.

## compose

Kleisli composition for optionals.

`∀x, y, z.compose(f: x → optional<y>, g: y → optional<z>, x: x) → optional<z>`

**Parameters**

- `f` (`x → optional<y>`): the first Kleisli arrow
- `g` (`y → optional<z>`): the second Kleisli arrow
- `x` (`x`): the value to apply the composition to

**Returns** `optional<z>`

*Since: 0.15.*

compose(f, g, x) returns the Kleisli composition of f and g applied to x: bind(f(x), g).

If either f or the second stage produces none, the result is none.

Total. Corresponds to Haskell's Kleisli composition for Maybe, (>=>) :: (a -> Maybe b) -> (b -> Maybe c) -> a -> Maybe c.

**[HYDRA-CORE-LIB-OPTIONALS-COMPOSE-KLEISLI-COMPOSES-SHORT-CIRCUITING-ON-NONE] (requirement)** compose(f, g, x) applies f to x, then g to the result of f, short-circuiting to none as soon as either stage yields none.

## foldList

Left-fold over a list with an optional-returning function, short-circuiting on none.

`∀x, y.foldList(f: x → y → optional<x>, acc: x, xs: list<y>) → optional<x>`

**Parameters**

- `f` (`x → y → optional<x>`): the optional-returning step function
- `acc` (`x`): the initial accumulator
- `xs` (`list<y>`): the list to fold over

**Returns** `optional<x>`

*Since: 0.17.6.*

foldList(f, acc, xs) folds f over xs from the left, iterating while each application yields given, and returns none as soon as any step yields none. If every element is processed, the result is given of the final accumulator.

foldList(f, acc, xs) is lists.foldl(\m y -> bind(m, \x -> f(x, y)), given(acc), xs); this defining equation is the specification, and the default implementation.

Total. Corresponds to a short-circuiting foldM specialised to Maybe.

## given

Wrap a value in given.

`∀x.given(x: x) → optional<x>`

**Parameters**

- `x` (`x`): the value to wrap in given

**Returns** `optional<x>`

*Since: 0.17.6.*

given(x) is the optional value containing x.

The constructor for the present case of an optional value.

Total. Corresponds to Haskell's Just :: a -> Maybe a.

**[HYDRA-CORE-LIB-OPTIONALS-GIVEN-WRAPS-VALUE-AS-PRESENT] (requirement)** given wraps a value as the present variant of an optional, for any value type including integers and strings.

## givens

Concatenate optionals, keeping only the present values.

`∀x.givens(xs: list<optional<x>>) → list<x>`

**Parameters**

- `xs` (`list<optional<x>>`): the list of optionals to concatenate

**Returns** `list<x>`

*Since: 0.17.6.*

givens(xs) returns the list of contained values from given elements of xs, in original order; none elements are discarded.

Total. Corresponds to Haskell's Data.Maybe.catMaybes :: [Maybe a] -> [a].

Renamed from hydra.lib.optionals.cat.

**[HYDRA-CORE-LIB-OPTIONALS-GIVENS-KEEPS-PRESENT-VALUES-PRESERVING-ORDER] (requirement)** givens returns the list of contained values from every given element of the input list, in original order, discarding none elements; an all-none or empty input list yields the empty list.

## isGiven

Test whether an optional is present (given).

`∀x.isGiven(m: optional<x>) → boolean`

**Parameters**

- `m` (`optional<x>`): the optional to test

**Returns** `boolean`

*Since: 0.15.*

isGiven(m) returns true iff m is a given variant.

Total. Corresponds to Haskell's Data.Maybe.isJust :: Maybe a -> Bool.

**[HYDRA-CORE-LIB-OPTIONALS-IS-GIVEN-TESTS-PRESENT-VARIANT] (requirement)** isGiven returns true exactly when the optional is the given variant, and false for none.

## isNone

Test whether an optional is absent (none).

`∀x.isNone(m: optional<x>) → boolean`

**Parameters**

- `m` (`optional<x>`): the optional to test

**Returns** `boolean`

*Since: 0.15.*

isNone(m) returns true iff m is the none variant.

Total. Corresponds to Haskell's Data.Maybe.isNothing :: Maybe a -> Bool.

**[HYDRA-CORE-LIB-OPTIONALS-IS-NONE-TESTS-ABSENT-VARIANT] (requirement)** isNone returns true exactly when the optional is the none variant, and false for given.

## map

Map a function over an optional.

`∀x, y.map(f: x → y, m: optional<x>) → optional<y>`

**Parameters**

- `f` (`x → y`): the function to apply to the contained value
- `m` (`optional<x>`): the optional to map over

**Returns** `optional<y>`

*Since: 0.15.*

map(f, m) returns given(f x) when m is given(x), and none when m is none.

The functor instance for optionals.

Total. Corresponds to Haskell's fmap :: (a -> b) -> Maybe a -> Maybe b.

**[HYDRA-CORE-LIB-OPTIONALS-MAP-MAPS-GIVEN-LEAVES-NONE-UNCHANGED] (requirement)** map applies the given function to the contained value when the optional is given, and leaves none completely unchanged.

## mapList

Traverse a list in the optional monad.

`∀x, y.mapList(f: x → optional<y>, xs: list<x>) → optional<list<y>>`

**Parameters**

- `f` (`x → optional<y>`): the optional-returning function to apply to each element
- `xs` (`list<x>`): the list to traverse

**Returns** `optional<list<y>>`

*Since: 0.17.6.*

mapList(f, xs) applies f to each element of xs. If every application yields given, the result is given of the list of contained values, in their original order. The result is none as soon as any application yields none.

Total. Corresponds to Haskell's traverse :: (a -> Maybe b) -> [a] -> Maybe [b].

## mapOptional

Map a partial function over a list, keeping only the present results.

`∀x, y.mapOptional(f: x → optional<y>, xs: list<x>) → list<y>`

**Parameters**

- `f` (`x → optional<y>`): the partial function to apply to each element
- `xs` (`list<x>`): the list to map over

**Returns** `list<y>`

*Deprecated since: 0.18.*

mapOptional(f, xs) applies f to each element of xs and returns the list of contained values from given results in original order; none results are discarded.

Total. Corresponds to Haskell's Data.Maybe.mapMaybe :: (a -> Maybe b) -> [a] -> [b].

Deprecated since 0.18. Use: hydra.core.lib.lists.mapGivens.

**[HYDRA-CORE-LIB-OPTIONALS-MAP-OPTIONAL-FILTERS-AND-TRANSFORMS-KEEPING-GIVEN] (requirement)** mapOptional applies the given partial (optional-returning) function to each element of the input list and returns the list of contained values from every given result, in original order, discarding elements for which the function returned none; an input that yields no given results, or an empty input list, produces the empty list.

## mapSet

Traverse a set in the optional monad.

`∀x, y.(ordering x, ordering y) ⇒ mapSet(f: x → optional<y>, s: set<x>) → optional<set<y>>`

**Parameters**

- `f` (`x → optional<y>`): the optional-returning function to apply to each element
- `s` (`set<x>`): the set to traverse

**Returns** `optional<set<y>>`

*Since: 0.17.6.*

mapSet(f, s) applies f to each element of s. If every application yields given, the result is given of the set of contained values, deduplicated by the result type's ordering. The result is none as soon as any application yields none.

Requires 'ordering' constraints on both element types (the set type contract).

Total. Corresponds to a traverse-style operation specialised to Set in the Maybe monad.

## match

The fundamental eliminator for the optional type, scrutinee-first.

`∀x, y.match(m: optional<x>, def: lazy y, f: x → y) → y`

**Parameters**

- `m` (`optional<x>`): the optional value to eliminate
- `def` (`y`): the value to return when the optional is none
- `f` (`x → y`): the function to apply to the contained value when the optional is given

**Returns** `y`

*Since: 0.15.*

match(m, def, f) returns f(x) when m is given(x), and def when m is none.

The fundamental eliminator for the optional type; every other primitive in this namespace can be derived from it. The optional value is the first argument, matching the convention for case-statement-like elimination.

Total. Argument order is (m, def, f) rather than Haskell's maybe :: (def, f, m).

**[HYDRA-CORE-LIB-OPTIONALS-MATCH-ELIMINATES-BY-VARIANT] (requirement)** match returns the result of applying the given function to the contained value when the optional is given, and returns the default value unevaluated-until-needed when the optional is none.

## toList

Convert an optional to a list: given x maps to [x], none to [].

`∀x.toList(m: optional<x>) → list<x>`

**Parameters**

- `m` (`optional<x>`): the optional to convert

**Returns** `list<x>`

*Since: 0.15.*

toList(m) returns [x] when m is given(x), and the empty list when m is none.

Total. Corresponds to Haskell's Data.Maybe.maybeToList :: Maybe a -> [a].

**[HYDRA-CORE-LIB-OPTIONALS-TO-LIST-CONVERTS-GIVEN-TO-SINGLETON-NONE-TO-EMPTY] (requirement)** toList converts a given value to a one-element list containing it, and none to the empty list.

## withDefault

Return the value contained in an optional, falling back to a default if absent.

`∀x.withDefault(def: lazy x, m: optional<x>) → x`

**Parameters**

- `def` (`x`): the default value to return when the optional is none
- `m` (`optional<x>`): the optional to unwrap

**Returns** `x`

*Since: 0.17.6.*

withDefault(def, m) returns x when m is given(x), and def when m is none.

Total. Corresponds to Haskell's Data.Maybe.fromMaybe :: a -> Maybe a -> a.

Renamed from hydra.lib.optionals.fromOptional.

**[HYDRA-CORE-LIB-OPTIONALS-WITH-DEFAULT-RETURNS-CONTAINED-VALUE-OR-DEFAULT] (requirement)** withDefault returns the contained value when the optional is given, and the supplied default when the optional is none.
