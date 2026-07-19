<!-- NOTE: this page will be automatically generated from the primitive definitions in
     packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Lib/Eithers.hs (generator not
     yet built). Hand-authored draft under #417; the specifications here are the normative TARGET
     and may deliberately diverge from the currently-frozen definitions.
     Conventions (notation, laziness, badges, floating point) are defined in index.md. -->

# hydra.lib.eithers

The either type is Hydra's error/alternative type: a value of `either<t1, t2>` is either
`left x`, conventionally carrying an error or alternative of type `t1`, or `right y`, carrying
a success value of type `t2`.
The either type is a right-biased monad: `pure`, `map`, `apply`, and `bind` operate on the
`right` side, and the `left` type is held fixed through a computation, so a chain of steps
shares a common error type.
This module provides the full monad row — `pure`, `map`, `apply`, `bind`, and `compose` —
together with elimination (`cases`), predicates, and traversal.
The fundamental eliminator is `cases`; every other primitive in this module can be derived
from it.

#### apply — **Draft**

`∀t1,t2,t3. either<t1, t2 → t3> → either<t1, t2> → either<t1, t3>`

Usage: `apply ef ex`

Applicative apply for either: combine a function under either and an argument under either.
Returns `right (f x)` when `ef` is `right f` and `ex` is `right x`.
If either argument is a `left`, the result is that `left`; when both arguments are `left`,
the first (function-side) `left` is returned — first error wins, and `ex` is not consulted
once `ef` is known to be a `left`.
`apply ef ex` is `bind ef (λf → map f ex)`; this defining equation is the specification, and
the default implementation.

Since: 0.18

#### bimap — **Draft**

`∀t1,t2,t3,t4. (t1 → t3) → (t2 → t4) → either<t1, t2> → either<t3, t4>`

Usage: `bimap f g e`

Map over both sides of an either value.
Returns `left (f x)` when `e` is `left x`, and `right (g y)` when `e` is `right y`; the result
retains the same `left`/`right` variant as the argument.

Since: 0.15

#### bind — **Draft**

`∀t1,t2,t3. either<t1, t2> → (t2 → either<t1, t3>) → either<t1, t3>`

Usage: `bind e f`

Monadic bind for either, with a fixed `left` type.
Returns `f y` when `e` is `right y`; when `e` is `left x`, the result is `left x`, with the
`left` type preserved.
Used to chain computations that may fail with a common error type.

Since: 0.15

#### cases — **Draft**

`∀t1,t2,t3. either<t1, t2> → (t1 → t3) → (t2 → t3) → t3`

Usage: `cases e f g`

Case analysis on an either value.
Applies the first function to a `left` value and the second to a `right` value: returns `f x`
when `e` is `left x`, and `g y` when `e` is `right y`.
This is the fundamental eliminator for the either type; every other primitive in this module
can be derived from it.
The either value is the first argument, matching `hydra.lib.optionals.cases` and the convention
for case-statement-like elimination.

Since: 0.18 (renamed from `either`, with the scrutinee moved first)

#### compose — **Draft**

`∀t1,t2,t3,t4. (t1 → either<t4, t2>) → (t2 → either<t4, t3>) → t1 → either<t4, t3>`

Usage: `compose f g x`

Kleisli composition for either.
`compose f g x` is `bind (f x) g`; this defining equation is the specification, and the default
implementation.
If either `f x` or the second stage produces a `left`, the result is that `left`.

Since: 0.18

#### foldList — **Draft**

`∀t1,t2,t3. (t1 → t2 → either<t3, t1>) → t1 → list<t2> → either<t3, t1>`

Usage: `foldList f acc xs`

Left-fold over a list with an either-returning function, short-circuiting on `left`.
Folds `f` over `xs` from the left, threading an accumulator of type `t1`, where each
application may fail with a `left`: iterates while `f` returns `right`, propagates the first
`left` without processing further elements, and returns `right` of the final accumulator if
every element was processed.
Equivalent to chaining `bind` over the list.
`foldList f acc xs` is `hydra.lib.lists.foldl (λe y → bind e (λx → f x y)) (pure acc) xs`;
this defining equation is the specification, and the default implementation.

Since: 0.18 (renamed from `hydra.lib.eithers.foldl`)

#### isLeft — **Draft**

`∀t1,t2. either<t1, t2> → boolean`

Usage: `isLeft e`

Test whether an either value is a `left`.
Returns `true` if `e` is a `left` variant, and `false` if it is a `right`.

Since: 0.15

#### isRight — **Draft**

`∀t1,t2. either<t1, t2> → boolean`

Usage: `isRight e`

Test whether an either value is a `right`.
Returns `true` if `e` is a `right` variant, and `false` if it is a `left`.

Since: 0.15

#### lefts — **Draft**

`∀t1,t2. list<either<t1, t2>> → list<t1>`

Usage: `lefts xs`

Extract all `left` values from a list of either values.
Returns the list containing every `left` value in `xs`, in their original order, with `right`
values discarded.

Since: 0.15

#### map — **Draft**

`∀t1,t2,t3. (t1 → t2) → either<t3, t1> → either<t3, t2>`

Usage: `map f e`

Map a function over the `right` side of an either value.
Returns `right (f y)` when `e` is `right y`, and `left x` unchanged when `e` is `left x`.
This is the functor map for either; it treats the `right` variant as the focus and leaves the
`left` variant alone.

Since: 0.15

#### mapList — **Draft**

`∀t1,t2,t3. (t1 → either<t3, t2>) → list<t1> → either<t3, list<t2>>`

Usage: `mapList f xs`

Traverse a list in the either monad.
Applies `f` to each element of `xs`.
If every application returns `right`, the result is `right` of the list of contained values,
in their original order.
The first application that returns a `left` short-circuits the whole result to that `left`.

Since: 0.15

#### mapOptional — **Draft**

`∀t1,t2,t3. (t1 → either<t3, t2>) → optional<t1> → either<t3, optional<t2>>`

Usage: `mapOptional f m`

Traverse an optional in the either monad.
Returns `right none` when `m` is `none`; otherwise applies `f` to the contained value, and
returns the result with the contained value (if any) wrapped in `given`: `right (given y)` when
`f x` is `right y`, or the `left` unchanged when `f x` is a `left`.

Since: 0.15

#### mapSet — **Draft**

`∀t1,t2,t3. (ordering t1, ordering t2) ⇒ (t1 → either<t3, t2>) → set<t1> → either<t3, set<t2>>`

Usage: `mapSet f s`

Traverse a set in the either monad.
Applies `f` to each element of `s`, in unspecified order.
If every application returns `right`, the result is `right` of the set of contained values,
deduplicated by the result type's ordering.
The first application returning a `left` short-circuits the whole result to that `left`;
because the traversal order is unspecified, which `left` is returned when several elements map
to a `left` is also unspecified.

Since: 0.15

#### partition — **Draft**

`∀t1,t2. list<either<t1, t2>> → pair<list<t1>, list<t2>>`

Usage: `partition xs`

Partition a list of either values into lefts and rights.
Returns a pair whose first component contains every `left` value from `xs`, in their original
order, and whose second component contains every `right` value from `xs`, in their original
order.

Since: 0.18 (renamed from `hydra.lib.eithers.partitionEithers`)

#### pure — **Draft**

`∀t1,t2. t2 → either<t1, t2>`

Usage: `pure x`

Wrap a value as a `right`.
`pure x` is `right x`; this defining equation is the specification, and the default
implementation.
This is the unit of the either monad; it exists so that code written generically over a monad
can reach the unit.

Since: 0.18

#### rights — **Draft**

`∀t1,t2. list<either<t1, t2>> → list<t2>`

Usage: `rights xs`

Extract all `right` values from a list of either values.
Returns the list containing every `right` value in `xs`, in their original order, with `left`
values discarded.

Since: 0.15

#### either — **Deprecated**

`∀t1,t2,t3. (t1 → t3) → (t2 → t3) → either<t1, t2> → t3`

Deprecated since: 0.18. Use: `cases`.
Note that the argument order also changed: `cases` takes the scrutinee first.

#### fromLeft — **Deprecated**

`∀t1,t2. t1 → either<t1, t2> → t1`

Deprecated since: 0.18. Use: `cases`.
This was a default-extractor; `cases` expresses it directly.

#### fromRight — **Deprecated**

`∀t1,t2. t2 → either<t1, t2> → t2`

Deprecated since: 0.18. Use: `cases`.
This was a default-extractor; `cases` expresses it directly.

#### foldl — **Deprecated**

`∀t1,t2,t3. (t1 → t2 → either<t3, t1>) → t1 → list<t2> → either<t3, t1>`

Deprecated since: 0.18. Use: `foldList`.
Renamed to distinguish the monadic fold from the plain list fold `hydra.lib.lists.foldl`.

#### partitionEithers — **Deprecated**

`∀t1,t2. list<either<t1, t2>> → pair<list<t1>, list<t2>>`

Deprecated since: 0.18. Use: `partition`.
