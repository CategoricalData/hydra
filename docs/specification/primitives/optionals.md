<!-- NOTE: this page will be automatically generated from the primitive definitions in
     packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Lib/Optionals.hs (generator not
     yet built). Hand-authored draft under #417; the specifications here are the normative TARGET
     and may deliberately diverge from the currently-frozen definitions.
     Conventions (notation, laziness, badges, floating point) are defined in index.md. -->

# hydra.lib.optionals

The optional type is Hydra's presence/absence type: a value of `optional<t>` is either `given x`,
carrying a value `x` of type `t`, or `none`, carrying nothing.
The optional type is a monad, and this module provides the full monad row — `given` (the unit),
`map`, `apply`, `bind`, and `compose` — together with elimination (`cases`), predicates, and
traversal.
The fundamental eliminator is `cases`; every other primitive in this module can be derived
from it.

#### apply — **Draft**

`∀t1,t2. optional<t1 → t2> → optional<t1> → optional<t2>`

Usage: `apply mf mx`

Applicative apply for optionals: combine an optional function and an optional argument.
Returns `given (f x)` when `mf` is `given f` and `mx` is `given x`, and `none` if either
argument is `none`.
Threads a function-in-context with a value-in-context.
`apply mf mx` is `bind mf (λf → map f mx)`; this defining equation is the specification, and
the default implementation.

Since: 0.15

#### bind — **Draft**

`∀t1,t2. optional<t1> → (t1 → optional<t2>) → optional<t2>`

Usage: `bind m f`

Monadic bind for optionals.
Returns `f x` when `m` is `given x`, and `none` when `m` is `none`.
Used to chain computations that may be absent.
`bind m f` is `cases m none f`; this defining equation is the specification, and the default
implementation.

Since: 0.15

#### cases — **Draft**

`∀t1,t2. optional<t1> → t2 → (t1 → t2) → t2`

Usage: `cases m default f`

Case analysis on an optional value.
Returns `f x` when `m` is `given x`, and `default` when `m` is `none`.
This is the fundamental eliminator for the optional type; every other primitive in this module
can be derived from it.
The optional value is the first argument, matching the convention for case-statement-like
elimination.

Lazy: `default` — evaluated only when `m` is `none`.

Since: 0.15

#### compose — **Draft**

`∀t1,t2,t3. (t1 → optional<t2>) → (t2 → optional<t3>) → t1 → optional<t3>`

Usage: `compose f g x`

Kleisli composition for optionals.
`compose f g x` is `bind (f x) g`; this defining equation is the specification, and the default
implementation.
If either `f x` or the second stage produces `none`, the result is `none`.

Since: 0.15

#### foldList — **Draft**

`∀t1,t2. (t1 → t2 → optional<t1>) → t1 → list<t2> → optional<t1>`

Usage: `foldList f acc xs`

Left-fold over a list with an optional-returning function, short-circuiting on `none`.
Threads the accumulator through the optional monad: folds `f` over `xs` from the left,
iterating while each application yields `given`, and returns `none` as soon as any step
yields `none`.
If every element is processed, the result is `given` of the final accumulator.
`foldList f acc xs` is `hydra.lib.lists.foldl (λm y → bind m (λx → f x y)) (given acc) xs`;
this defining equation is the specification, and the default implementation.

Since: 0.18

#### given — **Draft**

`∀t. t → optional<t>`

Usage: `given x`

Wrap a value in `given`.
Returns the optional value containing `x`. The constructor for the present case of an
optional value.

Since: 0.18

#### givens — **Draft**

`∀t. list<optional<t>> → list<t>`

Usage: `givens xs`

Collect the present values from a list of optionals.
Returns the list of contained values from the `given` elements of `xs`, in their original
order; `none` elements are discarded.
Parallel to `hydra.lib.eithers.lefts` and `hydra.lib.eithers.rights`.

Since: 0.18 (renamed from `hydra.lib.optionals.cat`)

#### isGiven — **Draft**

`∀t. optional<t> → boolean`

Usage: `isGiven m`

Test whether an optional value is present.
Returns `true` if `m` is a `given` variant, and `false` if `m` is `none`.

Since: 0.15

#### isNone — **Draft**

`∀t. optional<t> → boolean`

Usage: `isNone m`

Test whether an optional value is absent.
Returns `true` if `m` is the `none` variant, and `false` if `m` is a `given`.

Since: 0.15

#### map — **Draft**

`∀t1,t2. (t1 → t2) → optional<t1> → optional<t2>`

Usage: `map f m`

Map a function over an optional value.
Returns `given (f x)` when `m` is `given x`, and `none` when `m` is `none`.
This is the functor map for optionals.

Since: 0.15

#### mapList — **Draft**

`∀t1,t2. (t1 → optional<t2>) → list<t1> → optional<list<t2>>`

Usage: `mapList f xs`

Traverse a list in the optional monad.
Applies `f` to each element of `xs`.
If every application yields `given`, the result is `given` of the list of contained values,
in their original order.
The result is `none` as soon as any application yields `none`.

Since: 0.18

#### mapSet — **Draft**

`∀t1,t2. (ordering t1, ordering t2) ⇒ (t1 → optional<t2>) → set<t1> → optional<set<t2>>`

Usage: `mapSet f s`

Traverse a set in the optional monad.
Applies `f` to each element of `s`.
If every application yields `given`, the result is `given` of the set of contained values,
deduplicated by the result type's ordering.
The result is `none` as soon as any application yields `none`.

Since: 0.18

#### toList — **Draft**

`∀t. optional<t> → list<t>`

Usage: `toList m`

Convert an optional value to a list.
Returns the singleton list containing `x` when `m` is `given x`, and the empty list when `m`
is `none`.

Since: 0.15

#### withDefault — **Draft**

`∀t. t → optional<t> → t`

Usage: `withDefault default m`

Return the value contained in an optional, falling back to a default if absent.
Returns `x` when `m` is `given x`, and `default` when `m` is `none`.
`withDefault default m` is `cases m default (λx → x)`; this defining equation is the
specification, and the default implementation.
The default comes first — a documented exception to the standard parameter order.

Lazy: `default` — evaluated only when the optional is `none`.

Since: 0.18 (renamed from `hydra.lib.optionals.fromOptional`)

#### cat — **Deprecated**

`∀t. list<optional<t>> → list<t>`

Deprecated since: 0.18. Use: `givens`.

#### fromOptional — **Deprecated**

`∀t. t → optional<t> → t`

Deprecated since: 0.18. Use: `withDefault`.

#### mapOptional — **Deprecated**

`∀t1,t2. (t1 → optional<t2>) → list<t1> → list<t2>`

Deprecated since: 0.18. Use: `hydra.lib.lists.mapGivens`.

<!-- [PENDING]: the name mapOptional is reserved; once this deprecated alias is removed, the
     traversal-family cell optionals.mapOptional ((t1 → optional<t2>) → optional<t1> →
     optional<optional<t2>>) may be added. -->
