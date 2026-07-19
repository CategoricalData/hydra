<!-- NOTE: this page will be automatically generated from the primitive definitions in
     packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Lib/Effects.hs (generator not
     yet built). Hand-authored draft under #417; the specifications here are the normative TARGET
     and may deliberately diverge from the currently-frozen definitions.
     Conventions (notation, laziness, badges, floating point) are defined in index.md. -->

# hydra.lib.effects

The effect monad: sequencing and traversal of effectful computations.
A value of `effect<t>` describes a deferred, executable computation which, when executed by a
host interpreter, produces a value of type `t`; describing a computation is separate from
executing it, and Hydra uses the `effect` type to keep that boundary explicit.
There is no eliminator (`cases`) for effects: an effect value is opaque, consumable only by
further composition within this module or by an executor outside the language.
Every primitive in this module is pure: constructing or composing an effect description is a
referentially transparent operation on ordinary values, and nothing observable happens until
an executor runs the description.
Effect descriptions have no identity semantics — two equal descriptions are interchangeable —
so the usual pure-term transformations are valid over effect-building code.
(Purity is independent of implementation strategy: a host may implement these combinators
natively rather than from a default implementation.)

#### apply — **Draft**

`∀t1,t2. effect<t1 → t2> → effect<t1> → effect<t2>`

Usage: `apply ef ex`

Applicative apply for effects.
Describes an effectful computation which first interprets `ef` to produce a function, then
interprets `ex` to produce an argument, and returns the result of applying the function to the
argument.
The effects are sequenced in that order: `ef` first, then `ex`.

Since: 0.17

#### bind — **Draft**

`∀t1,t2. effect<t1> → (t1 → effect<t2>) → effect<t2>`

Usage: `bind e f`

Sequence two effectful computations.
Describes an effectful computation which first interprets `e`, then passes its result to `f`
to obtain the next effect.
Host interpreters provide the operational meaning; Hydra uses the type `effect<t>` to keep the
effect boundary explicit.

Since: 0.17

#### compose — **Draft**

`∀t1,t2,t3. (t1 → effect<t2>) → (t2 → effect<t3>) → t1 → effect<t3>`

Usage: `compose f g x`

Kleisli composition for effects.
`compose f g x` describes an effectful computation equivalent to `bind (f x) g`: the effect
described by `f x` is interpreted first, and its result is passed to `g` to obtain the second
effect.

Since: 0.17

#### foldList — **Draft**

`∀t1,t2. (t1 → t2 → effect<t1>) → t1 → list<t2> → effect<t1>`

Usage: `foldList f acc xs`

Left-fold over a list with an effect-returning function.
Describes an effectful left fold over `xs`, sequencing the applications of `f` from left to
right in list order and threading the accumulator through each step.
The described computation produces the final accumulator.

Since: 0.18 (renamed from `hydra.lib.effects.foldl`)

#### map — **Draft**

`∀t1,t2. (t1 → t2) → effect<t1> → effect<t2>`

Usage: `map f e`

Map a pure function over the result of an effect.
Describes an effectful computation which interprets `e` and applies `f` to its result.
`map f e` is equivalent to `bind e (λx → pure (f x))`.

Since: 0.17

#### mapList — **Draft**

`∀t1,t2. (t1 → effect<t2>) → list<t1> → effect<list<t2>>`

Usage: `mapList f xs`

Map an effect-returning function over a list.
Describes an effectful computation which applies `f` to each element of `xs` from left to right,
sequencing the effects in list order, and collects the results in the corresponding order.

Since: 0.17

#### mapOptional — **Draft**

`∀t1,t2. (t1 → effect<t2>) → optional<t1> → effect<optional<t2>>`

Usage: `mapOptional f m`

Map an effect-returning function over an optional.
Describes an effectful computation which returns `none` when `m` is `none` (interpreting no
effect from `f`), or applies `f` to the contained value and wraps the result in `given`.

Since: 0.17

#### mapSet — **Draft**

`∀t1,t2. (ordering t1, ordering t2) ⇒ (t1 → effect<t2>) → set<t1> → effect<set<t2>>`

Usage: `mapSet f s`

Map an effect-returning function over a set.
Describes an effectful computation which applies `f` to each element of `s`, sequencing the
effects in ascending element order (the order is normative, since the effects are observable),
and collects the results as a set, deduplicated by the result type's ordering.

Since: 0.18

#### pure — **Draft**

`∀t. t → effect<t>`

Usage: `pure x`

Lift a pure value into an effect.
Describes an effectful computation which succeeds with `x` without requiring host interaction.
This is the unit of the effect monad.

Since: 0.17

#### foldl — **Deprecated**

`∀t1,t2. (t1 → t2 → effect<t1>) → t1 → list<t2> → effect<t1>`

Deprecated since: 0.18. Use: `foldList`.
Renamed to distinguish the monadic fold from the plain list fold `hydra.lib.lists.foldl`.
