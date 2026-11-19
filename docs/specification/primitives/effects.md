<!-- Note: this is an automatically generated file. Do not edit. -->

# hydra.core.lib.effects

A value of effect<t> describes a deferred, executable computation which, when executed by a host interpreter, produces a value of type t; describing a computation is separate from executing it, and Hydra uses the effect type to keep that boundary explicit.

There is no eliminator (cases) for effects: an effect value is opaque, consumable only by further composition within this module or by an executor outside the language.

Every primitive in this module is pure: constructing or composing an effect description is a referentially transparent operation on ordinary values, and nothing observable happens until an executor runs the description. Effect descriptions have no identity semantics -- two equal descriptions are interchangeable -- so the usual pure-term transformations are valid over effect-building code. (Purity is independent of implementation strategy: a host may implement these combinators natively rather than from a default implementation.)

A host interpreter of effect<t> must honor, in addition to the purity of the combinators above: purity of construction (building or composing a description performs no host interaction; only an executor performing the described computation may interact with the host); order (when a combinator specifies an order between the effects it composes, an executor performs them in exactly that order); exactly-once (each composed effect is performed exactly once per execution, never skipped, never repeated); and deferral (an effect specified to run after another must not be performed, nor its producing function evaluated for effect, until the prior effect has run to completion and its value is available). These obligations are observable -- the effects are real host interactions -- so they are normative, not advisory.

effect<t> is total: a leaf effect always completes and yields a value (for hydra.lib.files, an either<FileError, t>); the obligations above govern order, multiplicity, and deferral, not failure short-circuiting.

## apply

Applicative apply for effects.

`∀x, y.apply(ef: effect<x → y>, ex: effect<x>) → effect<y>`

**Parameters**

- `ef` (`effect<x → y>`): the effect producing the function to apply
- `ex` (`effect<x>`): the effect producing the argument

**Returns** `effect<y>`

*Since: 0.17.*

apply(ef, ex) describes an effectful computation which first interprets ef to produce a function, then interprets ex to produce an argument, and returns the result of applying the function to the argument.

**[HYDRA-CORE-LIB-EFFECTS-APPLY-APPLIES-EFFECTFUL-FUNCTION-TO-EFFECTFUL-ARGUMENT] (requirement)** apply interprets the function-producing effect and the argument-producing effect, then applies the resulting function to the resulting argument.

## bind

Sequence two effectful computations.

`∀x, y.bind(e: effect<x>, f: x → effect<y>) → effect<y>`

**Parameters**

- `e` (`effect<x>`): the effect to interpret first
- `f` (`x → effect<y>`): the function producing the next effect from e's result

**Returns** `effect<y>`

*Since: 0.17.*

bind(e, f) describes an effectful computation which first interprets e, then passes its result to f to obtain the next effect.

Host interpreters provide the operational meaning; Hydra uses the type effect<t> to keep the effect boundary explicit.

**[HYDRA-CORE-LIB-EFFECTS-BIND-THREADS-RESULT-INTO-NEXT-EFFECT] (requirement)** bind passes the result of the first effect to the given function to obtain the next effect, whose result is the overall result.

**[HYDRA-CORE-LIB-EFFECTS-BIND-PERFORMS-EFFECTS-IN-ORDER-EXACTLY-ONCE] (requirement)** bind performs the first effect to completion, exactly once, before performing the second effect, also exactly once -- observable via real host side effects (not merely by return value), ruling out a host that reorders, double-runs, skips, or eagerly runs either effect.

## compose

Kleisli composition for effects.

`∀x, y, z.compose(f: x → effect<y>, g: y → effect<z>, x: x) → effect<z>`

**Parameters**

- `f` (`x → effect<y>`): the first effect-returning function
- `g` (`y → effect<z>`): the second effect-returning function
- `x` (`x`): the input to the composed computation

**Returns** `effect<z>`

*Since: 0.17.*

compose(f, g, x) describes an effectful computation equivalent to bind(f(x), g).

**[HYDRA-CORE-LIB-EFFECTS-COMPOSE-SEQUENCES-TWO-KLEISLI-ARROWS] (requirement)** compose(f, g, x) runs f on x to obtain the first effect, then runs g on that effect's result to obtain the second effect, equivalent to bind(f(x), g).

## foldList

Left-fold over a list with an effect-returning function.

`∀x, y.foldList(f: x → y → effect<x>, acc0: x, xs: list<y>) → effect<x>`

**Parameters**

- `f` (`x → y → effect<x>`): the effect-returning combining function
- `acc0` (`x`): the initial accumulator value
- `xs` (`list<y>`): the list to fold over

**Returns** `effect<x>`

*Since: 0.17.6.*

foldList(f, acc0, xs) describes an effectful left fold over xs, sequencing applications of f from left to right.

Renamed from hydra.lib.effects.foldl.

**[HYDRA-CORE-LIB-EFFECTS-FOLD-LIST-SEQUENCES-EFFECT-RETURNING-STEP-OVER-LIST] (requirement)** foldList sequences an effect-returning step function over the list's elements, threading the accumulator through, and the overall result is the final accumulator.

**[HYDRA-CORE-LIB-EFFECTS-FOLD-LIST-PERFORMS-ELEMENT-EFFECTS-LEFT-TO-RIGHT-EXACTLY-ONCE] (requirement)** foldList performs the element effects left-to-right in list order, each exactly once -- observable via real host side effects, ruling out a host that reorders, double-runs, or skips an element's effect.

## map

Map a pure function over the result of an effect.

`∀x, y.map(f: x → y, e: effect<x>) → effect<y>`

**Parameters**

- `f` (`x → y`): the pure function to apply to the effect's result
- `e` (`effect<x>`): the effect to interpret

**Returns** `effect<y>`

*Since: 0.17.*

map(f, e) describes an effectful computation which interprets e and applies f to its result.

Equivalent to bind(e, \x -> pure(f x)).

**[HYDRA-CORE-LIB-EFFECTS-MAP-APPLIES-PURE-FUNCTION-TO-EFFECT-RESULT] (requirement)** map interprets the given effect and applies the pure function to its result; equivalent to bind(e, \x -> pure(f x)).

## mapList

Map an effect-returning function over a list.

`∀x, y.mapList(f: x → effect<y>, xs: list<x>) → effect<list<y>>`

**Parameters**

- `f` (`x → effect<y>`): the effect-returning function to apply to each element
- `xs` (`list<x>`): the list to map over

**Returns** `effect<list<y>>`

*Since: 0.17.*

mapList(f, xs) describes an effectful computation which applies f to each element of xs from left to right and collects the results.

**[HYDRA-CORE-LIB-EFFECTS-MAP-LIST-APPLIES-EFFECT-RETURNING-FUNCTION-AND-COLLECTS-RESULTS] (requirement)** mapList applies the effect-returning function to each element and collects the per-element results into a list of the same length, in order.

**[HYDRA-CORE-LIB-EFFECTS-MAP-LIST-PERFORMS-ELEMENT-EFFECTS-LEFT-TO-RIGHT-EXACTLY-ONCE] (requirement)** mapList performs the element effects left-to-right in list order, each exactly once -- observable via real host side effects, ruling out a host that reorders, double-runs, or skips an element's effect.

## mapOptional

Map an effect-returning function over an optional.

`∀x, y.mapOptional(f: x → effect<y>, m: optional<x>) → effect<optional<y>>`

**Parameters**

- `f` (`x → effect<y>`): the effect-returning function to apply to the contained value
- `m` (`optional<x>`): the optional to map over

**Returns** `effect<optional<y>>`

*Since: 0.17.*

mapOptional(f, m) describes an effectful computation which returns none when m is none, or applies f to the contained value and wraps the result in given.

**[HYDRA-CORE-LIB-EFFECTS-MAP-OPTIONAL-APPLIES-FUNCTION-WHEN-PRESENT-PASSES-THROUGH-NONE-WHEN-ABSENT] (requirement)** mapOptional over a present value applies the effect-returning function and wraps the result in given; mapOptional over none yields none, with the function not applied.

**[HYDRA-CORE-LIB-EFFECTS-MAP-OPTIONAL-PERFORMS-EXACTLY-ZERO-OR-ONE-EFFECT] (requirement)** mapOptional over none performs zero effects; mapOptional over a present value performs exactly one effect -- observable via a real host side effect, distinguishing "the function was never invoked" from "the function was invoked and its effect ran".

## pure

Lift a pure value into an effect.

`∀x.pure(x: x) → effect<x>`

**Parameters**

- `x` (`x`): the pure value to lift into an effect

**Returns** `effect<x>`

*Since: 0.17.*

pure(x) describes an effectful computation which succeeds with x without requiring host interaction.

**[HYDRA-CORE-LIB-EFFECTS-PURE-LIFTS-VALUE-WITHOUT-HOST-INTERACTION] (requirement)** pure(x) succeeds with exactly x, without requiring any host interaction.
