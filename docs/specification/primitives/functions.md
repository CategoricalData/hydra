<!-- Note: this is an automatically generated file. Do not edit. -->

# hydra.core.lib.functions

<!-- Note: this is an automatically generated file. Do not edit. -->

Function combinators: the identity/compose/const/flip basis. These primitives manipulate functions themselves, independently of any particular data type.

## hydra.core.lib.functions.absurd

Eliminate a value of the uninhabited void type, producing any type.

`∀t1.void → t1`

*Since: 0.17.6.*

absurd(v) is the unique function from void to any type t1; since void has no inhabitants, this function can never actually be applied in a well-typed program.

Its purpose is to discharge an impossible branch in otherwise-total code, such as the left case of an either<void, a> that can never occur.

The eliminator dual to the (nonexistent) introducer for void.

Total, vacuously: there is no well-typed argument on which absurd could be called. Corresponds to Haskell's Data.Void.absurd :: Void -> a.

## hydra.core.lib.functions.compose

Compose two functions.

`∀t1,t2,t3.(t2 → t3) → (t1 → t2) → t1 → t3`

*Since: 0.17.6.*

compose(g, f, x) = g(f(x)); this defining equation is the specification.

The outer function comes first: f is applied to x, and g is applied to the result.

This is ordinary function composition, distinct from the Kleisli compose primitives of the monad modules.

Total. Corresponds to Haskell's (.) :: (b -> c) -> (a -> b) -> a -> c.

## hydra.core.lib.functions.const

Return the first argument, ignoring the second.

`∀t1,t2.t1 → t2 → t1`

*Since: 0.17.6.*

const(x, y) = x; this defining equation is the specification.

Partially applied, const(x) is the constant function which returns x on every input.

Total. Corresponds to Haskell's const :: a -> b -> a.

**[HYDRA-CORE-LIB-FUNCTIONS-CONST-RETURNS-FIRST-ARGUMENT-IGNORING-SECOND] (requirement) **const returns its first argument unchanged and ignores its second argument entirely, regardless of the second argument's type or value.

## hydra.core.lib.functions.flip

Swap the argument order of a binary function.

`∀t1,t2,t3.(t1 → t2 → t3) → t2 → t1 → t3`

*Since: 0.17.6.*

flip(f, x, y) = f(y, x); this defining equation is the specification.

Total. Corresponds to Haskell's flip :: (a -> b -> c) -> b -> a -> c.

**[HYDRA-CORE-LIB-FUNCTIONS-FLIP-SWAPS-ARGUMENT-ORDER] (requirement) **flip(f, x, y) applies f with its two arguments in reversed order, i.e. flip(f, x, y) = f(y, x).

## hydra.core.lib.functions.identity

Return the argument unchanged.

`∀t1.t1 → t1`

*Since: 0.17.6.*

identity(x) = x; this defining equation is the specification. The polymorphic identity function.

identity is the unit of compose: compose(identity, f) and compose(f, identity) are both f.

Total. Corresponds to Haskell's id :: a -> a.

Moved from hydra.lib.equality.identity.

**[HYDRA-CORE-LIB-FUNCTIONS-IDENTITY-RETURNS-ARGUMENT-UNCHANGED] (requirement) **identity returns its argument completely unchanged, for any value.
