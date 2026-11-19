<!-- Note: this is an automatically generated file. Do not edit. -->

# hydra.core.lib.functions

Function combinators: the identity/compose/const/flip basis. These primitives manipulate functions themselves, independently of any particular data type.

## absurd

Eliminate a value of the uninhabited void type, producing any type.

`∀t1.absurd(v: void) → t1`

**Parameters**

- `v` (`void`): a value of the void type; unreachable in any well-typed program

**Returns** `t1`

*Since: 0.17.6.*

absurd(v) is the unique function from void to any type t1; since void has no inhabitants, this function can never actually be applied in a well-typed program.

Its purpose is to discharge an impossible branch in otherwise-total code, such as the left case of an either<void, a> that can never occur.

The eliminator dual to the (nonexistent) introducer for void.

Total, vacuously: there is no well-typed argument on which absurd could be called. Corresponds to Haskell's Data.Void.absurd :: Void -> a.

## compose

Compose two functions.

`∀t1, t2, t3.compose(g: t2 → t3, f: t1 → t2, x: t1) → t3`

**Parameters**

- `g` (`t2 → t3`): the outer function, applied to the result of f
- `f` (`t1 → t2`): the inner function, applied to x first
- `x` (`t1`): the argument to which f is applied

**Returns** `t3`

*Since: 0.17.6.*

compose(g, f, x) = g(f(x)); this defining equation is the specification.

The outer function comes first: f is applied to x, and g is applied to the result.

This is ordinary function composition, distinct from the Kleisli compose primitives of the monad modules.

Total. Corresponds to Haskell's (.) :: (b -> c) -> (a -> b) -> a -> c.

## const

Return the first argument, ignoring the second.

`∀t1, t2.const(x: t1, y: t2) → t1`

**Parameters**

- `x` (`t1`): the value to return
- `y` (`t2`): the ignored second argument

**Returns** `t1`

*Since: 0.17.6.*

const(x, y) = x; this defining equation is the specification.

Partially applied, const(x) is the constant function which returns x on every input.

Total. Corresponds to Haskell's const :: a -> b -> a.

**[HYDRA-CORE-LIB-FUNCTIONS-CONST-RETURNS-FIRST-ARGUMENT-IGNORING-SECOND] (requirement)** const returns its first argument unchanged and ignores its second argument entirely, regardless of the second argument's type or value.

## flip

Swap the argument order of a binary function.

`∀t1, t2, t3.flip(f: t1 → t2 → t3, x: t2, y: t1) → t3`

**Parameters**

- `f` (`t1 → t2 → t3`): the binary function whose argument order is swapped
- `x` (`t2`): the argument passed as f's second argument
- `y` (`t1`): the argument passed as f's first argument

**Returns** `t3`

*Since: 0.17.6.*

flip(f, x, y) = f(y, x); this defining equation is the specification.

Total. Corresponds to Haskell's flip :: (a -> b -> c) -> b -> a -> c.

**[HYDRA-CORE-LIB-FUNCTIONS-FLIP-SWAPS-ARGUMENT-ORDER] (requirement)** flip(f, x, y) applies f with its two arguments in reversed order, i.e. flip(f, x, y) = f(y, x).

## identity

Return the argument unchanged.

`∀t1.identity(x: t1) → t1`

**Parameters**

- `x` (`t1`): the value to return unchanged

**Returns** `t1`

*Since: 0.17.6.*

identity(x) = x; this defining equation is the specification. The polymorphic identity function.

identity is the unit of compose: compose(identity, f) and compose(f, identity) are both f.

Total. Corresponds to Haskell's id :: a -> a.

Moved from hydra.lib.equality.identity.

**[HYDRA-CORE-LIB-FUNCTIONS-IDENTITY-RETURNS-ARGUMENT-UNCHANGED] (requirement)** identity returns its argument completely unchanged, for any value.
