<!-- NOTE: this page will be automatically generated from the primitive definitions in
     packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Lib/Functions.hs (a module new
     under #417; neither the module nor the generator exists yet). Hand-authored draft under
     #417; the specifications here are the normative TARGET.
     Conventions (notation, laziness, badges, floating point) are defined in index.md. -->

# hydra.lib.functions

Function combinators: the `identity`/`compose`/`const`/`flip` basis.
These primitives manipulate functions themselves, independently of any particular data type.

#### compose — **Draft**

`∀t1,t2,t3. (t2 → t3) → (t1 → t2) → t1 → t3`

Usage: `compose g f x`

Compose two functions: `compose g f x` is `g (f x)`; this defining equation is the
specification.
The outer function comes first: `f` is applied to `x`, and `g` is applied to the result.
This is ordinary function composition, distinct from the Kleisli `compose` primitives of the
monad modules.

Since: 0.18

#### const — **Draft**

`∀t1,t2. t1 → t2 → t1`

Usage: `const x y`

Return the first argument, ignoring the second: `const x y` is `x`; this defining equation is
the specification.
Partially applied, `const x` is the constant function which returns `x` on every input.

Since: 0.18

#### flip — **Draft**

`∀t1,t2,t3. (t1 → t2 → t3) → t2 → t1 → t3`

Usage: `flip f x y`

Swap the argument order of a binary function: `flip f x y` is `f y x`; this defining equation
is the specification.

Since: 0.18

#### identity — **Draft**

`∀t. t → t`

Usage: `identity x`

Return the argument unchanged: `identity x` is `x`; this defining equation is the
specification.
`identity` is the unit of `compose`: `compose identity f` and `compose f identity` are both
`f`.

Since: 0.18 (moved from `hydra.lib.equality.identity`)
