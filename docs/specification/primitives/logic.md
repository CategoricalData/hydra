<!-- NOTE: this page will be automatically generated from the primitive definitions in
     packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Lib/Logic.hs (generator not yet
     built). Hand-authored draft under #417; the specifications here are the normative TARGET
     and may deliberately diverge from the currently-frozen definitions.
     Conventions (notation, laziness, badges, floating point) are defined in index.md. -->

# hydra.lib.logic

Boolean operations and branching.
`and` and `or` are short-circuiting: their second argument is evaluated only when the first
does not already determine the result.

#### and — **Draft**

`boolean → boolean → boolean`

Usage: `and p q`

Compute the logical conjunction of two boolean values.
Returns `true` if both `p` and `q` are `true`, and `false` otherwise.
`and p q` is `ifElse p q false`; this defining equation is the specification, and the default
implementation.

Lazy: `q` — evaluated only if `p` is `true`.

Since: 0.15

#### ifElse — **Draft**

`∀t. boolean → t → t → t`

Usage: `ifElse condition ifTrue ifFalse`

Compute a conditional expression.
Returns `ifTrue` if `condition` is `true`, and `ifFalse` if `condition` is `false`.
This is the standard way to express branching in Hydra.

Lazy: `ifTrue`, `ifFalse` — only the selected branch is evaluated; `condition` is strict.

Since: 0.15

#### not — **Draft**

`boolean → boolean`

Usage: `not p`

Compute the logical negation of a boolean value.
Returns `false` if `p` is `true`, and `true` if `p` is `false`.

Since: 0.15

#### or — **Draft**

`boolean → boolean → boolean`

Usage: `or p q`

Compute the logical disjunction of two boolean values.
Returns `true` if at least one of `p` and `q` is `true`, and `false` otherwise.
`or p q` is `ifElse p true q`; this defining equation is the specification, and the default
implementation.

Lazy: `q` — evaluated only if `p` is `false`.

Since: 0.15
