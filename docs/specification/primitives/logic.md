<!-- Note: this is an automatically generated file. Do not edit. -->

# hydra.core.lib.logic

<!-- Note: this is an automatically generated file. Do not edit. -->

## hydra.core.lib.logic.and

Compute the logical AND of two boolean values.

`∀.boolean → boolean → boolean`

*Since: 0.15.*

and(p, q) returns true iff both p and q are true.

Evaluation is strict in both arguments at the primitive level; for short-circuiting behavior, use ifElse.

Total. Corresponds to Haskell's (&&) :: Bool -> Bool -> Bool (but without short-circuit evaluation).

**[HYDRA-CORE-LIB-LOGIC-AND-TRUE-ONLY-WHEN-BOTH-TRUE] (requirement) **and(p, q) returns true exactly when both p and q are true; any false operand makes the result false.

## hydra.core.lib.logic.ifElse

Compute a conditional expression.

`∀x.boolean → x → x → x`

*Since: 0.15.*

ifElse(p, t, f) returns t if p is true, or f if p is false.

The unselected branch is not necessarily evaluated; ifElse is the standard way to express short-circuiting boolean logic and branching in Hydra.

Total. Corresponds to Haskell's if/then/else.

**[HYDRA-CORE-LIB-LOGIC-IF-ELSE-SELECTS-BRANCH-BY-CONDITION] (requirement) **ifElse(p, t, f) returns t when p is true and f when p is false, for any value type of t/f including booleans, integers, and strings -- the then/else branches may be of any (matching) type.

## hydra.core.lib.logic.not

Compute the logical NOT of a boolean value.

`∀.boolean → boolean`

*Since: 0.15.*

not(p) returns false if p is true, or true if p is false.

Total. Corresponds to Haskell's not :: Bool -> Bool.

**[HYDRA-CORE-LIB-LOGIC-NOT-NEGATES-BOOLEAN] (requirement) **not(p) returns false when p is true and true when p is false.

## hydra.core.lib.logic.or

Compute the logical OR of two boolean values.

`∀.boolean → boolean → boolean`

*Since: 0.15.*

or(p, q) returns true iff at least one of p and q is true.

Evaluation is strict in both arguments at the primitive level; for short-circuiting behavior, use ifElse.

Total. Corresponds to Haskell's (||) :: Bool -> Bool -> Bool (but without short-circuit evaluation).

**[HYDRA-CORE-LIB-LOGIC-OR-TRUE-WHEN-EITHER-TRUE] (requirement) **or(p, q) returns true when at least one of p and q is true; it returns false only when both are false.
