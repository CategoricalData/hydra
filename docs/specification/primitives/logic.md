<!-- Note: this is an automatically generated file. Do not edit. -->

# hydra.core.lib.logic

## and

Compute the logical AND of two boolean values.

`and(p: boolean, q: lazy boolean) → boolean`

**Parameters**

- `p` (`boolean`): the first boolean operand
- `q` (`boolean`): the second boolean operand

**Returns** `boolean`

*Since: 0.15.*

and(p, q) returns true iff both p and q are true.

The second operand is not necessarily evaluated: and is short-circuiting, and returns false as soon as the first operand is false without evaluating the second.

Total. Corresponds to Haskell's (&&) :: Bool -> Bool -> Bool.

**[HYDRA-CORE-LIB-LOGIC-AND-TRUE-ONLY-WHEN-BOTH-TRUE] (requirement)** and(p, q) returns true exactly when both p and q are true; any false operand makes the result false.

## ifElse

Compute a conditional expression.

`∀x.ifElse(p: boolean, t: lazy x, f: lazy x) → x`

**Parameters**

- `p` (`boolean`): the boolean condition
- `t` (`x`): the value returned when p is true
- `f` (`x`): the value returned when p is false

**Returns** `x`

*Since: 0.15.*

ifElse(p, t, f) returns t if p is true, or f if p is false.

The unselected branch is not necessarily evaluated; ifElse is the standard way to express short-circuiting boolean logic and branching in Hydra.

Total. Corresponds to Haskell's if/then/else.

**[HYDRA-CORE-LIB-LOGIC-IF-ELSE-SELECTS-BRANCH-BY-CONDITION] (requirement)** ifElse(p, t, f) returns t when p is true and f when p is false, for any value type of t/f including booleans, integers, and strings -- the then/else branches may be of any (matching) type.

## not

Compute the logical NOT of a boolean value.

`not(p: boolean) → boolean`

**Parameters**

- `p` (`boolean`): the boolean value to negate

**Returns** `boolean`

*Since: 0.15.*

not(p) returns false if p is true, or true if p is false.

Total. Corresponds to Haskell's not :: Bool -> Bool.

**[HYDRA-CORE-LIB-LOGIC-NOT-NEGATES-BOOLEAN] (requirement)** not(p) returns false when p is true and true when p is false.

## or

Compute the logical OR of two boolean values.

`or(p: boolean, q: lazy boolean) → boolean`

**Parameters**

- `p` (`boolean`): the first boolean operand
- `q` (`boolean`): the second boolean operand

**Returns** `boolean`

*Since: 0.15.*

or(p, q) returns true iff at least one of p and q is true.

The second operand is not necessarily evaluated: or is short-circuiting, and returns true as soon as the first operand is true without evaluating the second.

Total. Corresponds to Haskell's (||) :: Bool -> Bool -> Bool.

**[HYDRA-CORE-LIB-LOGIC-OR-TRUE-WHEN-EITHER-TRUE] (requirement)** or(p, q) returns true when at least one of p and q is true; it returns false only when both are false.
