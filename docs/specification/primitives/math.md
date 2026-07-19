<!-- NOTE: this page will be automatically generated from the primitive definitions in
     packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Lib/Math.hs (generator not yet
     built). Hand-authored draft under #417; the specifications here are the normative TARGET
     and may deliberately diverge from the currently-frozen definitions.
     Conventions (notation, laziness, badges, floating point) are defined in index.md. -->

# hydra.lib.math

Arithmetic and numeric functions.
The core arithmetic operations — `add`, `sub`, `mul`, `negate` — are polymorphic over the
`numeric` class: fixed-width integer types wrap in two's complement, `bigint` is
arbitrary-precision, and floating-point types follow IEEE 754.
The remaining integer operations (`div`, `mod`, `rem`, `abs`, `signum`, `even`, `odd`, `range`)
are int32-typed for now and generalize when the class machinery lands (#566).
Floating-point special values (NaN, ±∞, negative zero) follow
[Floating-point values](index.md#floating-point-values).

#### abs — **Draft**

`int32 → int32`

Usage: `abs x`

The absolute value of an int32.
For non-negative inputs the result equals the input; for negative inputs the result is the
arithmetic negation.
Total but not injective at the boundary: `abs -2147483648` is `-2147483648` (the minimum
int32), because +2147483648 is not representable in int32.

Since: 0.15

#### acos — **Draft**

`float64 → float64`

Usage: `acos x`

The principal value of the inverse cosine of `x`, in radians (IEEE 754 §9.2 acos).
The result is in [0, π].
For arguments outside the domain [-1, 1] (including ±∞) the result is NaN; `acos NaN` is NaN.
Results follow the host's math library: implementations should be faithfully rounded (error at
most one unit in the last place), and results may differ across hosts in the final unit of
precision.
Special values are exact as specified.

Since: 0.15

#### acosh — **Draft**

`float64 → float64`

Usage: `acosh x`

The principal value of the inverse hyperbolic cosine of `x` (IEEE 754 §9.2 acosh).
The result is in [0, +∞).
For arguments less than 1 the result is NaN; `acosh 1.0` is +0; `acosh +∞` is +∞; `acosh NaN`
is NaN.
Results follow the host's math library: implementations should be faithfully rounded (error at
most one unit in the last place), and results may differ across hosts in the final unit of
precision.
Special values are exact as specified.

Since: 0.15

#### add — **Draft**

`∀t. (numeric t) ⇒ t → t → t`

Usage: `add x y`

Addition, polymorphic over numeric types.
Returns the sum of `x` and `y`, with semantics determined by the type.
For fixed-width integer types (int8 through int64, uint8 through uint64), the result is the
mathematical sum reduced modulo 2^N (N the bit width) and reinterpreted per the type's
signedness; arithmetic wraps silently on overflow, with no exception raised.
For `bigint`, the result is the exact mathematical sum (arbitrary precision).
For floating-point types, the result is IEEE 754 addition: the exact sum rounded to the nearest
representable value under roundTiesToEven; adding infinities of opposite sign produces NaN;
adding NaN to any value produces NaN; the sum of two zeros is +0, except that
`add -0.0 -0.0` is -0.
Instances: the nine integer types (int8, int16, int32, int64, uint8, uint16, uint32,
uint64, bigint) and the two floating-point types (float32, float64).
`decimal` has no numeric instance: it is a presentation/interchange type, and exact division
without a precision context is not well-defined; see classes.md for the re-open trigger.

Since: 0.18 (generalized from int32-only; the `numeric` class is #566)

#### asin — **Draft**

`float64 → float64`

Usage: `asin x`

The principal value of the inverse sine of `x`, in radians (IEEE 754 §9.2 asin).
The result is in [-π/2, π/2].
For arguments outside the domain [-1, 1] (including ±∞) the result is NaN; `asin ±0.0` is ±0;
`asin NaN` is NaN.
Results follow the host's math library: implementations should be faithfully rounded (error at
most one unit in the last place), and results may differ across hosts in the final unit of
precision.
Special values are exact as specified.

Since: 0.15

#### asinh — **Draft**

`float64 → float64`

Usage: `asinh x`

The principal value of the inverse hyperbolic sine of `x` (IEEE 754 §9.2 asinh).
Defined for all finite reals; `asinh ±0.0` is ±0; `asinh ±∞` is ±∞; `asinh NaN` is NaN.
Results follow the host's math library: implementations should be faithfully rounded (error at
most one unit in the last place), and results may differ across hosts in the final unit of
precision.
Special values are exact as specified.

Since: 0.15

#### atan — **Draft**

`float64 → float64`

Usage: `atan x`

The principal value of the inverse tangent of `x`, in radians (IEEE 754 §9.2 atan).
The result is in (-π/2, π/2); `atan ±0.0` is ±0; `atan ±∞` is ±π/2; `atan NaN` is NaN.
Results follow the host's math library: implementations should be faithfully rounded (error at
most one unit in the last place), and results may differ across hosts in the final unit of
precision.
Special values are exact as specified.

Since: 0.15

#### atan2 — **Draft**

`float64 → float64 → float64`

Usage: `atan2 y x`

The two-argument arctangent: the angle in radians, in (-π, π], from the positive x-axis to the
point (x, y), using the signs of both arguments to determine the quadrant.
Note the parameter order: the y-coordinate comes first (a documented exception to the
parameter-order conventions in index.md).
Special cases follow IEEE 754 §9.2: `atan2 ±0.0 x` is ±0 when x > 0 and ±π when x < 0;
`atan2 y 0.0` is ±π/2 for nonzero y, with the sign taken from y; `atan2 y +∞` is ±0 and
`atan2 y -∞` is ±π for finite y; `atan2 ±∞ x` is ±π/2 for finite x; `atan2 ±∞ +∞` is ±π/4 and
`atan2 ±∞ -∞` is ±3π/4; the result is NaN when either argument is NaN.
Results follow the host's math library: implementations should be faithfully rounded (error at
most one unit in the last place), and results may differ across hosts in the final unit of
precision.
Special values are exact as specified.

Since: 0.15

#### atanh — **Draft**

`float64 → float64`

Usage: `atanh x`

The principal value of the inverse hyperbolic tangent of `x` (IEEE 754 §9.2 atanh).
Defined for arguments in (-1, 1); `atanh ±1.0` is ±∞; arguments outside [-1, 1] produce NaN;
`atanh ±0.0` is ±0; `atanh NaN` is NaN.
Results follow the host's math library: implementations should be faithfully rounded (error at
most one unit in the last place), and results may differ across hosts in the final unit of
precision.
Special values are exact as specified.

Since: 0.15

#### ceiling — **Draft**

`float64 → float64`

Usage: `ceiling x`

The smallest integer value not less than `x`, returned as a float64.
This is the IEEE 754 §5.3.1 roundToIntegralTowardPositive operation: `ceiling ±0.0` is ±0;
`ceiling ±∞` is ±∞; `ceiling NaN` is NaN; the sign of a negative argument is preserved when
rounding to zero (e.g. `ceiling -0.5` is -0).
The return type is float64, not an integer type, so the result can exceed any fixed integer
range.

Since: 0.15

#### cos — **Draft**

`float64 → float64`

Usage: `cos x`

The cosine of an angle `x` in radians (IEEE 754 §9.2 cos).
The result is in [-1, 1]; `cos ±0.0` is 1; `cos ±∞` is NaN; `cos NaN` is NaN.
Results follow the host's math library: implementations should be faithfully rounded (error at
most one unit in the last place), and results may differ across hosts in the final unit of
precision.
Special values are exact as specified.

Since: 0.15

#### cosh — **Draft**

`float64 → float64`

Usage: `cosh x`

The hyperbolic cosine of `x` (IEEE 754 §9.2 cosh).
The result is in [1, +∞]; `cosh ±0.0` is 1; `cosh ±∞` is +∞; `cosh NaN` is NaN;
large-magnitude arguments overflow to +∞.
Results follow the host's math library: implementations should be faithfully rounded (error at
most one unit in the last place), and results may differ across hosts in the final unit of
precision.
Special values are exact as specified.

Since: 0.15

#### div — **Draft**

`int32 → int32 → optional<int32>`

Usage: `div x y`

Integer division, safe against division by zero.
Returns `given q`, where `q` is `x` divided by `y` rounded toward negative infinity (floor
division), when `y` is non-zero; returns `none` when `y` is 0.
Because rounding is toward negative infinity, `div -7 2` is `given -4`.
The boundary case `div -2147483648 -1`, whose mathematical result +2147483648 is not
representable in int32, wraps to `given -2147483648` (two's-complement overflow).
See `mod` for the matching floor-division remainder.
`div`, `mod`, and `rem` generalize over the `integral` class (the nine integer types);
float division is `divide` (the `fractional` class: float32, float64).

Since: 0.18 (renamed from `hydra.lib.math.maybeDiv`)

#### divide — **Draft**

`∀t. (fractional t) ⇒ t → t → t`

Usage: `divide x y`

Division, polymorphic over fractional (floating-point) types.
Returns the quotient of `x` and `y` under IEEE 754 division: the exact quotient rounded
to the nearest representable value under roundTiesToEven.
The operation is total; division by zero does not raise.
A finite nonzero `x` divided by ±0 is ±∞, with the sign given by the XOR of the operand
signs; ±0 divided by ±0 is NaN; ±∞ divided by ±∞ is NaN; ±∞ divided by a finite value is
±∞; a finite value divided by ±∞ is ±0; if either operand is NaN the result is NaN.
Note: integer division is `div` (which returns an optional, since no integer sentinel
value exists); `divide` has no integer instances.

Since: 0.18

#### e — **Draft**

`float64`

Usage: `e`

Euler's constant: the mathematical constant e ≈ 2.718281828459045, the base of the natural
logarithm, as the nearest representable float64.
Equal to `exp 1.0`.

Since: 0.15

#### even — **Draft**

`int32 → boolean`

Usage: `even x`

Test whether an int32 is even.
Returns `true` if `x` is divisible by 2 (that is, `mod x 2` is `given 0`), and `false`
otherwise.
Total on all int32 inputs, including negative values and the minimum int32.

Since: 0.15

#### exp — **Draft**

`float64 → float64`

Usage: `exp x`

The exponential function: e raised to the power `x` (IEEE 754 §9.2 exp).
`exp ±0.0` is 1; `exp -∞` is +0; `exp +∞` is +∞; `exp NaN` is NaN; large positive arguments
overflow to +∞, and large negative arguments underflow to +0.
Results follow the host's math library: implementations should be faithfully rounded (error at
most one unit in the last place), and results may differ across hosts in the final unit of
precision.
Special values are exact as specified.

Since: 0.15

#### floor — **Draft**

`float64 → float64`

Usage: `floor x`

The largest integer value not greater than `x`, returned as a float64.
This is the IEEE 754 §5.3.1 roundToIntegralTowardNegative operation: `floor ±0.0` is ±0;
`floor ±∞` is ±∞; `floor NaN` is NaN.
The return type is float64, not an integer type, so the result can exceed any fixed integer
range.

Since: 0.15

#### log — **Draft**

`float64 → float64`

Usage: `log x`

The natural (base-e) logarithm of `x` (IEEE 754 §9.2 log).
`log 1.0` is +0; `log ±0.0` is -∞; `log x` is NaN for x < 0; `log +∞` is +∞; `log NaN` is NaN.
Results follow the host's math library: implementations should be faithfully rounded (error at
most one unit in the last place), and results may differ across hosts in the final unit of
precision.
Special values are exact as specified.

Since: 0.15

#### logBase — **Draft**

`float64 → float64 → float64`

Usage: `logBase b x`

The logarithm of `x` in base `b`, equivalent to `log x` divided by `log b`.
Note the parameter order: the base comes first (a documented exception to the parameter-order
conventions in index.md).
Inherits the special-case behavior of `log` in each argument: NaN propagation, -∞ on a zero
argument, and NaN on negative arguments.
Results follow the host's math library: implementations should be faithfully rounded (error at
most one unit in the last place), and results may differ across hosts in the final unit of
precision.
Special values are exact as specified.

Since: 0.15

#### mod — **Draft**

`int32 → int32 → optional<int32>`

Usage: `mod x y`

Integer modulus, safe against division by zero.
Returns `given r`, where `r` is `x` modulo `y`, when `y` is non-zero; returns `none` when `y`
is 0.
The result satisfies the identity x = q*y + r, where `q` is the floor-division quotient
returned by `div x y`; consequently the sign of the result matches the sign of `y`
(floor-division convention), e.g. `mod -7 2` is `given 1`.
See `rem` for the truncated-division counterpart, whose result takes the sign of `x`.

Since: 0.18 (renamed from `hydra.lib.math.maybeMod`)

#### mul — **Draft**

`∀t. (numeric t) ⇒ t → t → t`

Usage: `mul x y`

Multiplication, polymorphic over numeric types.
Returns the product of `x` and `y`, with semantics determined by the type.
For fixed-width integer types (int8 through int64, uint8 through uint64), the result is the
mathematical product reduced modulo 2^N (N the bit width) and reinterpreted per the type's
signedness; arithmetic wraps silently on overflow, with no exception raised.
For `bigint`, the result is the exact mathematical product (arbitrary precision).
For floating-point types, the result is IEEE 754 multiplication: the exact product rounded to
the nearest representable value under roundTiesToEven; multiplying a zero by ±∞ (in either
order) produces NaN; multiplying any value by NaN produces NaN; the sign of the result is the
exclusive-or of the operand signs (so `mul -0.0 0.0` is -0).
Instances: the nine integer types (int8, int16, int32, int64, uint8, uint16, uint32,
uint64, bigint) and the two floating-point types (float32, float64).
`decimal` has no numeric instance: it is a presentation/interchange type, and exact division
without a precision context is not well-defined; see classes.md for the re-open trigger.

Since: 0.18 (generalized from int32-only; the `numeric` class is #566)

#### negate — **Draft**

`∀t. (numeric t) ⇒ t → t`

Usage: `negate x`

Arithmetic negation, polymorphic over numeric types.
For fixed-width integer types, the result is 0 - x reduced modulo 2^N and reinterpreted per
the type's signedness; total but not injective at the boundary of signed types, where the
minimum value negates to itself (e.g. `negate -2147483648` is `-2147483648` at int32).
For `bigint`, the result is the exact mathematical negation.
For floating-point types, this is the IEEE 754 §5.5.1 negate operation: it flips the sign bit,
so `negate 0.0` is -0, `negate -0.0` is +0, `negate ±∞` is ∓∞, and `negate NaN` is NaN;
it is a bit-level operation that raises no floating-point exception.
Instances: the nine integer types (int8, int16, int32, int64, uint8, uint16, uint32,
uint64, bigint) and the two floating-point types (float32, float64).
`decimal` has no numeric instance: it is a presentation/interchange type, and exact division
without a precision context is not well-defined; see classes.md for the re-open trigger.

Since: 0.18 (generalized from int32-only; the `numeric` class is #566)

#### odd — **Draft**

`int32 → boolean`

Usage: `odd x`

Test whether an int32 is odd.
`odd x` is `hydra.lib.logic.not (even x)`; this defining equation is the specification, and
the default implementation.
Total on all int32 inputs, including negative values and the minimum int32.

Since: 0.15

#### pi — **Draft**

`float64`

Usage: `pi`

The mathematical constant π ≈ 3.141592653589793, the ratio of a circle's circumference to its
diameter, as the nearest representable float64.

Since: 0.15

#### pow — **Draft**

`float64 → float64 → float64`

Usage: `pow x y`

Raise `x` to the power `y` (IEEE 754 §9.2 pow).
Special cases follow IEEE 754 §9.2: `pow ±0.0 y` is ±∞ for y < 0; `pow ±0.0 y` for y > 0 is
±0 if y is an odd integer and +0 otherwise; `pow 1.0 y` is 1 for every y, including NaN;
`pow x 0.0` is 1 for every x, including NaN; `pow x y` is NaN for negative finite x and
non-integer y; `pow ±∞ y` follows the mathematical limits.
Otherwise the result is x^y rounded to a representable float64.
Results follow the host's math library: implementations should be faithfully rounded (error at
most one unit in the last place), and results may differ across hosts in the final unit of
precision.
Special values are exact as specified.

Since: 0.15

#### range — **Draft**

`int32 → int32 → list<int32>`

Usage: `range a b`

Construct the inclusive integer range from `a` to `b`: the list [a, a+1, ..., b].
The range is inclusive at both ends; if a > b the result is the empty list (the range does not
count downward), and for a = b the result is the singleton [a].
The length of the result is b - a + 1 when a <= b, and 0 otherwise.

Since: 0.15

#### rem — **Draft**

`int32 → int32 → optional<int32>`

Usage: `rem x y`

Integer remainder, safe against division by zero.
Returns `given r`, where `r` is the remainder of `x` by `y` under truncated division, when `y`
is non-zero; returns `none` when `y` is 0.
The result satisfies the identity x = q*y + r, where `q` is the quotient of `x` by `y` rounded
toward zero; consequently the sign of the result matches the sign of `x` (truncated-division
convention), e.g. `rem -7 2` is `given -1`.
The boundary case `rem -2147483648 -1` is `given 0`: the quotient overflow is absorbed, and no
wrapping occurs.
See `mod` for the floor-division counterpart, whose result takes the sign of `y`.

Since: 0.18 (renamed from `hydra.lib.math.maybeRem`)

#### round — **Draft**

`float64 → float64`

Usage: `round x`

Round `x` to the nearest integer value, returned as a float64, with ties rounded to the
nearest even integer (banker's rounding).
This is the IEEE 754 §5.3.1 roundToIntegralTiesToEven operation: `round ±0.0` is ±0;
`round ±∞` is ±∞; `round NaN` is NaN.
The return type is float64, not an integer type, so the result can exceed any fixed integer
range.

Since: 0.15

#### roundFloat32 — **Draft**

`int32 → float32 → float32`

Usage: `roundFloat32 n x`

Round the float32 `x` to `n` decimal places, using round-half-to-even.
The result is the nearest float32 representation of `x` rounded to that decimal precision;
when the exact decimal-rounded value is not representable in float32 (the usual case), the
closest float32 is returned.
Special values pass through: `roundFloat32 n ±0.0` is ±0; `roundFloat32 n ±∞` is ±∞;
`roundFloat32 n NaN` is NaN.
The rounding position is defined for every int32 `n` as the nearest multiple of 10^-n
under round-half-to-even: negative `n` rounds at positions above the ones place (`n = -1`
to the nearest ten, `n = -2` to the nearest hundred), and large negative `n` degenerates
gracefully to ±0.0.

Since: 0.15

#### roundFloat64 — **Draft**

`int32 → float64 → float64`

Usage: `roundFloat64 n x`

Round the float64 `x` to `n` decimal places, using round-half-to-even.
The result is the nearest float64 representation of `x` rounded to that decimal precision;
when the exact decimal-rounded value is not representable in float64 (the usual case), the
closest float64 is returned.
Special values pass through: `roundFloat64 n ±0.0` is ±0; `roundFloat64 n ±∞` is ±∞;
`roundFloat64 n NaN` is NaN.
The rounding position is defined for every int32 `n` as the nearest multiple of 10^-n
under round-half-to-even: negative `n` rounds at positions above the ones place (`n = -1`
to the nearest ten, `n = -2` to the nearest hundred), and large negative `n` degenerates
gracefully to ±0.0.

Since: 0.15

#### signum — **Draft**

`int32 → int32`

Usage: `signum x`

The sign of an int32, as -1, 0, or 1.
Returns -1 if x < 0, 0 if x = 0, and 1 if x > 0.
Satisfies the identity `mul (abs x) (signum x)` = `x` for every int32 except the minimum
value, where the product wraps to the minimum value rather than its (unrepresentable)
negation.

Since: 0.15

#### sin — **Draft**

`float64 → float64`

Usage: `sin x`

The sine of an angle `x` in radians (IEEE 754 §9.2 sin).
The result is in [-1, 1]; `sin ±0.0` is ±0; `sin ±∞` is NaN; `sin NaN` is NaN.
Results follow the host's math library: implementations should be faithfully rounded (error at
most one unit in the last place), and results may differ across hosts in the final unit of
precision.
Special values are exact as specified.

Since: 0.15

#### sinh — **Draft**

`float64 → float64`

Usage: `sinh x`

The hyperbolic sine of `x` (IEEE 754 §9.2 sinh).
`sinh ±0.0` is ±0; `sinh ±∞` is ±∞; `sinh NaN` is NaN; large-magnitude arguments overflow
to ±∞.
Results follow the host's math library: implementations should be faithfully rounded (error at
most one unit in the last place), and results may differ across hosts in the final unit of
precision.
Special values are exact as specified.

Since: 0.15

#### sqrt — **Draft**

`float64 → float64`

Usage: `sqrt x`

The non-negative square root of `x`.
This is the IEEE 754 §5.4.1 squareRoot operation: the result is the square root of `x`
correctly rounded to the nearest representable float64 under roundTiesToEven.
Correct rounding is required here (unlike the transcendental functions), so results are
bit-identical across hosts.
`sqrt 0.0` is +0, and `sqrt -0.0` is -0 (the sign is preserved); `sqrt x` for x < 0
(including -∞) is NaN; `sqrt +∞` is +∞; `sqrt NaN` is NaN.

Since: 0.15

#### sub — **Draft**

`∀t. (numeric t) ⇒ t → t → t`

Usage: `sub x y`

Subtraction, polymorphic over numeric types.
Returns the difference of `x` and `y`, with semantics determined by the type.
For fixed-width integer types (int8 through int64, uint8 through uint64), the result is the
mathematical difference reduced modulo 2^N (N the bit width) and reinterpreted per the type's
signedness; arithmetic wraps silently on overflow, with no exception raised.
For `bigint`, the result is the exact mathematical difference (arbitrary precision).
For floating-point types, the result is IEEE 754 subtraction, defined as the addition of `x`
and the negation of `y`, rounded to the nearest representable value under roundTiesToEven;
subtracting infinities of the same sign produces NaN; any NaN operand produces NaN; the
difference of two equal finite values is +0 under the default rounding.
Instances: the nine integer types (int8, int16, int32, int64, uint8, uint16, uint32,
uint64, bigint) and the two floating-point types (float32, float64).
`decimal` has no numeric instance: it is a presentation/interchange type, and exact division
without a precision context is not well-defined; see classes.md for the re-open trigger.

Since: 0.18 (generalized from int32-only; the `numeric` class is #566)

#### tan — **Draft**

`float64 → float64`

Usage: `tan x`

The tangent of an angle `x` in radians (IEEE 754 §9.2 tan).
`tan ±0.0` is ±0; near odd multiples of π/2 the result has large magnitude but remains finite;
`tan ±∞` is NaN; `tan NaN` is NaN.
Results follow the host's math library: implementations should be faithfully rounded (error at
most one unit in the last place), and results may differ across hosts in the final unit of
precision.
Special values are exact as specified.

Since: 0.15

#### tanh — **Draft**

`float64 → float64`

Usage: `tanh x`

The hyperbolic tangent of `x` (IEEE 754 §9.2 tanh).
The result is in [-1, 1]; `tanh ±0.0` is ±0; `tanh ±∞` is ±1; `tanh NaN` is NaN.
Results follow the host's math library: implementations should be faithfully rounded (error at
most one unit in the last place), and results may differ across hosts in the final unit of
precision.
Special values are exact as specified.

Since: 0.15

#### truncate — **Draft**

`float64 → float64`

Usage: `truncate x`

Round `x` toward zero (discard the fractional part), returned as a float64.
This is the IEEE 754 §5.3.1 roundToIntegralTowardZero operation: `truncate ±0.0` is ±0;
`truncate ±∞` is ±∞; `truncate NaN` is NaN; the sign of the result matches the sign of the
argument (so `truncate -0.7` is -0, and `truncate 0.7` is +0).
The return type is float64, not an integer type, so the result can exceed any fixed integer
range.

Since: 0.15

#### addFloat64 — **Deprecated**

`float64 → float64 → float64`

Deprecated since: 0.18. Use: `hydra.lib.math.add`, which is polymorphic over numeric types.

#### max — **Deprecated**

`int32 → int32 → int32`

Deprecated since: 0.18. Use: `hydra.lib.ordering.max` (polymorphic).

#### maybeDiv — **Deprecated**

`int32 → int32 → optional<int32>`

Deprecated since: 0.18. Use: `hydra.lib.math.div`.

#### maybeMod — **Deprecated**

`int32 → int32 → optional<int32>`

Deprecated since: 0.18. Use: `hydra.lib.math.mod`.

#### maybeRem — **Deprecated**

`int32 → int32 → optional<int32>`

Deprecated since: 0.18. Use: `hydra.lib.math.rem`.

#### min — **Deprecated**

`int32 → int32 → int32`

Deprecated since: 0.18. Use: `hydra.lib.ordering.min` (polymorphic).

#### mulFloat64 — **Deprecated**

`float64 → float64 → float64`

Deprecated since: 0.18. Use: `hydra.lib.math.mul`, which is polymorphic over numeric types.

#### negateFloat64 — **Deprecated**

`float64 → float64`

Deprecated since: 0.18. Use: `hydra.lib.math.negate`, which is polymorphic over numeric types.

#### subFloat64 — **Deprecated**

`float64 → float64 → float64`

Deprecated since: 0.18. Use: `hydra.lib.math.sub`, which is polymorphic over numeric types.

<!-- maybePred/maybeSucc removed in 0.18 (zero usage; Enum vestiges) -->
