<!-- Note: this is an automatically generated file. Do not edit. -->

# hydra.core.lib.math

<!-- Note: this is an automatically generated file. Do not edit. -->

The core arithmetic operations -- add, sub, mul, negate -- are polymorphic over the numeric class: fixed-width integer types wrap in two's complement, bigint is arbitrary-precision, and floating-point types follow IEEE 754.

The sign and magnitude operations abs and signum are likewise polymorphic over the numeric class.

The integer-division and parity operations (div, mod, rem, even, odd) are polymorphic over the integral class (the fixed-width integer types plus bigint).

Only range remains int32-typed.

## hydra.core.lib.math.abs

Numeric absolute value.

`∀x.(numeric x) ⇒ x → x`

*Since: 0.15.*

Constraint-polymorphic absolute value over any type with a 'numeric' instance. The per-type semantics live in the instance: for fixed-width signed integers the result is total but not injective at the boundary (abs(minBound) = minBound, e.g. abs(-2147483648) = -2147483648 for int32, because +2147483648 is not representable); for unsigned integers and bigint the result is the input itself or its arithmetic negation, whichever is non-negative; for floating-point it clears the sign bit per IEEE 754 ੵ.5.1 (abs(ଐ) = +0, abs(NaN) is NaN).

Requires a 'numeric' type-class constraint on the argument type.

Corresponds to Haskell's abs :: Num a => a -> a.

**[HYDRA-CORE-LIB-MATH-ABS-PRESERVES-MAGNITUDE-ACROSS-TYPES] (requirement) **abs returns the non-negative magnitude of the argument, for every numeric type with a 'numeric' instance (signed/unsigned integers of every width, bigint, float32/float64).

**[HYDRA-CORE-LIB-MATH-ABS-BOUNDARY-AND-SPECIAL-VALUES] (requirement) **abs at the signed-integer minimum value wraps to itself (not representable as its positive magnitude); abs of signed zero preserves sign to +0; abs(NaN) is NaN; abs(-Infinity) is +Infinity.

## hydra.core.lib.math.acos

The arc cosine of a floating-point number.

`∀.float64 → float64`

*Since: 0.15.*

Principal value of the inverse cosine, in radians. The result is in [0, π].

For arguments outside the domain [-1, +1] (including ±∞), the result is NaN. acos(NaN) is NaN.

Corresponds to the IEEE 754 ੹.2 acos operation and to Haskell's acos :: Double -> Double.

**[HYDRA-CORE-LIB-MATH-ACOS-INVERSE-COSINE-IN-PRINCIPAL-RANGE] (requirement) **acos returns the principal value of the inverse cosine, in radians, in [0, pi], for arguments in its domain [-1, +1].

**[HYDRA-CORE-LIB-MATH-ACOS-OUT-OF-DOMAIN-IS-NAN-SPECIAL-VALUES-PROPAGATE] (requirement) **acos of an argument outside [-1, +1] (including +-Infinity) is NaN; acos(NaN) is NaN.

## hydra.core.lib.math.acosh

The hyperbolic arc cosine of a floating-point number.

`∀.float64 → float64`

*Since: 0.15.*

Principal value of the inverse hyperbolic cosine. The result is in [0, +∞).

For arguments less than 1 the result is NaN; acosh(1) = +0; acosh(+∞) = +∞; acosh(NaN) is NaN.

Corresponds to the IEEE 754 ੹.2 acosh operation and to Haskell's acosh :: Double -> Double.

**[HYDRA-CORE-LIB-MATH-ACOSH-INVERSE-HYPERBOLIC-COSINE-IN-NONNEGATIVE-RANGE] (requirement) **acosh returns the principal value of the inverse hyperbolic cosine, in [0, +Infinity), for arguments in its domain [1, +Infinity).

**[HYDRA-CORE-LIB-MATH-ACOSH-OUT-OF-DOMAIN-IS-NAN-ASYMMETRIC-AT-INFINITY] (requirement) **acosh of an argument less than 1 (including -Infinity) is NaN; acosh(+Infinity) is +Infinity; acosh(NaN) is NaN.

## hydra.core.lib.math.add

Numeric addition.

`∀x.(numeric x) ⇒ x → x → x`

*Since: 0.17.6.*

Constraint-polymorphic addition over any type with a 'numeric' instance. The per-type semantics live in the instance: two's-complement wraparound for fixed-width integers (e.g. int32 addition reduces x + y modulo 2^32 and reinterprets as signed, wrapping silently on overflow), IEEE 754 round-to-nearest for floating-point, and arbitrary precision for bigint.

Requires a 'numeric' type-class constraint on the argument type, which is the closest Hydra equivalent to Haskell's Num instance.

The operation is total. Corresponds to Haskell's (+) :: Num a => a -> a -> a.

Generalized from int32-only; the numeric class is #566.

**[HYDRA-CORE-LIB-MATH-ADD-WRAPS-OR-ROUNDS-PER-TYPE] (requirement) **add combines two values of a numeric type per that type's own arithmetic: two's-complement wraparound for fixed-width integers, arbitrary precision for bigint, IEEE 754 round-to-nearest for floating-point -- and dispatches correctly regardless of which numeric type is supplied at the call site.

**[HYDRA-CORE-LIB-MATH-ADD-BOUNDARY-WRAPAROUND-AND-IEEE-SPECIAL-VALUES] (requirement) **add wraps silently at every fixed-width integer type's boundary (no overflow exception); add of two bigints beyond int64/uint64 range remains exact; add on float64/float32 follows IEEE 754 special-value rules (opposite-sign infinities and any NaN operand yield NaN, ties round to even, -0 + -0 = -0).

## hydra.core.lib.math.addFloat64

Floating-point addition.

`∀.float64 → float64 → float64`

*Deprecated since: 0.18.*

IEEE 754 binary64 addition. The result is the value of x + y rounded to the nearest representable float64 under the roundTiesToEven rounding-direction attribute.

Adding infinities of opposite sign (+∞ + -∞ or -∞ + +∞) produces a NaN; adding any value to NaN produces a NaN. The sum of two zeros is +0, except (-0) + (-0) = -0.

The operation is total: it never raises, but it may produce NaN. Corresponds to the IEEE 754 ੵ.4.1 addition operation and to Haskell's (+) :: Double -> Double -> Double.

Deprecated since 0.18. Use: hydra.core.lib.math.add.

**[HYDRA-CORE-LIB-MATH-ADD-FLOAT64-ADDS-FLOAT64-VALUES] (requirement) **addFloat64 computes the IEEE 754 binary64 sum of its two arguments.

## hydra.core.lib.math.asin

The arc sine of a floating-point number.

`∀.float64 → float64`

*Since: 0.15.*

Principal value of the inverse sine, in radians. The result is in [-π/2, +π/2].

For arguments outside the domain [-1, +1] (including ±∞), the result is NaN; asin(ଐ) = ଐ; asin(NaN) is NaN.

Corresponds to the IEEE 754 ੹.2 asin operation and to Haskell's asin :: Double -> Double.

**[HYDRA-CORE-LIB-MATH-ASIN-INVERSE-SINE-IN-PRINCIPAL-RANGE] (requirement) **asin returns the principal value of the inverse sine, in radians, in [-pi/2, +pi/2], for arguments in its domain [-1, +1].

**[HYDRA-CORE-LIB-MATH-ASIN-OUT-OF-DOMAIN-IS-NAN-SPECIAL-VALUES-PROPAGATE] (requirement) **asin of an argument outside [-1, +1] (including +-Infinity) is NaN; asin(NaN) is NaN.

## hydra.core.lib.math.asinh

The hyperbolic arc sine of a floating-point number.

`∀.float64 → float64`

*Since: 0.15.*

Principal value of the inverse hyperbolic sine. Defined for all finite reals; asinh(ଐ) = ଐ; asinh(±∞) = ±∞; asinh(NaN) is NaN.

Corresponds to the IEEE 754 ੹.2 asinh operation and to Haskell's asinh :: Double -> Double.

**[HYDRA-CORE-LIB-MATH-ASINH-INVERSE-HYPERBOLIC-SINE-DEFINED-FOR-ALL-FINITE-REALS] (requirement) **asinh returns the principal value of the inverse hyperbolic sine, defined for every finite real argument.

**[HYDRA-CORE-LIB-MATH-ASINH-ODD-SYMMETRY-AT-INFINITY-NAN-PROPAGATES] (requirement) **asinh(+-Infinity) is +-Infinity respectively (odd symmetry preserved at the boundary); asinh(NaN) is NaN.

## hydra.core.lib.math.atan

The arc tangent of a floating-point number.

`∀.float64 → float64`

*Since: 0.15.*

Principal value of the inverse tangent, in radians. The result is in (-π/2, +π/2); atan(ଐ) = ଐ; atan(±∞) = ±π/2; atan(NaN) is NaN.

Corresponds to the IEEE 754 ੹.2 atan operation and to Haskell's atan :: Double -> Double.

**[HYDRA-CORE-LIB-MATH-ATAN-INVERSE-TANGENT-IN-PRINCIPAL-RANGE] (requirement) **atan returns the principal value of the inverse tangent, in radians, in (-pi/2, +pi/2), for any finite argument.

**[HYDRA-CORE-LIB-MATH-ATAN-SATURATES-AT-INFINITY-NAN-PROPAGATES] (requirement) **atan of +-Infinity saturates to +-pi/2 respectively (rather than NaN); atan(NaN) is NaN.

## hydra.core.lib.math.atan2

The two-argument arc tangent (atan2).

`∀.float64 → float64 → float64`

*Since: 0.15.*

atan2(y, x) returns the angle in radians, in (-π, +π], from the positive x-axis to the point (x, y), using the signs of both arguments to determine the quadrant.

Special match follow IEEE 754 ੹.2: atan2(ଐ, +x>0) = ଐ; atan2(ଐ, -x<0) = ±π; atan2(±y>0, 0) = ±π/2; atan2(±y, +∞) = ଐ; atan2(±y, -∞) = ±π; atan2(±∞, finite) = ±π/2; atan2(±∞, +∞) = ±π/4; atan2(±∞, -∞) = ଓπ/4; atan2 of any NaN argument is NaN.

Corresponds to Haskell's atan2 :: Double -> Double -> Double.

**[HYDRA-CORE-LIB-MATH-ATAN2-QUADRANT-AWARE-ARC-TANGENT-OF-TWO-ARGUMENTS] (requirement) **atan2(y, x) returns the angle in radians, in (-pi, +pi], from the positive x-axis to the point (x, y), using the signs of both arguments to determine the quadrant.

**[HYDRA-CORE-LIB-MATH-ATAN2-IEEE-SPECIAL-VALUE-MATRIX] (requirement) **atan2 follows the IEEE 754 9.2 special-value table for signed-zero, infinite, and NaN arguments in either position, including the cross-infinity combinations where this implementation returns NaN.

## hydra.core.lib.math.atanh

The hyperbolic arc tangent of a floating-point number.

`∀.float64 → float64`

*Since: 0.15.*

Principal value of the inverse hyperbolic tangent. Defined for arguments in (-1, +1); atanh(଑) = ±∞ (with division-by-zero exception in IEEE 754); arguments outside [-1, +1] produce NaN; atanh(ଐ) = ଐ; atanh(NaN) is NaN.

Corresponds to the IEEE 754 ੹.2 atanh operation and to Haskell's atanh :: Double -> Double.

**[HYDRA-CORE-LIB-MATH-ATANH-INVERSE-HYPERBOLIC-TANGENT-ON-OPEN-INTERVAL] (requirement) **atanh returns the principal value of the inverse hyperbolic tangent, defined on the open interval (-1, +1).

**[HYDRA-CORE-LIB-MATH-ATANH-BOUNDARY-IS-INFINITE-OUT-OF-DOMAIN-IS-NAN] (requirement) **atanh at the boundary (+-1) is +-Infinity respectively; atanh of an argument outside [-1, +1] (including +-Infinity) is NaN; atanh(NaN) is NaN.

## hydra.core.lib.math.ceiling

The smallest integer greater than or equal to the argument, as a float.

`∀.float64 → float64`

*Since: 0.15.*

The smallest integer value not less than the argument, returned as a float64. Equivalent to IEEE 754 ੵ.3.1 roundToIntegralTowardPositive: ceiling(ଐ) = ଐ; ceiling(±∞) = ±∞; ceiling(NaN) is NaN; the sign of a negative result is preserved when rounding to zero (e.g. ceiling(-0.5) = -0).

Note that the return type is float64, not an integer type, so the result can exceed the integer range.

Corresponds to Haskell's fromIntegral . ceiling :: Double -> Double.

**[HYDRA-CORE-LIB-MATH-CEILING-ROUNDS-TOWARD-POSITIVE-INFINITY-AS-FLOAT] (requirement) **ceiling returns the smallest integer value not less than the argument, as a float64.

**[HYDRA-CORE-LIB-MATH-CEILING-IEEE-SPECIAL-VALUES-PASS-THROUGH] (requirement) **ceiling of +-Infinity or NaN passes the value through unchanged.

## hydra.core.lib.math.cos

The cosine of a floating-point number.

`∀.float64 → float64`

*Since: 0.15.*

Cosine of an angle in radians, correctly rounded for finite arguments. The result is in [-1, +1]; cos(ଐ) = 1; cos(±∞) is NaN (with invalid-operation exception in IEEE 754); cos(NaN) is NaN.

Corresponds to the IEEE 754 ੹.2 cos operation and to Haskell's cos :: Double -> Double.

**[HYDRA-CORE-LIB-MATH-COS-COSINE-OF-ANGLE-IN-RADIANS] (requirement) **cos returns the cosine of an angle in radians, correctly rounded for finite arguments, in the range [-1, +1].

**[HYDRA-CORE-LIB-MATH-COS-UNDEFINED-AT-INFINITY-NAN-PROPAGATES] (requirement) **cos of +-Infinity is NaN; cos(NaN) is NaN.

## hydra.core.lib.math.cosh

The hyperbolic cosine of a floating-point number.

`∀.float64 → float64`

*Since: 0.15.*

Hyperbolic cosine. The result is in [1, +∞]; cosh(ଐ) = 1; cosh(±∞) = +∞; cosh(NaN) is NaN. Large-magnitude arguments overflow to +∞.

Corresponds to the IEEE 754 ੹.2 cosh operation and to Haskell's cosh :: Double -> Double.

**[HYDRA-CORE-LIB-MATH-COSH-HYPERBOLIC-COSINE-IN-RANGE-AT-LEAST-ONE] (requirement) **cosh returns the hyperbolic cosine, in [1, +Infinity), for any finite real argument.

**[HYDRA-CORE-LIB-MATH-COSH-EVEN-SYMMETRY-AT-INFINITY-NAN-PROPAGATES] (requirement) **cosh(+-Infinity) is +Infinity (even symmetry -- both signs saturate to the same value); cosh(NaN) is NaN.

## hydra.core.lib.math.div

Integral division, or Nothing if dividing by zero.

`∀x.(integral x) ⇒ x → x → optional<x>`

*Since: 0.17.6.*

Constraint-polymorphic total integer division over any type with an 'integral' instance: div(x, y) returns Just(x divided by y, rounded toward negative infinity) when y is non-zero, and Nothing when y = 0.

The division rounds toward negative infinity (floor), so for example div(-7, 2) = Just(-4).

For fixed-width signed integers, the boundary case div(minBound, -1) wraps to minBound (the two's-complement overflow), since the mathematical result is not representable at that width.

Requires an 'integral' type-class constraint on the argument type.

Corresponds to Haskell's div :: Integral a => a -> a -> a, wrapped in maybe to make the zero-divisor case total.

Renamed from hydra.lib.math.maybeDiv.

**[HYDRA-CORE-LIB-MATH-DIV-FLOORED-QUOTIENT-WHEN-DIVISOR-NONZERO] (requirement) **div returns the integer quotient of its two arguments rounded toward negative infinity (floor division), wrapped, for every integral type, when the divisor is non-zero.

**[HYDRA-CORE-LIB-MATH-DIV-NONE-ON-ZERO-DIVISOR-WRAPAROUND-AT-MIN-BOUND] (requirement) **div is none when the divisor is zero, for every integral type; at a fixed-width signed integer type's boundary case (minimum value divided by -1), the result wraps to the minimum value rather than overflowing.

## hydra.core.lib.math.divide

Floating-point division.

`∀x.(fractional x) ⇒ x → x → x`

*Since: 0.17.6.*

Constraint-polymorphic division over any type with a 'fractional' instance (float32, float64). IEEE 754 division: the result is x / y correctly rounded (roundTiesToEven) for finite y ≠ 0.

The operation is total via IEEE sentinels rather than an optional codomain: for finite nonzero x, x / ଐ = ±∞ (sign = XOR of the operand signs); ଐ / ଐ and ±∞ / ±∞ are NaN; ±∞ / finite = ±∞ (sign XOR); finite / ±∞ = ଐ (sign XOR); NaN propagates unconditionally.

Requires a 'fractional' type-class constraint on the argument type.

Corresponds to Haskell's (/) :: Fractional a => a -> a -> a.

Generalized from int32-only; the numeric class is #566.

**[HYDRA-CORE-LIB-MATH-DIVIDE-ROUNDS-TO-NEAREST-PER-IEEE754] (requirement) **divide computes the IEEE 754 correctly-rounded quotient of its two float32/float64 arguments for finite, non-zero-divisor cases.

**[HYDRA-CORE-LIB-MATH-DIVIDE-IEEE-SPECIAL-VALUES-AND-SIGNED-ZERO-DIVISION] (requirement) **divide is total via IEEE 754 sentinels rather than a partial/optional result: dividing a finite non-zero value by signed zero yields signed infinity (sign = XOR of operand signs); zero divided by zero and infinity divided by infinity are NaN; infinity divided by a finite value is infinity (sign XOR); a finite value divided by infinity is signed zero; NaN propagates unconditionally.

## hydra.core.lib.math.e

Euler's constant (the base of the natural logarithm).

`∀.float64`

*Since: 0.15.*

The mathematical constant e ≈ 2.718281828459045, the base of the natural logarithm, as the nearest representable float64. Equal to exp(1).

Corresponds to Haskell's exp 1 :: Double.

**[HYDRA-CORE-LIB-MATH-E-IS-EULERS-CONSTANT] (requirement) **e is the float64 value nearest to Euler's number, equal to exp(1).

## hydra.core.lib.math.even

Test whether a value is even.

`∀x.(integral x) ⇒ x → boolean`

*Since: 0.15.*

Constraint-polymorphic parity test over any type with an 'integral' instance: true if the argument is divisible by 2 (i.e. x mod 2 = 0), false otherwise.

Total on all integral inputs including negative numbers and signed minBound (parity is preserved by two's-complement wraparound).

Requires an 'integral' type-class constraint on the argument type.

Corresponds to Haskell's even :: Integral a => a -> Bool.

**[HYDRA-CORE-LIB-MATH-EVEN-TESTS-DIVISIBILITY-BY-TWO] (requirement) **even returns true exactly when the argument is divisible by 2, for every integral type, including at the signed-integer minimum value and for negative values (parity is preserved under two's-complement wraparound).

## hydra.core.lib.math.exp

The exponential function.

`∀.float64 → float64`

*Since: 0.15.*

The exponential function: exp(x) = e^x. exp(ଐ) = 1; exp(-∞) = +0; exp(+∞) = +∞; exp(NaN) is NaN. Large positive arguments overflow to +∞; large negative arguments underflow to +0.

Corresponds to the IEEE 754 ੹.2 exp operation and to Haskell's exp :: Double -> Double.

**[HYDRA-CORE-LIB-MATH-EXP-EXPONENTIAL-FUNCTION-BASE-E] (requirement) **exp(x) returns e^x, correctly rounded, for finite x.

**[HYDRA-CORE-LIB-MATH-EXP-SATURATES-AT-INFINITY-NAN-PROPAGATES] (requirement) **exp(-Infinity) underflows to +0; exp(+Infinity) is +Infinity; exp(NaN) is NaN.

## hydra.core.lib.math.floor

The largest integer less than or equal to the argument, as a float.

`∀.float64 → float64`

*Since: 0.15.*

The largest integer value not greater than the argument, returned as a float64. Equivalent to IEEE 754 ੵ.3.1 roundToIntegralTowardNegative: floor(ଐ) = ଐ; floor(±∞) = ±∞; floor(NaN) is NaN.

Note that the return type is float64, not an integer type, so the result can exceed the integer range.

Corresponds to Haskell's fromIntegral . floor :: Double -> Double.

**[HYDRA-CORE-LIB-MATH-FLOOR-ROUNDS-TOWARD-NEGATIVE-INFINITY-AS-FLOAT] (requirement) **floor returns the largest integer value not greater than the argument, as a float64.

**[HYDRA-CORE-LIB-MATH-FLOOR-IEEE-SPECIAL-VALUES-PASS-THROUGH] (requirement) **floor of +-Infinity or NaN passes the value through unchanged.

## hydra.core.lib.math.log

The natural logarithm.

`∀.float64 → float64`

*Since: 0.15.*

The natural (base-e) logarithm. log(1) = +0; log(+0) = log(-0) = -∞ (with division-by-zero exception in IEEE 754); log(x) for x < 0 is NaN (with invalid-operation exception); log(+∞) = +∞; log(NaN) is NaN.

Corresponds to the IEEE 754 ੹.2 log operation and to Haskell's log :: Double -> Double.

**[HYDRA-CORE-LIB-MATH-LOG-NATURAL-LOGARITHM-ON-POSITIVE-DOMAIN] (requirement) **log returns the natural (base-e) logarithm, correctly rounded, for positive finite arguments.

**[HYDRA-CORE-LIB-MATH-LOG-BOUNDARY-IS-NEGATIVE-INFINITY-NEGATIVE-IS-NAN] (requirement) **log of zero (either sign) is -Infinity; log of a negative argument is NaN; log(+Infinity) is +Infinity; log(NaN) is NaN.

## hydra.core.lib.math.logBase

Logarithm of the second argument in the base of the first.

`∀.float64 → float64 → float64`

*Since: 0.15.*

logBase(b, x) computes the logarithm of x in base b, equivalent to log(x) / log(b).

Inherits the special-case behavior of log for each argument (NaN propagation, sign of zeros, division by zero on log of a zero, NaN on negative arguments).

Corresponds to Haskell's logBase :: Double -> Double -> Double.

**[HYDRA-CORE-LIB-MATH-LOG-BASE-LOGARITHM-IN-ARBITRARY-BASE] (requirement) **logBase(b, x) computes the logarithm of x in base b, equivalent to log(x) / log(b), for positive finite b and x.

**[HYDRA-CORE-LIB-MATH-LOG-BASE-INHERITS-LOGS-SPECIAL-CASE-BEHAVIOR-IN-EACH-ARGUMENT] (requirement) **logBase inherits the special-case behavior of log independently in each argument position: a zero or negative x yields -Infinity/NaN respectively (per log's own rule), a non-positive base yields NaN, and NaN/+-Infinity in either argument propagate per log's own special-value rules.

## hydra.core.lib.math.mod

Integral modulus, or Nothing if dividing by zero.

`∀x.(integral x) ⇒ x → x → optional<x>`

*Since: 0.17.6.*

Constraint-polymorphic total integer modulus over any type with an 'integral' instance: mod(x, y) returns Just(x mod y) when y is non-zero, and Nothing when y = 0.

The result satisfies the identity x = (div(x, y) result) * y + (mod(x, y) result), so the sign of the result matches the sign of y (Knuth-style floor division). For example mod(-7, 2) = Just(1).

Requires an 'integral' type-class constraint on the argument type.

Corresponds to Haskell's mod :: Integral a => a -> a -> a, wrapped in maybe to make the zero-divisor case total.

Renamed from hydra.lib.math.maybeMod.

**[HYDRA-CORE-LIB-MATH-MOD-FLOORED-REMAINDER-SIGN-FOLLOWS-DIVISOR] (requirement) **mod returns the integer remainder of floored division, wrapped, whose sign matches the sign of the divisor (Knuth-style floor division), for every integral type, when the divisor is non-zero.

**[HYDRA-CORE-LIB-MATH-MOD-NONE-ON-ZERO-DIVISOR] (requirement) **mod is none when the divisor is zero, for every integral type; at a fixed-width signed integer type's boundary case (minimum value divided by -1), the result is zero (no overflow -- the quotient's overflow is absorbed by div, not mod).

## hydra.core.lib.math.mul

Numeric multiplication.

`∀x.(numeric x) ⇒ x → x → x`

*Since: 0.17.6.*

Constraint-polymorphic multiplication over any type with a 'numeric' instance. The per-type semantics live in the instance: two's-complement wraparound for fixed-width integers (e.g. int32 multiplication reduces x * y modulo 2^32 and reinterprets as signed, wrapping silently on overflow), IEEE 754 round-to-nearest for floating-point, and arbitrary precision for bigint.

Requires a 'numeric' type-class constraint on the argument type.

The operation is total. Corresponds to Haskell's (*) :: Num a => a -> a -> a.

Generalized from int32-only; the numeric class is #566.

**[HYDRA-CORE-LIB-MATH-MUL-WRAPS-OR-ROUNDS-PER-TYPE] (requirement) **mul computes the product of two values of a numeric type per that type's own arithmetic: two's-complement wraparound for fixed-width integers, arbitrary precision for bigint, IEEE 754 round-to-nearest for floating-point -- and dispatches correctly regardless of which numeric type is supplied at the call site.

**[HYDRA-CORE-LIB-MATH-MUL-BOUNDARY-WRAPAROUND-AND-IEEE-SPECIAL-VALUES] (requirement) **mul wraps silently at every fixed-width integer type's boundary; mul on bigint beyond int64/uint64 range remains exact; mul on float64/float32 follows IEEE 754 special-value rules (zero times infinity is NaN, any NaN operand yields NaN, sign of the result is the XOR of operand signs, results underflow to signed zero or overflow to signed infinity when out of representable range).

## hydra.core.lib.math.mulFloat64

Floating-point multiplication.

`∀.float64 → float64 → float64`

*Deprecated since: 0.18.*

IEEE 754 binary64 multiplication. The result is the value of x * y rounded to the nearest representable float64 under the roundTiesToEven rounding-direction attribute.

Multiplying 0 by ±∞ (in either order) produces a NaN (with invalid-operation exception in IEEE 754); multiplying any value by NaN produces a NaN. The result's sign is the XOR of the operand signs (ଐ * ଐ = ଐ accordingly).

The operation is total: it never raises, but it may produce NaN or ±∞. Corresponds to the IEEE 754 ੵ.4.1 multiplication operation and to Haskell's (*) :: Double -> Double -> Double.

Deprecated since 0.18. Use: hydra.core.lib.math.mul.

**[HYDRA-CORE-LIB-MATH-MUL-FLOAT64-MULTIPLIES-FLOAT64-VALUES] (requirement) **mulFloat64 computes the IEEE 754 binary64 product of its two arguments.

## hydra.core.lib.math.negate

Numeric negation.

`∀x.(numeric x) ⇒ x → x`

*Since: 0.17.6.*

Constraint-polymorphic arithmetic negation over any type with a 'numeric' instance. The per-type semantics live in the instance: for fixed-width integers the result is 0 - x reduced modulo 2^32 and reinterpreted as signed (total but not injective at the boundary: negate(minBound) = minBound, because +2147483648 is not representable in int32); for floating-point it flips the sign bit per IEEE 754 ੵ.5.1.

Requires a 'numeric' type-class constraint on the argument type.

Corresponds to Haskell's negate :: Num a => a -> a.

Generalized from int32-only; the numeric class is #566.

**[HYDRA-CORE-LIB-MATH-NEGATE-ARITHMETIC-NEGATION-PER-TYPE] (requirement) **negate returns the arithmetic inverse of its argument per the argument's numeric type: two's-complement negation for fixed-width integers, sign-bit flip for floating-point, arbitrary precision for bigint -- and dispatches correctly regardless of which numeric type is supplied at the call site.

**[HYDRA-CORE-LIB-MATH-NEGATE-BOUNDARY-AND-IEEE-SPECIAL-VALUES] (requirement) **negate at a signed fixed-width integer type's minimum value wraps to itself (not representable as its positive magnitude); negate on bigint beyond int64 range remains exact; negate on float64/float32 flips the sign of zero and of infinity, and preserves NaN.

## hydra.core.lib.math.negateFloat64

Negate a floating-point number.

`∀.float64 → float64`

*Deprecated since: 0.18.*

Sign reversal of a float64. Equivalent to IEEE 754 ੵ.5.1 negate: flips the sign bit, so negate(ଐ) = ଐ (sign flips), negate(±∞) = ±∞ (sign flips), and negate(NaN) is a NaN (sign may flip; payload preserved).

This is a bit-level operation that does not raise any floating-point exception. Corresponds to Haskell's negate :: Double -> Double.

Deprecated since 0.18. Use: hydra.core.lib.math.negate.

**[HYDRA-CORE-LIB-MATH-NEGATE-FLOAT64-NEGATES-FLOAT64-VALUES] (requirement) **negateFloat64 flips the sign of its float64 argument, including zero.

## hydra.core.lib.math.odd

Test whether a value is odd.

`∀x.(integral x) ⇒ x → boolean`

*Since: 0.15.*

Constraint-polymorphic parity test over any type with an 'integral' instance: true if the argument is not divisible by 2 (i.e. x mod 2 ≠ 0), false otherwise.

Total on all integral inputs including negative numbers and signed minBound. Requires an 'integral' type-class constraint on the argument type.

Corresponds to Haskell's odd :: Integral a => a -> Bool.

**[HYDRA-CORE-LIB-MATH-ODD-TESTS-NON-DIVISIBILITY-BY-TWO] (requirement) **odd returns true exactly when the argument is not divisible by 2, for every integral type, including at the signed-integer minimum value and for negative values.

## hydra.core.lib.math.pi

The mathematical constant pi.

`∀.float64`

*Since: 0.15.*

The mathematical constant π ≈ 3.141592653589793, the ratio of a circle's circumference to its diameter, as the nearest representable float64.

Corresponds to Haskell's pi :: Double.

**[HYDRA-CORE-LIB-MATH-PI-IS-PI-CONSTANT] (requirement) **pi is the float64 value nearest to the mathematical constant pi.

## hydra.core.lib.math.pow

Raise the first argument to the power of the second.

`∀.float64 → float64 → float64`

*Since: 0.15.*

pow(x, y) = x^y.

Follows the IEEE 754 ੹.2 pow operation: pow(ଐ, y) for y < 0 is ±∞ (with division-by-zero exception); pow(ଐ, y) for y > 0 is ଐ if y is an odd integer, else +0; pow(1, y) = 1 for any y including NaN; pow(x, ଐ) = 1 for any x including NaN; pow(x, y) for negative x and non-integer y is NaN (with invalid-operation exception); pow(±∞, y) follows the limits in the usual way. Otherwise the result is x^y rounded to the nearest representable float64.

Corresponds to Haskell's (**) :: Double -> Double -> Double.

**[HYDRA-CORE-LIB-MATH-POW-EXPONENTIATION-FOR-FINITE-ARGUMENTS] (requirement) **pow(x, y) = x^y, correctly rounded, for finite x and y within pow's defined domain.

**[HYDRA-CORE-LIB-MATH-POW-IEEE-SPECIAL-VALUE-MATRIX] (requirement) **pow follows the IEEE 754 9.2 special-value table: 0^0 = 1; 0 raised to a negative exponent is +Infinity; a negative base raised to a non-integer exponent is NaN; 1 raised to any exponent (including NaN) is 1; any base raised to an exponent of 0 (including NaN) is 1; and NaN/+-Infinity in either argument otherwise follow the documented limit behavior.

## hydra.core.lib.math.range

Construct the half-open integer range from the first argument up to (but excluding) the second.

`∀.int32 → int32 → list<int32>`

*Since: 0.15.*

range(a, b) returns the list [a, a+1, ..., b-1]. The range is inclusive of the lower bound and exclusive of the upper bound; if a >= b the result is the empty list (i.e. the range does not count downward, and an empty range is the natural result of equal bounds). The length of the result is max(0, b - a).

Does not directly correspond to Haskell's enumFromTo :: Int32 -> Int32 -> [Int32] (which is inclusive of both bounds, i.e. [a..b]); range(a, b) is equivalent to [a .. b-1]. This matches the half-open convention of Python's range, Rust's a..b, Go, and Java's IntStream.range.

**[HYDRA-CORE-LIB-MATH-RANGE-GENERATES-HALF-OPEN-ASCENDING-SEQUENCE] (requirement) **range(a, b) returns the list [a, a+1, ..., b-1] -- inclusive of the lower bound and exclusive of the upper bound, with length max(0, b-a).

**[HYDRA-CORE-LIB-MATH-RANGE-EMPTY-WHEN-BOUNDS-EQUAL-OR-DESCENDING] (requirement) **range(a, b) is the empty list whenever a >= b, including equal bounds and descending bounds -- the range never counts downward.

## hydra.core.lib.math.rem

Integral remainder, or Nothing if dividing by zero.

`∀x.(integral x) ⇒ x → x → optional<x>`

*Since: 0.17.6.*

Constraint-polymorphic total integer remainder over any type with an 'integral' instance: rem(x, y) returns Just(x rem y) when y is non-zero, and Nothing when y = 0.

The result satisfies x = (truncate(x / y)) * y + (rem(x, y) result), so the sign of the result matches the sign of x (truncated division, C-style remainder). For example rem(-7, 2) = Just(-1).

For fixed-width signed integers, the boundary case rem(minBound, -1) is 0 (no overflow, since the quotient overflow is absorbed by div, not rem).

Requires an 'integral' type-class constraint on the argument type.

Corresponds to Haskell's rem :: Integral a => a -> a -> a, wrapped in maybe to make the zero-divisor case total.

Renamed from hydra.lib.math.maybeRem.

**[HYDRA-CORE-LIB-MATH-REM-TRUNCATED-REMAINDER-SIGN-FOLLOWS-DIVIDEND] (requirement) **rem returns the integer remainder of truncated division (toward zero), wrapped, whose sign matches the sign of the dividend (C-style remainder), for every integral type, when the divisor is non-zero.

**[HYDRA-CORE-LIB-MATH-REM-NONE-ON-ZERO-DIVISOR-NO-OVERFLOW-AT-MIN-BOUND] (requirement) **rem is none when the divisor is zero, for every integral type; at a fixed-width signed integer type's boundary case (minimum value divided by -1), the result is zero (no overflow -- the quotient's overflow is absorbed by div, not rem).

## hydra.core.lib.math.round

Round a floating-point number to the nearest integer-valued float.

`∀.float64 → float64`

*Since: 0.15.*

Round to the nearest integer value, returned as a float64, with ties rounded to the nearest even integer (banker's rounding). Equivalent to IEEE 754 ੵ.3.1 roundToIntegralTiesToEven: round(ଐ) = ଐ; round(±∞) = ±∞; round(NaN) is NaN.

Note that the return type is float64, not an integer type, so the result can exceed the integer range.

Corresponds to Haskell's fromIntegral . round :: Double -> Double.

**[HYDRA-CORE-LIB-MATH-ROUND-ROUNDS-TO-NEAREST-TIES-TO-EVEN-AS-FLOAT] (requirement) **round returns the nearest integer value as a float64, with ties rounded to the nearest even integer (banker's rounding).

**[HYDRA-CORE-LIB-MATH-ROUND-IEEE-SPECIAL-VALUES-PASS-THROUGH] (requirement) **round of +-Infinity or NaN passes the value through unchanged.

## hydra.core.lib.math.roundFloat32

Round a float32 to the given number of decimal places.

`∀.int32 → float32 → float32`

*Since: 0.15.*

roundFloat32(n, x) rounds the float32 value x to n decimal places using round-half-to-even. The result is the nearest float32 representation of x rounded to that decimal precision; if the exact decimal-rounded value is not representable in float32 (the usual case), the closest float32 is returned.

Special values pass through: roundFloat32(n, ଐ) = ଐ; roundFloat32(n, ±∞) = ±∞; roundFloat32(n, NaN) is NaN.

Negative n is supported in principle (rounding to powers of ten above 1); host implementations may differ on out-of-range n.

**[HYDRA-CORE-LIB-MATH-ROUND-FLOAT32-ROUNDS-TO-GIVEN-DECIMAL-PLACES-HALF-TO-EVEN] (requirement) **roundFloat32(n, x) rounds the float32 value x to n decimal places using round-half-to-even, returning the nearest representable float32.

**[HYDRA-CORE-LIB-MATH-ROUND-FLOAT32-SPECIAL-VALUES-PASS-THROUGH-UNROUNDED] (requirement) **roundFloat32 passes NaN and +-Infinity through unrounded, regardless of the requested decimal-place count.

## hydra.core.lib.math.roundFloat64

Round a float64 to the given number of decimal places.

`∀.int32 → float64 → float64`

*Since: 0.15.*

roundFloat64(n, x) rounds the float64 value x to n decimal places using round-half-to-even. The result is the nearest float64 representation of x rounded to that decimal precision; if the exact decimal-rounded value is not representable in float64 (the usual case), the closest float64 is returned.

Special values pass through: roundFloat64(n, ଐ) = ଐ; roundFloat64(n, ±∞) = ±∞; roundFloat64(n, NaN) is NaN.

Negative n is supported in principle (rounding to powers of ten above 1); host implementations may differ on out-of-range n.

**[HYDRA-CORE-LIB-MATH-ROUND-FLOAT64-ROUNDS-TO-GIVEN-DECIMAL-PLACES-HALF-TO-EVEN] (requirement) **roundFloat64(n, x) rounds the float64 value x to n decimal places using round-half-to-even, returning the nearest representable float64.

**[HYDRA-CORE-LIB-MATH-ROUND-FLOAT64-SPECIAL-VALUES-PASS-THROUGH-UNROUNDED] (requirement) **roundFloat64 passes NaN and +-Infinity through unrounded, regardless of the requested decimal-place count.

## hydra.core.lib.math.signum

Numeric sign.

`∀x.(numeric x) ⇒ x → x`

*Since: 0.15.*

Constraint-polymorphic sign function over any type with a 'numeric' instance. For integer types, signum(x) returns -1 if x < 0, 0 if x = 0, and 1 if x > 0 (same width as the argument); unsigned integers return only 0 or 1. The function satisfies abs(x) * signum(x) = x for all integer instances except signed minBound (where the identity fails because +maxBound+1 is not representable).

For floating-point, signum(x) returns ଑.0 for nonzero finite or infinite x (per IEEE 754 ੵ.5.1); signum(ଐ) = ଐ (the sign of the input zero is preserved, not collapsed to a single zero); signum(NaN) is NaN. A naive three-branch implementation (positive/negative/else- zero) gets both of these float match wrong and must not be used.

Requires a 'numeric' type-class constraint on the argument type.

Corresponds to Haskell's signum :: Num a => a -> a.

**[HYDRA-CORE-LIB-MATH-SIGNUM-SIGN-PER-TYPE] (requirement) **signum returns the sign of its argument per the argument's numeric type: -1/0/1 (same width) for integers, +-1.0 for nonzero finite or infinite floats -- satisfying abs(x) * signum(x) = x for every integer instance except signed minBound.

**[HYDRA-CORE-LIB-MATH-SIGNUM-PRESERVES-SIGNED-ZERO-AND-NAN] (requirement) **signum of signed zero returns that same signed zero (not collapsed to a single zero), and signum(NaN) is NaN -- a naive positive/negative/else-zero branch implementation gets both of these cases wrong and must not be used.

## hydra.core.lib.math.sin

The sine of a floating-point number.

`∀.float64 → float64`

*Since: 0.15.*

Sine of an angle in radians, correctly rounded for finite arguments. The result is in [-1, +1]; sin(ଐ) = ଐ; sin(±∞) is NaN (with invalid-operation exception in IEEE 754); sin(NaN) is NaN.

Corresponds to the IEEE 754 ੹.2 sin operation and to Haskell's sin :: Double -> Double.

**[HYDRA-CORE-LIB-MATH-SIN-SINE-OF-ANGLE-IN-RADIANS] (requirement) **sin returns the sine of an angle in radians, correctly rounded for finite arguments, in the range [-1, +1].

**[HYDRA-CORE-LIB-MATH-SIN-UNDEFINED-AT-INFINITY-NAN-PROPAGATES] (requirement) **sin of +-Infinity is NaN (not a finite oscillation); sin(NaN) is NaN.

## hydra.core.lib.math.sinh

The hyperbolic sine of a floating-point number.

`∀.float64 → float64`

*Since: 0.15.*

Hyperbolic sine. sinh(ଐ) = ଐ; sinh(±∞) = ±∞; sinh(NaN) is NaN. Large-magnitude arguments overflow to ±∞.

Corresponds to the IEEE 754 ੹.2 sinh operation and to Haskell's sinh :: Double -> Double.

**[HYDRA-CORE-LIB-MATH-SINH-HYPERBOLIC-SINE-DEFINED-FOR-ALL-FINITE-REALS] (requirement) **sinh returns the hyperbolic sine, defined for every finite real argument.

**[HYDRA-CORE-LIB-MATH-SINH-ODD-SYMMETRY-AT-INFINITY-NAN-PROPAGATES] (requirement) **sinh(+-Infinity) is +-Infinity respectively; sinh(NaN) is NaN.

## hydra.core.lib.math.sqrt

The non-negative square root of a floating-point number.

`∀.float64 → float64`

*Since: 0.15.*

IEEE 754 binary64 square root. The result is the value of √x correctly rounded to the nearest representable float64 under roundTiesToEven.

sqrt(+0) = +0; sqrt(-0) = -0 (sign preserved); sqrt(x) for x < 0 (including -∞) is NaN (with invalid-operation exception); sqrt(+∞) = +∞; sqrt(NaN) is NaN.

Corresponds to the IEEE 754 ੵ.4.1 squareRoot operation and to Haskell's sqrt :: Double -> Double.

**[HYDRA-CORE-LIB-MATH-SQRT-NONNEGATIVE-SQUARE-ROOT-ON-NONNEGATIVE-DOMAIN] (requirement) **sqrt returns the non-negative, correctly-rounded square root for non-negative finite arguments.

**[HYDRA-CORE-LIB-MATH-SQRT-PRESERVES-SIGN-OF-ZERO-NEGATIVE-IS-NAN] (requirement) **sqrt(-0) is -0 (sign preserved, distinct from sqrt(+0) = +0); sqrt of a negative argument (including -Infinity) is NaN; sqrt(+Infinity) is +Infinity; sqrt(NaN) is NaN.

## hydra.core.lib.math.sub

Numeric subtraction.

`∀x.(numeric x) ⇒ x → x → x`

*Since: 0.17.6.*

Constraint-polymorphic subtraction over any type with a 'numeric' instance. The per-type semantics live in the instance: two's-complement wraparound for fixed-width integers (e.g. int32 subtraction reduces x - y modulo 2^32 and reinterprets as signed, wrapping silently on overflow), IEEE 754 round-to-nearest for floating-point, and arbitrary precision for bigint.

Requires a 'numeric' type-class constraint on the argument type.

The operation is total. Corresponds to Haskell's (-) :: Num a => a -> a -> a.

Generalized from int32-only; the numeric class is #566.

**[HYDRA-CORE-LIB-MATH-SUB-WRAPS-OR-ROUNDS-PER-TYPE] (requirement) **sub computes the difference of two values of a numeric type per that type's own arithmetic: two's-complement wraparound for fixed-width integers, arbitrary precision for bigint, IEEE 754 round-to-nearest for floating-point -- and dispatches correctly regardless of which numeric type is supplied at the call site.

**[HYDRA-CORE-LIB-MATH-SUB-BOUNDARY-WRAPAROUND-AND-IEEE-SPECIAL-VALUES] (requirement) **sub wraps silently at every fixed-width integer type's boundary; sub on bigint beyond int64/uint64 range remains exact; sub on float64/float32 follows IEEE 754 special-value rules (subtracting same-sign infinities or any NaN operand yields NaN, the difference of two equal finite values is +0, ties round to even).

## hydra.core.lib.math.subFloat64

Floating-point subtraction.

`∀.float64 → float64 → float64`

*Deprecated since: 0.18.*

IEEE 754 binary64 subtraction, defined as x + (-y). The result is correctly rounded to the nearest representable float64 under roundTiesToEven.

Subtracting infinities of the same sign (+∞ - +∞ or -∞ - -∞) produces a NaN; subtracting any value involving NaN produces a NaN. The difference of two equal finite values is +0 (or -0 under round-toward-negative, which is not the default).

The operation is total: it never raises, but it may produce NaN. Corresponds to the IEEE 754 ੵ.4.1 subtraction operation and to Haskell's (-) :: Double -> Double -> Double.

Deprecated since 0.18. Use: hydra.core.lib.math.sub.

**[HYDRA-CORE-LIB-MATH-SUB-FLOAT64-SUBTRACTS-FLOAT64-VALUES] (requirement) **subFloat64 computes the IEEE 754 binary64 difference of its two arguments.

## hydra.core.lib.math.tan

The tangent of a floating-point number.

`∀.float64 → float64`

*Since: 0.15.*

Tangent of an angle in radians, correctly rounded for finite arguments. tan(ଐ) = ଐ; near odd multiples of π/2 the result has large magnitude but remains finite (no exception is raised); tan(±∞) is NaN (with invalid-operation exception in IEEE 754); tan(NaN) is NaN.

Corresponds to the IEEE 754 ੹.2 tan operation and to Haskell's tan :: Double -> Double.

**[HYDRA-CORE-LIB-MATH-TAN-TANGENT-OF-ANGLE-IN-RADIANS] (requirement) **tan returns the tangent of an angle in radians, correctly rounded for finite arguments; near odd multiples of pi/2 the result has large magnitude but remains finite (no exception is raised).

**[HYDRA-CORE-LIB-MATH-TAN-UNDEFINED-AT-INFINITY-NAN-PROPAGATES] (requirement) **tan of +-Infinity is NaN; tan(NaN) is NaN.

## hydra.core.lib.math.tanh

The hyperbolic tangent of a floating-point number.

`∀.float64 → float64`

*Since: 0.15.*

Hyperbolic tangent. The result is in [-1, +1]; tanh(ଐ) = ଐ; tanh(±∞) = ଑; tanh(NaN) is NaN.

Corresponds to the IEEE 754 ੹.2 tanh operation and to Haskell's tanh :: Double -> Double.

**[HYDRA-CORE-LIB-MATH-TANH-HYPERBOLIC-TANGENT-IN-RANGE-NEG-ONE-TO-ONE] (requirement) **tanh returns the hyperbolic tangent, in (-1, +1), for any finite real argument.

**[HYDRA-CORE-LIB-MATH-TANH-SATURATES-AT-INFINITY-NAN-PROPAGATES] (requirement) **tanh(+-Infinity) saturates to +-1 respectively; tanh(NaN) is NaN.

## hydra.core.lib.math.truncate

Truncate a floating-point number toward zero, as a float.

`∀.float64 → float64`

*Since: 0.15.*

Round toward zero (truncate the fractional part), returned as a float64. Equivalent to IEEE 754 ੵ.3.1 roundToIntegralTowardZero: truncate(ଐ) = ଐ; truncate(±∞) = ±∞; truncate(NaN) is NaN; sign of the result matches the sign of the argument (so truncate(-0.7) = -0, truncate(+0.7) = +0).

Note that the return type is float64, not an integer type, so the result can exceed the integer range.

Corresponds to Haskell's fromIntegral . truncate :: Double -> Double.

**[HYDRA-CORE-LIB-MATH-TRUNCATE-ROUNDS-TOWARD-ZERO-AS-FLOAT] (requirement) **truncate returns the argument with its fractional part discarded (rounded toward zero), as a float64, preserving the sign of the argument.

**[HYDRA-CORE-LIB-MATH-TRUNCATE-IEEE-SPECIAL-VALUES-PASS-THROUGH] (requirement) **truncate of +-Infinity or NaN passes the value through unchanged.
