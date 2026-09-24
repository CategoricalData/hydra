// Package math implements the hydra.core.lib.math primitives.
//
// Integer primitives operate on int32; floating primitives on float64 (a few on
// float32). The stdlib "math" package is aliased as gomath to avoid clashing
// with this package's own name. maybe* primitives return the overlay
// util.Optional (None on division by zero or int32 bound overflow).
//
// Divergence from Haskell (shared by ceiling/floor/round/truncate): these return
// a float64 rather than an Integer so that NaN and ±Inf propagate per IEEE 754,
// matching C/Java/Go/Rust/JS conventions.
package math

import (
	gomath "math"

	"hydra.dev/hydra/overlay/go/util"
)

// Abs : int32 -> int32
func Abs(x any) any {
	v := x.(int32)
	if v < 0 {
		return -v
	}
	return v
}

// Acos : float64 -> float64
func Acos(x any) any { return gomath.Acos(x.(float64)) }

// Acosh : float64 -> float64
func Acosh(x any) any { return gomath.Acosh(x.(float64)) }

// Add : int32 -> int32 -> int32
func Add(x any, y any) any { return x.(int32) + y.(int32) }

// AddFloat64 : float64 -> float64 -> float64
func AddFloat64(x any, y any) any { return x.(float64) + y.(float64) }

// Asin : float64 -> float64
func Asin(x any) any { return gomath.Asin(x.(float64)) }

// Asinh : float64 -> float64
func Asinh(x any) any { return gomath.Asinh(x.(float64)) }

// Atan : float64 -> float64
func Atan(x any) any { return gomath.Atan(x.(float64)) }

// Atan2 : float64 -> float64 -> float64
func Atan2(y any, x any) any { return gomath.Atan2(y.(float64), x.(float64)) }

// Atanh : float64 -> float64
func Atanh(x any) any { return gomath.Atanh(x.(float64)) }

// Ceiling : float64 -> float64. Returns NaN/±Inf unchanged.
func Ceiling(x any) any {
	v := x.(float64)
	if gomath.IsNaN(v) || gomath.IsInf(v, 0) {
		return v
	}
	return gomath.Ceil(v)
}

// Cos : float64 -> float64
func Cos(x any) any { return gomath.Cos(x.(float64)) }

// Cosh : float64 -> float64
func Cosh(x any) any { return gomath.Cosh(x.(float64)) }

// E : float64. Euler's number.
func E() any { return gomath.E }

// Even : int32 -> boolean
func Even(x any) any { return x.(int32)%2 == 0 }

// Exp : float64 -> float64
func Exp(x any) any { return gomath.Exp(x.(float64)) }

// Floor : float64 -> float64. Returns NaN/±Inf unchanged.
func Floor(x any) any {
	v := x.(float64)
	if gomath.IsNaN(v) || gomath.IsInf(v, 0) {
		return v
	}
	return gomath.Floor(v)
}

// Log : float64 -> float64
func Log(x any) any { return gomath.Log(x.(float64)) }

// LogBase : float64 -> float64 -> float64. logBase b x = log x / log b.
func LogBase(b any, x any) any { return gomath.Log(x.(float64)) / gomath.Log(b.(float64)) }

// Max : int32 -> int32 -> int32
func Max(x any, y any) any {
	a, b := x.(int32), y.(int32)
	if a >= b {
		return a
	}
	return b
}

// Div : int32 -> int32 -> optional<int32>. Integer division (floored,
// Haskell div), None on division by zero. (Total form; #417 renamed maybeDiv->div.)
func Div(x any, y any) any {
	a, b := x.(int32), y.(int32)
	if b == 0 {
		return util.None()
	}
	return util.Some(intDiv(a, b))
}

// Divide : float64 -> float64 -> float64. The FRACTIONAL division primitive,
// distinct from Div above: hydra.core.lib.math.div is integral and total
// (optional<x>), while hydra.core.lib.math.divide is fractional and total in the
// IEEE sense (division by zero yields +/-Inf or NaN, not none).
func Divide(x any, y any) any { return x.(float64) / y.(float64) }

// Mod : int32 -> int32 -> optional<int32>. Haskell mod (sign follows
// divisor), None on division by zero. (Total form; #417 renamed maybeMod->mod.)
func Mod(x any, y any) any {
	a, b := x.(int32), y.(int32)
	if b == 0 {
		return util.None()
	}
	return util.Some(intMod(a, b))
}

// MaybePred : int32 -> optional<int32>. None at int32 minBound.
func MaybePred(x any) any {
	v := x.(int32)
	if v == gomath.MinInt32 {
		return util.None()
	}
	return util.Some(v - 1)
}

// Rem : int32 -> int32 -> optional<int32>. Haskell rem (sign follows
// dividend), None on division by zero. (Total form; #417 renamed maybeRem->rem.)
func Rem(x any, y any) any {
	a, b := x.(int32), y.(int32)
	if b == 0 {
		return util.None()
	}
	return util.Some(a % b)
}

// MaybeSucc : int32 -> optional<int32>. None at int32 maxBound.
func MaybeSucc(x any) any {
	v := x.(int32)
	if v == gomath.MaxInt32 {
		return util.None()
	}
	return util.Some(v + 1)
}

// Min : int32 -> int32 -> int32
func Min(x any, y any) any {
	a, b := x.(int32), y.(int32)
	if a <= b {
		return a
	}
	return b
}

// Mul : int32 -> int32 -> int32
func Mul(x any, y any) any { return x.(int32) * y.(int32) }

// MulFloat64 : float64 -> float64 -> float64
func MulFloat64(x any, y any) any { return x.(float64) * y.(float64) }

// Negate : int32 -> int32
func Negate(x any) any { return -x.(int32) }

// NegateFloat64 : float64 -> float64
func NegateFloat64(x any) any { return -x.(float64) }

// Odd : int32 -> boolean
func Odd(x any) any { return x.(int32)%2 != 0 }

// Pi : float64
func Pi() any { return gomath.Pi }

// Pow : float64 -> float64 -> float64
func Pow(x any, y any) any { return gomath.Pow(x.(float64), y.(float64)) }

// Range : int32 -> int32 -> list<int32>. Inclusive on both ends, matching
// Haskell's [start .. end]; empty when start > end.
func Range(start any, end any) any {
	s, e := start.(int32), end.(int32)
	out := []any{}
	for i := s; i <= e; i++ {
		out = append(out, i)
		if i == gomath.MaxInt32 {
			break // avoid int32 overflow wraparound
		}
	}
	return out
}

// Round : float64 -> float64. Round-half-to-even, NaN/±Inf unchanged.
func Round(x any) any {
	v := x.(float64)
	if gomath.IsNaN(v) || gomath.IsInf(v, 0) {
		return v
	}
	return gomath.RoundToEven(v)
}

// RoundFloat32 : int32 -> float32 -> float32. Round to n significant digits;
// NaN/±Inf and zero returned unchanged.
func RoundFloat32(n any, x any) any {
	digits := int(n.(int32))
	v := x.(float32)
	if v == 0 {
		return v
	}
	v64 := float64(v)
	if gomath.IsNaN(v64) || gomath.IsInf(v64, 0) {
		return v
	}
	factor := gomath.Pow(10, float64(digits-1-int(gomath.Floor(gomath.Log10(gomath.Abs(v64))))))
	return float32(gomath.RoundToEven(v64*factor) / factor)
}

// RoundFloat64 : int32 -> float64 -> float64. Round to n significant digits;
// NaN/±Inf and zero returned unchanged.
func RoundFloat64(n any, x any) any {
	digits := int(n.(int32))
	v := x.(float64)
	if v == 0 {
		return v
	}
	if gomath.IsNaN(v) || gomath.IsInf(v, 0) {
		return v
	}
	factor := gomath.Pow(10, float64(digits-1-int(gomath.Floor(gomath.Log10(gomath.Abs(v))))))
	return gomath.RoundToEven(v*factor) / factor
}

// Signum : int32 -> int32
func Signum(x any) any {
	v := x.(int32)
	switch {
	case v > 0:
		return int32(1)
	case v < 0:
		return int32(-1)
	default:
		return int32(0)
	}
}

// Sin : float64 -> float64
func Sin(x any) any { return gomath.Sin(x.(float64)) }

// Sinh : float64 -> float64
func Sinh(x any) any { return gomath.Sinh(x.(float64)) }

// Sqrt : float64 -> float64
func Sqrt(x any) any { return gomath.Sqrt(x.(float64)) }

// Sub : int32 -> int32 -> int32
func Sub(x any, y any) any { return x.(int32) - y.(int32) }

// SubFloat64 : float64 -> float64 -> float64
func SubFloat64(x any, y any) any { return x.(float64) - y.(float64) }

// Tan : float64 -> float64
func Tan(x any) any { return gomath.Tan(x.(float64)) }

// Tanh : float64 -> float64
func Tanh(x any) any { return gomath.Tanh(x.(float64)) }

// Truncate : float64 -> float64. Truncates toward zero; NaN/±Inf unchanged.
func Truncate(x any) any {
	v := x.(float64)
	if gomath.IsNaN(v) || gomath.IsInf(v, 0) {
		return v
	}
	return gomath.Trunc(v)
}

// intDiv implements Haskell's div: integer division rounding toward negative
// infinity.
func intDiv(a, b int32) int32 {
	q := a / b
	if (a%b != 0) && ((a < 0) != (b < 0)) {
		q--
	}
	return q
}

// intMod implements Haskell's mod: the remainder whose sign follows the divisor.
func intMod(a, b int32) int32 {
	r := a % b
	if r != 0 && ((r < 0) != (b < 0)) {
		r += b
	}
	return r
}
