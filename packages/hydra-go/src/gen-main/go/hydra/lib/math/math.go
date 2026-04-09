// Note: this is an automatically generated file. Do not edit.

package math

// hydra.lib.math.add : Int -> Int -> Int
func Add(a any) any {
	return func(b any) any {
		return a.(int) + b.(int)
	}
}

// hydra.lib.math.sub : Int -> Int -> Int
func Sub(a any) any {
	return func(b any) any {
		return a.(int) - b.(int)
	}
}

// hydra.lib.math.mul : Int -> Int -> Int
func Mul(a any) any {
	return func(b any) any {
		return a.(int) * b.(int)
	}
}

// hydra.lib.math.div : Int -> Int -> Int
func Div(a any) any {
	return func(b any) any {
		return a.(int) / b.(int)
	}
}

// hydra.lib.math.mod : Int -> Int -> Int
func Mod(a any) any {
	return func(b any) any {
		return a.(int) % b.(int)
	}
}

// hydra.lib.math.neg : Int -> Int
func Neg(a any) any {
	return -a.(int)
}

// hydra.lib.math.negate : Int -> Int
func Negate(a any) any {
	return -a.(int)
}

// hydra.lib.math.rem : Int -> Int -> Int
func Rem(a any) any {
	return func(b any) any {
		return a.(int) % b.(int)
	}
}

// hydra.lib.math.abs : Int -> Int
func Abs(a any) any {
	v := a.(int)
	if v < 0 {
		return -v
	}
	return v
}

// hydra.lib.math.signum : Int -> Int
func Signum(a any) any {
	v := a.(int)
	if v < 0 {
		return -1
	} else if v > 0 {
		return 1
	}
	return 0
}

// hydra.lib.math.max : Int -> Int -> Int
func Max(a any) any {
	return func(b any) any {
		av := a.(int)
		bv := b.(int)
		if av > bv {
			return av
		}
		return bv
	}
}

// hydra.lib.math.min : Int -> Int -> Int
func Min(a any) any {
	return func(b any) any {
		av := a.(int)
		bv := b.(int)
		if av < bv {
			return av
		}
		return bv
	}
}

// hydra.lib.math.range : Int -> Int -> [Int]
func Range(start any) any {
	return func(end any) any {
		s := start.(int)
		e := end.(int)
		if s >= e {
			return any([]any{})
		}
		r := make([]any, e-s)
		for i := s; i < e; i++ {
			r[i-s] = i
		}
		return any(r)
	}
}
