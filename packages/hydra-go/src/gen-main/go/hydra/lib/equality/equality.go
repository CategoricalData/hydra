// Note: this is an automatically generated file. Do not edit.

package equality

import "fmt"

func cmp(a, b any) int {
	switch av := a.(type) {
	case int:
		bv := b.(int)
		if av < bv {
			return -1
		} else if av > bv {
			return 1
		}
		return 0
	case int32:
		bv := b.(int32)
		if av < bv {
			return -1
		} else if av > bv {
			return 1
		}
		return 0
	case int64:
		bv := b.(int64)
		if av < bv {
			return -1
		} else if av > bv {
			return 1
		}
		return 0
	case float32:
		bv := b.(float32)
		if av < bv {
			return -1
		} else if av > bv {
			return 1
		}
		return 0
	case float64:
		bv := b.(float64)
		if av < bv {
			return -1
		} else if av > bv {
			return 1
		}
		return 0
	case string:
		bv := b.(string)
		if av < bv {
			return -1
		} else if av > bv {
			return 1
		}
		return 0
	case bool:
		bv := b.(bool)
		if av == bv {
			return 0
		} else if !av {
			return -1
		}
		return 1
	default:
		sa := fmt.Sprintf("%v", a)
		sb := fmt.Sprintf("%v", b)
		if sa < sb {
			return -1
		} else if sa > sb {
			return 1
		}
		return 0
	}
}

// hydra.lib.equality.equal : a -> a -> Boolean
func Equal(a any) any {
	return func(b any) any {
		return cmp(a, b) == 0
	}
}

// hydra.lib.equality.identity : a -> a
func Identity(a any) any {
	return a
}

// hydra.lib.equality.gt : a -> a -> Boolean
func Gt(a any) any {
	return func(b any) any {
		return cmp(a, b) > 0
	}
}

// hydra.lib.equality.gte : a -> a -> Boolean
func Gte(a any) any {
	return func(b any) any {
		return cmp(a, b) >= 0
	}
}

// hydra.lib.equality.lt : a -> a -> Boolean
func Lt(a any) any {
	return func(b any) any {
		return cmp(a, b) < 0
	}
}

// hydra.lib.equality.lte : a -> a -> Boolean
func Lte(a any) any {
	return func(b any) any {
		return cmp(a, b) <= 0
	}
}

// hydra.lib.equality.max : a -> a -> a
func Max(a any) any {
	return func(b any) any {
		if cmp(a, b) >= 0 { return a }
		return b
	}
}

// hydra.lib.equality.min : a -> a -> a
func Min(a any) any {
	return func(b any) any {
		if cmp(a, b) <= 0 { return a }
		return b
	}
}

// hydra.lib.equality.compare : a -> a -> Comparison
func Compare(a any) any {
	return func(b any) any {
		c := cmp(a, b)
		if c < 0 { return "LessThan" }
		if c > 0 { return "GreaterThan" }
		return "EqualTo"
	}
}
