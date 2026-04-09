// Note: this is an automatically generated file. Do not edit.

package sets

import "fmt"

func eq(a, b any) bool {
	switch av := a.(type) {
	case int:
		bv, ok := b.(int)
		return ok && av == bv
	case int32:
		bv, ok := b.(int32)
		return ok && av == bv
	case int64:
		bv, ok := b.(int64)
		return ok && av == bv
	case float64:
		bv, ok := b.(float64)
		return ok && av == bv
	case string:
		bv, ok := b.(string)
		return ok && av == bv
	case bool:
		bv, ok := b.(bool)
		return ok && av == bv
	default:
		return fmt.Sprintf("%v", a) == fmt.Sprintf("%v", b)
	}
}

func contains(xs []any, v any) bool {
	for _, x := range xs {
		if eq(x, v) {
			return true
		}
	}
	return false
}

// hydra.lib.sets.empty : Set a
var Empty = []any{}

// hydra.lib.sets.singleton : a -> Set a
func Singleton(x any) any {
	return []any{x}
}

// hydra.lib.sets.fromList : [a] -> Set a
func FromList(lst any) any {
	xs := lst.([]any)
	result := make([]any, 0, len(xs))
	for _, x := range xs {
		if !contains(result, x) {
			result = append(result, x)
		}
	}
	return result
}

// hydra.lib.sets.toList : Set a -> [a]
func ToList(s any) any {
	return s
}

// hydra.lib.sets.member : a -> Set a -> Boolean
func Member(x any) any {
	return func(s any) any {
		return contains(s.([]any), x)
	}
}

// hydra.lib.sets.insert : a -> Set a -> Set a
func Insert(x any) any {
	return func(s any) any {
		xs := s.([]any)
		if contains(xs, x) {
			return s
		}
		result := make([]any, 0, len(xs)+1)
		result = append(result, xs...)
		result = append(result, x)
		return result
	}
}

// hydra.lib.sets.union : Set a -> Set a -> Set a
func Union(s1 any) any {
	return func(s2 any) any {
		xs1 := s1.([]any)
		xs2 := s2.([]any)
		result := make([]any, len(xs1))
		copy(result, xs1)
		for _, x := range xs2 {
			if !contains(result, x) {
				result = append(result, x)
			}
		}
		return result
	}
}

// hydra.lib.sets.unions : [Set a] -> Set a
func Unions(sets any) any {
	xs := sets.([]any)
	var result []any
	for _, s := range xs {
		for _, x := range s.([]any) {
			if !contains(result, x) {
				result = append(result, x)
			}
		}
	}
	if result == nil {
		return []any{}
	}
	return result
}

// hydra.lib.sets.size : Set a -> Int
func Size(s any) any {
	return len(s.([]any))
}

// hydra.lib.sets.null : Set a -> Boolean
func Null(s any) any {
	return len(s.([]any)) == 0
}

// hydra.lib.sets.delete : a -> Set a -> Set a
func Delete(x any) any {
	return func(s any) any {
		xs := s.([]any)
		result := make([]any, 0, len(xs))
		for _, v := range xs {
			if !eq(v, x) {
				result = append(result, v)
			}
		}
		return result
	}
}

// hydra.lib.sets.difference : Set a -> Set a -> Set a
func Difference(s1 any) any {
	return func(s2 any) any {
		xs1 := s1.([]any)
		xs2 := s2.([]any)
		result := make([]any, 0)
		for _, x := range xs1 {
			if !contains(xs2, x) {
				result = append(result, x)
			}
		}
		return result
	}
}

// hydra.lib.sets.intersection : Set a -> Set a -> Set a
func Intersection(s1 any) any {
	return func(s2 any) any {
		xs1 := s1.([]any)
		xs2 := s2.([]any)
		result := make([]any, 0)
		for _, x := range xs1 {
			if contains(xs2, x) {
				result = append(result, x)
			}
		}
		return result
	}
}

// hydra.lib.sets.map : (a -> b) -> Set a -> Set b
func Map(f any) any {
	return func(s any) any {
		fn := f.(func(any) any)
		xs := s.([]any)
		result := make([]any, 0, len(xs))
		for _, x := range xs {
			v := fn(x)
			if !contains(result, v) {
				result = append(result, v)
			}
		}
		return result
	}
}
