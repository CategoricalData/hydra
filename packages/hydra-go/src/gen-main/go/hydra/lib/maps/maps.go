// Note: this is an automatically generated file. Do not edit.

package maps

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

func find(m []any, key any) (any, int) {
	for i, entry := range m {
		pair := entry.([2]any)
		if eq(pair[0], key) {
			return pair[1], i
		}
	}
	return nil, -1
}

// hydra.lib.maps.empty : Map k v
var Empty = []any{}

// hydra.lib.maps.singleton : k -> v -> Map k v
func Singleton(k any) any {
	return func(v any) any {
		return []any{[2]any{k, v}}
	}
}

// hydra.lib.maps.fromList : [(k, v)] -> Map k v
func FromList(lst any) any {
	pairs := lst.([]any)
	result := make([]any, 0, len(pairs))
	for _, p := range pairs {
		pair := p.([2]any)
		key := pair[0]
		_, idx := find(result, key)
		if idx >= 0 {
			result[idx] = p
		} else {
			result = append(result, p)
		}
	}
	return result
}

// hydra.lib.maps.toList : Map k v -> [(k, v)]
func ToList(m any) any {
	return m
}

// hydra.lib.maps.lookup : k -> Map k v -> Maybe v
func Lookup(k any) any {
	return func(m any) any {
		v, idx := find(m.([]any), k)
		if idx < 0 {
			return nil
		}
		val := any(v)
		return &val
	}
}

// hydra.lib.maps.insert : k -> v -> Map k v -> Map k v
func Insert(k any) any {
	return func(v any) any {
		return func(m any) any {
			xs := m.([]any)
			_, idx := find(xs, k)
			entry := [2]any{k, v}
			if idx >= 0 {
				result := make([]any, len(xs))
				copy(result, xs)
				result[idx] = entry
				return result
			}
			result := make([]any, 0, len(xs)+1)
			result = append(result, xs...)
			result = append(result, entry)
			return result
		}
	}
}

// hydra.lib.maps.member : k -> Map k v -> Boolean
func Member(k any) any {
	return func(m any) any {
		_, idx := find(m.([]any), k)
		return idx >= 0
	}
}

// hydra.lib.maps.keys : Map k v -> [k]
func Keys(m any) any {
	xs := m.([]any)
	result := make([]any, len(xs))
	for i, entry := range xs {
		result[i] = entry.([2]any)[0]
	}
	return result
}

// hydra.lib.maps.size : Map k v -> Int
func Size(m any) any {
	return len(m.([]any))
}

// hydra.lib.maps.map : (a -> b) -> Map k a -> Map k b
func Map(f any) any {
	return func(m any) any {
		fn := f.(func(any) any)
		xs := m.([]any)
		result := make([]any, len(xs))
		for i, entry := range xs {
			pair := entry.([2]any)
			result[i] = [2]any{pair[0], fn(pair[1])}
		}
		return result
	}
}

// hydra.lib.maps.delete : k -> Map k v -> Map k v
func Delete(k any) any {
	return func(m any) any {
		xs := m.([]any)
		result := make([]any, 0, len(xs))
		for _, entry := range xs {
			if !eq(entry.([2]any)[0], k) {
				result = append(result, entry)
			}
		}
		return result
	}
}

// hydra.lib.maps.mapKeys : (k1 -> k2) -> Map k1 v -> Map k2 v
func MapKeys(f any) any {
	return func(m any) any {
		fn := f.(func(any) any)
		xs := m.([]any)
		result := make([]any, len(xs))
		for i, entry := range xs {
			pair := entry.([2]any)
			result[i] = [2]any{fn(pair[0]), pair[1]}
		}
		return result
	}
}

// hydra.lib.maps.union : Map k v -> Map k v -> Map k v
func Union(m1 any) any {
	return func(m2 any) any {
		xs1 := m1.([]any)
		xs2 := m2.([]any)
		result := make([]any, len(xs1))
		copy(result, xs1)
		for _, entry := range xs2 {
			key := entry.([2]any)[0]
			_, idx := find(result, key)
			if idx < 0 {
				result = append(result, entry)
			}
		}
		return result
	}
}

// hydra.lib.maps.null : Map k v -> Boolean
func Null(m any) any {
	return len(m.([]any)) == 0
}

// hydra.lib.maps.elems : Map k v -> [v]
func Elems(m any) any {
	xs := m.([]any)
	result := make([]any, len(xs))
	for i, entry := range xs {
		result[i] = entry.([2]any)[1]
	}
	return result
}

// hydra.lib.maps.filter : (v -> Boolean) -> Map k v -> Map k v
func Filter(f any) any {
	return func(m any) any {
		fn := f.(func(any) any)
		xs := m.([]any)
		result := make([]any, 0)
		for _, entry := range xs {
			if fn(entry.([2]any)[1]).(bool) {
				result = append(result, entry)
			}
		}
		return result
	}
}

// hydra.lib.maps.filterWithKey : (k -> v -> Boolean) -> Map k v -> Map k v
func FilterWithKey(f any) any {
	return func(m any) any {
		fn := f.(func(any) any)
		xs := m.([]any)
		result := make([]any, 0)
		for _, entry := range xs {
			pair := entry.([2]any)
			if fn(pair[0]).(func(any) any)(pair[1]).(bool) {
				result = append(result, entry)
			}
		}
		return result
	}
}

// hydra.lib.maps.findWithDefault : v -> k -> Map k v -> v
func FindWithDefault(def any) any {
	return func(k any) any {
		return func(m any) any {
			v, idx := find(m.([]any), k)
			if idx < 0 {
				return def
			}
			return v
		}
	}
}

// hydra.lib.maps.lookupMaybe : k -> Map k v -> Maybe v
var LookupMaybe = Lookup

// Bimap applies functions to both keys and values.
// hydra.lib.maps.bimap : (k1 -> k2) -> (v1 -> v2) -> Map k1 v1 -> Map k2 v2
func Bimap(fk any) any {
	return func(fv any) any {
		return func(m any) any {
			fnk := fk.(func(any) any)
			fnv := fv.(func(any) any)
			es := m.([]any)
			r := make([]any, len(es))
			for i, e := range es {
				p := e.([2]any)
				r[i] = [2]any{fnk(p[0]), fnv(p[1])}
			}
			return any(r)
		}
	}
}

// Alter updates a value at a key using a function on Maybe.
// hydra.lib.maps.alter : (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
func Alter(f any) any {
	return func(k any) any {
		return func(m any) any {
			fn := f.(func(any) any)
			es := m.([]any)
			var found bool
			var r []any
			for _, e := range es {
				p := e.([2]any)
				if p[0] == k {
					found = true
					v := p[1]
					result := fn(&v)
					if result != nil {
						r = append(r, [2]any{k, *result.(*any)})
					}
				} else {
					r = append(r, e)
				}
			}
			if !found {
				result := fn(nil)
				if result != nil {
					r = append(r, [2]any{k, *result.(*any)})
				}
			}
			if r == nil {
				r = []any{}
			}
			return any(r)
		}
	}
}
