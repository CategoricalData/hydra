// Note: this is an automatically generated file. Do not edit.

package maybes

func deref(p any) any {
	return *p.(*any)
}

func just(v any) any {
	val := any(v)
	return &val
}

// hydra.lib.maybes.maybe : b -> (a -> b) -> Maybe a -> b
func Maybe(def any) any {
	return func(f any) any {
		return func(m any) any {
			if m == nil {
				return def
			}
			return f.(func(any) any)(deref(m))
		}
	}
}

// hydra.lib.maybes.map : (a -> b) -> Maybe a -> Maybe b
func Map(f any) any {
	return func(m any) any {
		if m == nil {
			return nil
		}
		return just(f.(func(any) any)(deref(m)))
	}
}

// hydra.lib.maybes.fromMaybe : a -> Maybe a -> a
func FromMaybe(def any) any {
	return func(m any) any {
		if m == nil {
			return def
		}
		return deref(m)
	}
}

// hydra.lib.maybes.cat : [Maybe a] -> [a]
func Cat(lst any) any {
	xs := lst.([]any)
	result := make([]any, 0)
	for _, m := range xs {
		if m != nil {
			result = append(result, deref(m))
		}
	}
	return result
}

// hydra.lib.maybes.isJust : Maybe a -> Boolean
func IsJust(m any) any {
	return m != nil
}

// hydra.lib.maybes.isNothing : Maybe a -> Boolean
func IsNothing(m any) any {
	return m == nil
}

// hydra.lib.maybes.fromJust : Maybe a -> a
func FromJust(m any) any {
	return deref(m)
}

// hydra.lib.maybes.pure : a -> Maybe a
func Pure(v any) any {
	return just(v)
}

// hydra.lib.maybes.bind : Maybe a -> (a -> Maybe b) -> Maybe b
func Bind(m any) any {
	return func(f any) any {
		if m == nil {
			return nil
		}
		return f.(func(any) any)(deref(m))
	}
}

// hydra.lib.maybes.apply : Maybe (a -> b) -> Maybe a -> Maybe b
func Apply(mf any) any {
	return func(ma any) any {
		if mf == nil || ma == nil {
			return nil
		}
		fn := deref(mf).(func(any) any)
		return just(fn(deref(ma)))
	}
}

// hydra.lib.maybes.cases : b -> (a -> b) -> Maybe a -> b
var Cases = Maybe

// hydra.lib.maybes.compose : (a -> Maybe b) -> (b -> Maybe c) -> a -> Maybe c
func Compose(f any) any {
	return func(g any) any {
		return func(a any) any {
			m := f.(func(any) any)(a)
			if m == nil {
				return nil
			}
			return g.(func(any) any)(deref(m))
		}
	}
}

// hydra.lib.maybes.catMaybes : [Maybe a] -> [a]
var CatMaybes = Cat

// hydra.lib.maybes.mapMaybe : (a -> Maybe b) -> [a] -> [b]
func MapMaybe(f any) any {
	return func(lst any) any {
		fn := f.(func(any) any)
		xs := lst.([]any)
		result := make([]any, 0)
		for _, x := range xs {
			m := fn(x)
			if m != nil {
				result = append(result, deref(m))
			}
		}
		return result
	}
}
