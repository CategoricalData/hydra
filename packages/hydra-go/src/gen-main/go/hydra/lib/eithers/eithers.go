// Note: this is an automatically generated file. Do not edit.

package eithers

func isL(e any) bool {
	return e.([2]any)[0].(string) == "left"
}

func lv(e any) any {
	return e.([2]any)[1]
}

func rv(e any) any {
	return e.([2]any)[1]
}

func ml(v any) any {
	return [2]any{"left", v}
}

func mr(v any) any {
	return [2]any{"right", v}
}

// hydra.lib.eithers.bind : Either a b -> (b -> Either a c) -> Either a c
func Bind(e any) any {
	return func(f any) any {
		if isL(e) {
			return e
		}
		return f.(func(any) any)(rv(e))
	}
}

// hydra.lib.eithers.map : (b -> c) -> Either a b -> Either a c
func Map(f any) any {
	return func(e any) any {
		if isL(e) {
			return e
		}
		return mr(f.(func(any) any)(rv(e)))
	}
}

// hydra.lib.eithers.either : (a -> c) -> (b -> c) -> Either a b -> c
func Either(fl any) any {
	return func(fr any) any {
		return func(e any) any {
			if isL(e) {
				return fl.(func(any) any)(lv(e))
			}
			return fr.(func(any) any)(rv(e))
		}
	}
}

// hydra.lib.eithers.mapList : (a -> Either b c) -> [a] -> Either b [c]
func MapList(f any) any {
	return func(lst any) any {
		fn := f.(func(any) any)
		xs := lst.([]any)
		result := make([]any, 0, len(xs))
		for _, x := range xs {
			e := fn(x)
			if isL(e) {
				return e
			}
			result = append(result, rv(e))
		}
		return mr(result)
	}
}

// hydra.lib.eithers.bimap : (a -> c) -> (b -> d) -> Either a b -> Either c d
func Bimap(fl any) any {
	return func(fr any) any {
		return func(e any) any {
			if isL(e) {
				return ml(fl.(func(any) any)(lv(e)))
			}
			return mr(fr.(func(any) any)(rv(e)))
		}
	}
}

// hydra.lib.eithers.fromLeft : a -> Either a b -> a
func FromLeft(def any) any {
	return func(e any) any {
		if isL(e) {
			return lv(e)
		}
		return def
	}
}

// hydra.lib.eithers.fromRight : b -> Either a b -> b
func FromRight(def any) any {
	return func(e any) any {
		if !isL(e) {
			return rv(e)
		}
		return def
	}
}

// hydra.lib.eithers.isLeft : Either a b -> Boolean
func IsLeft_(e any) any {
	return isL(e)
}

// hydra.lib.eithers.isRight : Either a b -> Boolean
func IsRight_(e any) any {
	return !isL(e)
}

// hydra.lib.eithers.mapLeft : (a -> c) -> Either a b -> Either c b
func MapLeft(f any) any {
	return func(e any) any {
		if isL(e) {
			return ml(f.(func(any) any)(lv(e)))
		}
		return e
	}
}

// hydra.lib.eithers.mapRight : (b -> c) -> Either a b -> Either a c
func MapRight(f any) any {
	return func(e any) any {
		if !isL(e) {
			return mr(f.(func(any) any)(rv(e)))
		}
		return e
	}
}

// hydra.lib.eithers.pure : a -> Either b a
func Pure(v any) any {
	return mr(v)
}
