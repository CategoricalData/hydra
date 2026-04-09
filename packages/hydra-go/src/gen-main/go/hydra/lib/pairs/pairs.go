// Note: this is an automatically generated file. Do not edit.

package pairs

// hydra.lib.pairs.first : (a, b) -> a
func First(p any) any {
	return p.([2]any)[0]
}

// hydra.lib.pairs.second : (a, b) -> b
func Second(p any) any {
	return p.([2]any)[1]
}

// hydra.lib.pairs.bimap : (a -> c) -> (b -> d) -> (a, b) -> (c, d)
func Bimap(f any) any {
	return func(g any) any {
		return func(p any) any {
			pair := p.([2]any)
			return [2]any{
				f.(func(any) any)(pair[0]),
				g.(func(any) any)(pair[1]),
			}
		}
	}
}

// hydra.lib.pairs.curry : ((a, b) -> c) -> a -> b -> c
func Curry(f any) any {
	return func(a any) any {
		return func(b any) any {
			return f.(func(any) any)([2]any{a, b})
		}
	}
}

// hydra.lib.pairs.uncurry : (a -> b -> c) -> (a, b) -> c
func Uncurry(f any) any {
	return func(p any) any {
		pair := p.([2]any)
		return f.(func(any) any)(pair[0]).(func(any) any)(pair[1])
	}
}

// hydra.lib.pairs.swap : (a, b) -> (b, a)
func Swap(p any) any {
	pair := p.([2]any)
	return [2]any{pair[1], pair[0]}
}

// hydra.lib.pairs.mapFirst : (a -> c) -> (a, b) -> (c, b)
func MapFirst(f any) any {
	return func(p any) any {
		pair := p.([2]any)
		return [2]any{f.(func(any) any)(pair[0]), pair[1]}
	}
}

// hydra.lib.pairs.mapSecond : (b -> c) -> (a, b) -> (a, c)
func MapSecond(f any) any {
	return func(p any) any {
		pair := p.([2]any)
		return [2]any{pair[0], f.(func(any) any)(pair[1])}
	}
}
