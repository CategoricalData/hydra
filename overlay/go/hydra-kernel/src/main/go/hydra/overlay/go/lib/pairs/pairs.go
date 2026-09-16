// Package pairs implements the hydra.lib.pairs primitives.
package pairs

import "hydra.dev/hydra/overlay/go/util"

// Bimap : (a -> c) -> (b -> d) -> pair<a,b> -> pair<c,d>
func Bimap(f any, g any, p any) any {
	pr := p.(util.Pair)
	return util.NewPair(f.(func(any) any)(pr.First), g.(func(any) any)(pr.Second))
}

// First : pair<a,b> -> a
func First(p any) any { return p.(util.Pair).First }

// Pair : a -> b -> pair<a,b>. The pair constructor.
func Pair(a any, b any) any { return util.NewPair(a, b) }

// Second : pair<a,b> -> b
func Second(p any) any { return p.(util.Pair).Second }
