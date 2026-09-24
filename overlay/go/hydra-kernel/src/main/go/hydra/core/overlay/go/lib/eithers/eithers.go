// Package eithers implements the hydra.core.lib.eithers primitives over util.Either.
//
// Function-valued arguments follow the coder's curried convention: a unary
// function is func(any) any. Eithers are right-biased (map/bind operate on the
// Right side). fromLeft/fromRight take a lazy default: a func() any thunk forced
// only when the requested side is absent. Lists are []any; sets use util.Set;
// optionals use util.Optional; the partitionEithers result is a util.Pair of
// (lefts, rights).
package eithers

import "hydra.dev/hydra/overlay/go/util"

func asEither(x any) util.Either { return x.(util.Either) }

// Apply : either<a, b -> c> -> either<a, b> -> either<a, c>
func Apply(ef any, ex any) any {
	f := asEither(ef)
	if f.IsLeft() {
		return f
	}
	x := asEither(ex)
	if x.IsLeft() {
		return x
	}
	return util.Right(f.Value().(func(any) any)(x.Value()))
}

// Bimap : (a -> c) -> (b -> d) -> either<a, b> -> either<c, d>
func Bimap(fl any, fr any, e any) any {
	ev := asEither(e)
	if ev.IsLeft() {
		return util.Left(fl.(func(any) any)(ev.Value()))
	}
	return util.Right(fr.(func(any) any)(ev.Value()))
}

// Bind : either<a, b> -> (b -> either<a, c>) -> either<a, c>. Right-biased.
func Bind(e any, f any) any {
	ev := asEither(e)
	if ev.IsLeft() {
		return ev
	}
	return f.(func(any) any)(ev.Value())
}

// Compose : (a -> either<e, b>) -> (b -> either<e, c>) -> a -> either<e, c>
func Compose(f any, g any, x any) any {
	r := f.(func(any) any)(x)
	return Bind(r, g)
}

// Either : (a -> c) -> (b -> c) -> either<a, b> -> c
func Either(fl any, fr any, e any) any {
	ev := asEither(e)
	if ev.IsLeft() {
		return fl.(func(any) any)(ev.Value())
	}
	return fr.(func(any) any)(ev.Value())
}

// FoldList : (a -> b -> either<c, a>) -> a -> list<b> -> either<c, a>.
// Left-fold short-circuiting on the first Left. The accumulator function is
// curried: func(any) func(any) any.
//
// Named for hydra.core.lib.eithers.foldList; there is no eithers.foldl primitive, so
// the former `Foldl` spelling was drift and would have gone undefined the moment
// the kernel called this.
func FoldList(f any, acc any, lst any) any {
	var cur any = util.Right(acc)
	for _, x := range lst.([]any) {
		ev := asEither(cur)
		if ev.IsLeft() {
			return ev
		}
		cur = f.(func(any) func(any) any)(ev.Value())(x)
	}
	return cur
}

// FromLeft : a -> either<a, b> -> a. The default (arg 0) is a lazy func() any
// thunk, forced only when the either is Right.
func FromLeft(def any, e any) any {
	ev := asEither(e)
	if ev.IsLeft() {
		return ev.Value()
	}
	return def.(func() any)()
}

// FromRight : b -> either<a, b> -> b. The default (arg 0) is a lazy func() any
// thunk, forced only when the either is Left.
func FromRight(def any, e any) any {
	ev := asEither(e)
	if ev.IsRight() {
		return ev.Value()
	}
	return def.(func() any)()
}

// IsLeft : either<a, b> -> boolean
func IsLeft(e any) any { return asEither(e).IsLeft() }

// IsRight : either<a, b> -> boolean
func IsRight(e any) any { return asEither(e).IsRight() }

// Left : a -> either<a, b>. The left constructor.
func Left(x any) any { return util.Left(x) }

// Lefts : list<either<a, b>> -> list<a>
func Lefts(lst any) any {
	out := []any{}
	for _, e := range lst.([]any) {
		if ev := asEither(e); ev.IsLeft() {
			out = append(out, ev.Value())
		}
	}
	return out
}

// Map : (b -> c) -> either<a, b> -> either<a, c>. Maps the Right side.
func Map(f any, e any) any {
	ev := asEither(e)
	if ev.IsLeft() {
		return ev
	}
	return util.Right(f.(func(any) any)(ev.Value()))
}

// MapList : (a -> either<c, b>) -> list<a> -> either<c, list<b>>.
// Short-circuits on the first Left.
func MapList(f any, lst any) any {
	out := []any{}
	for _, x := range lst.([]any) {
		ev := asEither(f.(func(any) any)(x))
		if ev.IsLeft() {
			return ev
		}
		out = append(out, ev.Value())
	}
	return util.Right(out)
}

// MapOptional : (a -> either<c, b>) -> optional<a> -> either<c, optional<b>>.
func MapOptional(f any, mx any) any {
	x := mx.(util.Optional)
	if !x.IsPresent() {
		return util.Right(util.None())
	}
	ev := asEither(f.(func(any) any)(x.Get()))
	if ev.IsLeft() {
		return ev
	}
	return util.Right(util.Some(ev.Value()))
}

// MapSet : (a -> either<c, b>) -> set<a> -> either<c, set<b>>.
// Short-circuits on the first Left.
func MapSet(f any, s any) any {
	out := []any{}
	for _, x := range s.(util.Set).ToList() {
		ev := asEither(f.(func(any) any)(x))
		if ev.IsLeft() {
			return ev
		}
		out = append(out, ev.Value())
	}
	return util.Right(util.NewSet(out...))
}

// Partition : list<either<a, b>> -> (list<a>, list<b>). Alias of partitionEithers.
func Partition(lst any) any { return PartitionEithers(lst) }

// PartitionEithers : list<either<a, b>> -> (list<a>, list<b>). Returns a pair of
// (lefts, rights).
func PartitionEithers(lst any) any {
	ls := []any{}
	rs := []any{}
	for _, e := range lst.([]any) {
		if ev := asEither(e); ev.IsLeft() {
			ls = append(ls, ev.Value())
		} else {
			rs = append(rs, ev.Value())
		}
	}
	return util.NewPair(ls, rs)
}

// Pure : b -> either<a, b>
func Pure(x any) any { return util.Right(x) }

// Right : b -> either<a, b>. The right constructor.
func Right(x any) any { return util.Right(x) }

// Rights : list<either<a, b>> -> list<b>
func Rights(lst any) any {
	out := []any{}
	for _, e := range lst.([]any) {
		if ev := asEither(e); ev.IsRight() {
			out = append(out, ev.Value())
		}
	}
	return out
}
