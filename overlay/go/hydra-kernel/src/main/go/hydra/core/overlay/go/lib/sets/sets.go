// Package sets implements the hydra.core.lib.sets primitives over the persistent,
// canonical-order util.Set. Sets are immutable; every mutator returns a new set.
package sets

import "hydra.dev/hydra/overlay/go/util"

func asSet(x any) util.Set { return x.(util.Set) }

// Delete : a -> set<a> -> set<a>
func Delete(x any, s any) any { return asSet(s).Delete(x) }

// Difference : set<a> -> set<a> -> set<a> (elements of a not in b)
func Difference(a any, b any) any {
	bs := asSet(b)
	res := util.EmptySet()
	for _, x := range asSet(a).ToList() {
		if !bs.Member(x) {
			res = res.Insert(x)
		}
	}
	return res
}

// Empty : set<a>
func Empty() any { return util.EmptySet() }

// Filter : (a -> boolean) -> set<a> -> set<a>
func Filter(p any, s any) any {
	pred := p.(func(any) bool)
	out := []any{}
	for _, x := range asSet(s).ToList() {
		if pred(x) {
			out = append(out, x)
		}
	}
	return util.NewSet(out...)
}

// FromList : list<a> -> set<a>
func FromList(lst any) any { return util.NewSet(lst.([]any)...) }

// Insert : a -> set<a> -> set<a>
func Insert(x any, s any) any { return asSet(s).Insert(x) }

// Intersection : set<a> -> set<a> -> set<a>
func Intersection(a any, b any) any {
	bs := asSet(b)
	res := util.EmptySet()
	for _, x := range asSet(a).ToList() {
		if bs.Member(x) {
			res = res.Insert(x)
		}
	}
	return res
}

// Map : (a -> b) -> set<a> -> set<b>
func Map(f any, s any) any {
	g := f.(func(any) any)
	out := []any{}
	for _, x := range asSet(s).ToList() {
		out = append(out, g(x))
	}
	return util.NewSet(out...)
}

// Member : a -> set<a> -> boolean
func Member(x any, s any) any { return asSet(s).Member(x) }

// IsEmpty : set<a> -> boolean
func IsEmpty(s any) any { return asSet(s).Size() == 0 }

// Singleton : a -> set<a>
func Singleton(x any) any { return util.NewSet(x) }

// Size : set<a> -> int32
func Size(s any) any { return int32(asSet(s).Size()) }

// ToList : set<a> -> list<a>
func ToList(s any) any { return asSet(s).ToList() }

// Union : set<a> -> set<a> -> set<a>
func Union(a any, b any) any {
	res := asSet(b)
	for _, x := range asSet(a).ToList() {
		res = res.Insert(x)
	}
	return res
}

// Unions : list<set<a>> -> set<a>
func Unions(lst any) any {
	res := util.EmptySet()
	for _, s := range lst.([]any) {
		for _, x := range asSet(s).ToList() {
			res = res.Insert(x)
		}
	}
	return res
}
