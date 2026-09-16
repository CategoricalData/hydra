// Package maps implements the hydra.lib.maps primitives over the persistent,
// canonical-order util.Map. findWithDefault's argument 0 is lazy (a func() any
// thunk). Maps are immutable; every mutator returns a new map.
package maps

import "hydra.dev/hydra/overlay/go/util"

func asMap(x any) util.Map { return x.(util.Map) }

// Alter : (optional<v> -> optional<v>) -> k -> map<k,v> -> map<k,v>
func Alter(f any, key any, m any) any {
	mp := asMap(m)
	res := f.(func(any) any)(mp.Lookup(key)).(util.Optional)
	if res.IsPresent() {
		return mp.Insert(key, res.Get())
	}
	return mp.Delete(key)
}

// Bimap : (k1 -> k2) -> (v1 -> v2) -> map<k1,v1> -> map<k2,v2>
func Bimap(fk any, fv any, m any) any {
	gk, gv := fk.(func(any) any), fv.(func(any) any)
	pairs := []util.Pair{}
	for _, p := range asMap(m).ToPairs() {
		pr := p.(util.Pair)
		pairs = append(pairs, util.NewPair(gk(pr.First), gv(pr.Second)))
	}
	return util.NewMap(pairs...)
}

// Delete : k -> map<k,v> -> map<k,v>
func Delete(key any, m any) any { return asMap(m).Delete(key) }

// Difference : map<k,v> -> map<k,v> -> map<k,v> (entries of a whose keys are
// not present in b)
func Difference(a any, b any) any {
	bs := asMap(b)
	res := util.EmptyMap()
	for _, pp := range asMap(a).ToPairs() {
		pr := pp.(util.Pair)
		if !bs.Member(pr.First) {
			res = res.Insert(pr.First, pr.Second)
		}
	}
	return res
}

// Elems : map<k,v> -> list<v>
func Elems(m any) any { return asMap(m).Values() }

// Empty : map<k,v>
func Empty() any { return util.EmptyMap() }

// Filter : (v -> boolean) -> map<k,v> -> map<k,v>
func Filter(p any, m any) any {
	pred := p.(func(any) bool)
	pairs := []util.Pair{}
	for _, pp := range asMap(m).ToPairs() {
		pr := pp.(util.Pair)
		if pred(pr.Second) {
			pairs = append(pairs, pr)
		}
	}
	return util.NewMap(pairs...)
}

// FilterWithKey : (k -> v -> boolean) -> map<k,v> -> map<k,v>. Predicate curried.
func FilterWithKey(p any, m any) any {
	pred := p.(func(any) func(any) bool)
	pairs := []util.Pair{}
	for _, pp := range asMap(m).ToPairs() {
		pr := pp.(util.Pair)
		if pred(pr.First)(pr.Second) {
			pairs = append(pairs, pr)
		}
	}
	return util.NewMap(pairs...)
}

// FindWithDefault : v -> k -> map<k,v> -> v. Argument 0 (default) is lazy.
func FindWithDefault(def any, key any, m any) any {
	if v := asMap(m).Lookup(key); v.IsPresent() {
		return v.Get()
	}
	return def.(func() any)()
}

// FromList : list<pair<k,v>> -> map<k,v>
func FromList(lst any) any {
	pairs := []util.Pair{}
	for _, p := range lst.([]any) {
		pairs = append(pairs, p.(util.Pair))
	}
	return util.NewMap(pairs...)
}

// Insert : k -> v -> map<k,v> -> map<k,v>
func Insert(key any, value any, m any) any { return asMap(m).Insert(key, value) }

// Intersection : map<k,v> -> map<k,v> -> map<k,v> (entries whose keys are in
// both; values taken from a, matching Union's left bias)
func Intersection(a any, b any) any {
	bs := asMap(b)
	res := util.EmptyMap()
	for _, pp := range asMap(a).ToPairs() {
		pr := pp.(util.Pair)
		if bs.Member(pr.First) {
			res = res.Insert(pr.First, pr.Second)
		}
	}
	return res
}

// Keys : map<k,v> -> list<k>
func Keys(m any) any { return asMap(m).Keys() }

// Lookup : k -> map<k,v> -> optional<v>
func Lookup(key any, m any) any { return asMap(m).Lookup(key) }

// Map : (v1 -> v2) -> map<k,v1> -> map<k,v2>
func Map(f any, m any) any {
	g := f.(func(any) any)
	pairs := []util.Pair{}
	for _, pp := range asMap(m).ToPairs() {
		pr := pp.(util.Pair)
		pairs = append(pairs, util.NewPair(pr.First, g(pr.Second)))
	}
	return util.NewMap(pairs...)
}

// MapKeys : (k1 -> k2) -> map<k1,v> -> map<k2,v>
func MapKeys(f any, m any) any {
	g := f.(func(any) any)
	pairs := []util.Pair{}
	for _, pp := range asMap(m).ToPairs() {
		pr := pp.(util.Pair)
		pairs = append(pairs, util.NewPair(g(pr.First), pr.Second))
	}
	return util.NewMap(pairs...)
}

// Member : k -> map<k,v> -> boolean
func Member(key any, m any) any { return asMap(m).Member(key) }

// IsEmpty : map<k,v> -> boolean
func IsEmpty(m any) any { return asMap(m).Size() == 0 }

// Singleton : k -> v -> map<k,v>
func Singleton(key any, value any) any { return util.NewMap(util.NewPair(key, value)) }

// Size : map<k,v> -> int32
func Size(m any) any { return int32(asMap(m).Size()) }

// ToList : map<k,v> -> list<pair<k,v>>
func ToList(m any) any { return asMap(m).ToPairs() }

// Union : map<k,v> -> map<k,v> -> map<k,v> (left-biased on key clash)
func Union(a any, b any) any {
	res := asMap(b)
	for _, pp := range asMap(a).ToPairs() {
		pr := pp.(util.Pair)
		res = res.Insert(pr.First, pr.Second)
	}
	return res
}

// Unions : list<map<k,v>> -> map<k,v>. LEFT-BIASED: an earlier map's binding
// wins over a later one, matching Union above (which lets `a` win) and Haskell's
// `unions = foldl union empty`. Inserting unconditionally would silently give
// the opposite, last-wins bias.
func Unions(lst any) any {
	res := util.EmptyMap()
	for _, m := range lst.([]any) {
		for _, pp := range asMap(m).ToPairs() {
			pr := pp.(util.Pair)
			if !res.Member(pr.First) {
				res = res.Insert(pr.First, pr.Second)
			}
		}
	}
	return res
}
