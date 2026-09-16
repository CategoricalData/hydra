// Package lists implements the hydra.lib.lists primitives.
//
// Lists are represented as Go []any. Unary function arguments are func(any) any;
// binary function arguments are curried func(any) func(any) any; predicates
// return bool directly (matching the coder's call sites). Optional results use
// the overlay util.Optional; pairs use util.Pair.
package lists

import "hydra.dev/hydra/overlay/go/util"

func asList(x any) []any { return x.([]any) }

// Apply : list<a -> b> -> list<a> -> list<b> (all combinations)
func Apply(fs any, xs any) any {
	out := []any{}
	for _, f := range asList(fs) {
		for _, x := range asList(xs) {
			out = append(out, f.(func(any) any)(x))
		}
	}
	return out
}

// Bind : list<a> -> (a -> list<b>) -> list<b>
func Bind(xs any, f any) any {
	out := []any{}
	for _, x := range asList(xs) {
		out = append(out, asList(f.(func(any) any)(x))...)
	}
	return out
}

// Concat : list<list<a>> -> list<a>
func Concat(xss any) any {
	out := []any{}
	for _, xs := range asList(xss) {
		out = append(out, asList(xs)...)
	}
	return out
}

// Concat2 : list<a> -> list<a> -> list<a>
func Concat2(xs any, ys any) any {
	a, b := asList(xs), asList(ys)
	out := make([]any, 0, len(a)+len(b))
	out = append(out, a...)
	out = append(out, b...)
	return out
}

// Cons : a -> list<a> -> list<a>
func Cons(x any, xs any) any {
	a := asList(xs)
	out := make([]any, 0, len(a)+1)
	out = append(out, x)
	out = append(out, a...)
	return out
}

// Drop : int32 -> list<a> -> list<a>
func Drop(n any, xs any) any {
	a := asList(xs)
	k := int(n.(int32))
	if k < 0 {
		k = 0
	}
	if k > len(a) {
		k = len(a)
	}
	return append([]any{}, a[k:]...)
}

// DropWhile : (a -> boolean) -> list<a> -> list<a>
func DropWhile(p any, xs any) any {
	a := asList(xs)
	i := 0
	for i < len(a) && p.(func(any) bool)(a[i]) {
		i++
	}
	return append([]any{}, a[i:]...)
}

// Elem : a -> list<a> -> boolean
func Member(x any, xs any) any {
	for _, e := range asList(xs) {
		if util.Compare(e, x) == 0 {
			return true
		}
	}
	return false
}

// Filter : (a -> boolean) -> list<a> -> list<a>
func Filter(p any, xs any) any {
	out := []any{}
	for _, x := range asList(xs) {
		if p.(func(any) bool)(x) {
			out = append(out, x)
		}
	}
	return out
}

// Find : (a -> boolean) -> list<a> -> optional<a>
func Find(p any, xs any) any {
	for _, x := range asList(xs) {
		if p.(func(any) bool)(x) {
			return util.Some(x)
		}
	}
	return util.None()
}

// Foldl : (b -> a -> b) -> b -> list<a> -> b. The folding function is curried.
func Foldl(f any, acc any, xs any) any {
	g := f.(func(any) func(any) any)
	for _, x := range asList(xs) {
		acc = g(acc)(x)
	}
	return acc
}

// Foldr : (a -> b -> b) -> b -> list<a> -> b. The folding function is curried.
func Foldr(f any, acc any, xs any) any {
	g := f.(func(any) func(any) any)
	a := asList(xs)
	for i := len(a) - 1; i >= 0; i-- {
		acc = g(a[i])(acc)
	}
	return acc
}

// Intercalate : list<a> -> list<list<a>> -> list<a>
func Join(sep any, xss any) any {
	s := asList(sep)
	groups := asList(xss)
	out := []any{}
	for i, g := range groups {
		if i > 0 {
			out = append(out, s...)
		}
		out = append(out, asList(g)...)
	}
	return out
}

// Intersperse : a -> list<a> -> list<a>
func Intersperse(sep any, xs any) any {
	a := asList(xs)
	out := []any{}
	for i, x := range a {
		if i > 0 {
			out = append(out, sep)
		}
		out = append(out, x)
	}
	return out
}

// Length : list<a> -> int32
func Length(xs any) any { return int32(len(asList(xs))) }

// Map : (a -> b) -> list<a> -> list<b>
func Map(f any, xs any) any {
	a := asList(xs)
	out := make([]any, len(a))
	g := f.(func(any) any)
	for i, x := range a {
		out[i] = g(x)
	}
	return out
}

// MaybeAt : int32 -> list<a> -> optional<a>
func At(n any, xs any) any {
	a := asList(xs)
	i := int(n.(int32))
	if i < 0 || i >= len(a) {
		return util.None()
	}
	return util.Some(a[i])
}

// MaybeHead : list<a> -> optional<a>
func Head(xs any) any {
	a := asList(xs)
	if len(a) == 0 {
		return util.None()
	}
	return util.Some(a[0])
}

// MaybeInit : list<a> -> optional<list<a>>
func Init(xs any) any {
	a := asList(xs)
	if len(a) == 0 {
		return util.None()
	}
	return util.Some(append([]any{}, a[:len(a)-1]...))
}

// MaybeLast : list<a> -> optional<a>
func Last(xs any) any {
	a := asList(xs)
	if len(a) == 0 {
		return util.None()
	}
	return util.Some(a[len(a)-1])
}

// MaybeTail : list<a> -> optional<list<a>>
func Tail(xs any) any {
	a := asList(xs)
	if len(a) == 0 {
		return util.None()
	}
	return util.Some(append([]any{}, a[1:]...))
}

// Nub : list<a> -> list<a> (order-preserving dedup by canonical equality)
func Distinct(xs any) any {
	a := asList(xs)
	out := []any{}
	for _, x := range a {
		seen := false
		for _, y := range out {
			if util.Compare(x, y) == 0 {
				seen = true
				break
			}
		}
		if !seen {
			out = append(out, x)
		}
	}
	return out
}

// IsEmpty : list<a> -> boolean
func IsEmpty(xs any) any { return len(asList(xs)) == 0 }

// Partition : (a -> boolean) -> list<a> -> pair<list<a>, list<a>>
func Partition(p any, xs any) any {
	yes, no := []any{}, []any{}
	pred := p.(func(any) bool)
	for _, x := range asList(xs) {
		if pred(x) {
			yes = append(yes, x)
		} else {
			no = append(no, x)
		}
	}
	return util.NewPair(yes, no)
}

// Pure : a -> list<a>
func Pure(x any) any { return []any{x} }

// Replicate : int32 -> a -> list<a>
func Replicate(n any, x any) any {
	k := int(n.(int32))
	if k < 0 {
		k = 0
	}
	out := make([]any, k)
	for i := range out {
		out[i] = x
	}
	return out
}

// Reverse : list<a> -> list<a>
func Reverse(xs any) any {
	a := asList(xs)
	out := make([]any, len(a))
	for i, x := range a {
		out[len(a)-1-i] = x
	}
	return out
}

// Singleton : a -> list<a>
func Singleton(x any) any { return []any{x} }

// Sort : list<a> -> list<a> (canonical ascending order)
func Sort(xs any) any {
	out := append([]any{}, asList(xs)...)
	util.SortAny(out)
	return out
}

// SortOn : (a -> b) -> list<a> -> list<a> (by projected key, canonical order)
func SortBy(f any, xs any) any {
	a := append([]any{}, asList(xs)...)
	g := f.(func(any) any)
	keys := make([]any, len(a))
	for i, x := range a {
		keys[i] = g(x)
	}
	util.SortByKey(a, keys)
	return a
}

// Span : (a -> boolean) -> list<a> -> pair<list<a>, list<a>>
func Span(p any, xs any) any {
	a := asList(xs)
	pred := p.(func(any) bool)
	i := 0
	for i < len(a) && pred(a[i]) {
		i++
	}
	return util.NewPair(append([]any{}, a[:i]...), append([]any{}, a[i:]...))
}

// Take : int32 -> list<a> -> list<a>
func Take(n any, xs any) any {
	a := asList(xs)
	k := int(n.(int32))
	if k < 0 {
		k = 0
	}
	if k > len(a) {
		k = len(a)
	}
	return append([]any{}, a[:k]...)
}

// Uncons : list<a> -> optional<pair<a, list<a>>>
func Uncons(xs any) any {
	a := asList(xs)
	if len(a) == 0 {
		return util.None()
	}
	return util.Some(util.NewPair(a[0], append([]any{}, a[1:]...)))
}

// Zip : list<a> -> list<b> -> list<pair<a,b>>
func Zip(xs any, ys any) any {
	a, b := asList(xs), asList(ys)
	n := len(a)
	if len(b) < n {
		n = len(b)
	}
	out := make([]any, n)
	for i := 0; i < n; i++ {
		out[i] = util.NewPair(a[i], b[i])
	}
	return out
}

// ZipWith : (a -> b -> c) -> list<a> -> list<b> -> list<c>. Function is curried.
func ZipWith(f any, xs any, ys any) any {
	g := f.(func(any) func(any) any)
	a, b := asList(xs), asList(ys)
	n := len(a)
	if len(b) < n {
		n = len(b)
	}
	out := make([]any, n)
	for i := 0; i < n; i++ {
		out[i] = g(a[i])(b[i])
	}
	return out
}

// The following were added/split into hydra.lib.lists by #417/#566; semantics
// mirror the authoritative Haskell overlay (Hydra.Overlay.Haskell.Lib.Lists).

// TakeWhile : (a -> boolean) -> list<a> -> list<a>
func TakeWhile(p any, xs any) any {
	a := asList(xs)
	pred := p.(func(any) bool)
	i := 0
	for i < len(a) && pred(a[i]) {
		i++
	}
	return append([]any{}, a[:i]...)
}

// Group : list<a> -> list<list<a>> (group consecutive equal elements)
func Group(xs any) any {
	a := asList(xs)
	out := []any{}
	i := 0
	for i < len(a) {
		j := i + 1
		for j < len(a) && util.Compare(a[j], a[i]) == 0 {
			j++
		}
		out = append(out, append([]any{}, a[i:j]...))
		i = j
	}
	return out
}

// Transpose : list<list<a>> -> list<list<a>>
func Transpose(xss any) any {
	rows := asList(xss)
	maxLen := 0
	for _, r := range rows {
		if l := len(asList(r)); l > maxLen {
			maxLen = l
		}
	}
	out := []any{}
	for c := 0; c < maxLen; c++ {
		col := []any{}
		for _, r := range rows {
			rl := asList(r)
			if c < len(rl) {
				col = append(col, rl[c])
			}
		}
		out = append(out, col)
	}
	return out
}

// Compose : (a -> list<b>) -> (b -> list<c>) -> a -> c (Kleisli in the list monad).
// Flat-called Compose(f, g); returns func(x) = concatMap g (f x).
func Compose(f any, g any) any {
	ff := f.(func(any) any)
	gg := g.(func(any) any)
	return func(x any) any {
		out := []any{}
		for _, b := range asList(ff(x)) {
			out = append(out, asList(gg(b))...)
		}
		return out
	}
}

// FoldList : (a -> b -> list<a>) -> a -> list<b> -> list<a> (nondeterministic fold;
// Haskell's foldM in the list monad). f is curried (func(a) func(b) list<a>).
func FoldList(f any, acc any, xs any) any {
	g := f.(func(any) func(any) any)
	accs := []any{acc}
	for _, x := range asList(xs) {
		next := []any{}
		for _, a := range accs {
			next = append(next, asList(g(a)(x))...)
		}
		accs = next
	}
	return accs
}

// MapList : (a -> list<b>) -> list<a> -> list<list<b>> (traverse in the list monad;
// Cartesian product of the per-element choices).
func MapList(f any, xs any) any {
	g := f.(func(any) any)
	results := []any{[]any{}}
	for _, x := range asList(xs) {
		choices := asList(g(x))
		next := []any{}
		for _, r := range results {
			rl := asList(r)
			for _, c := range choices {
				combined := append(append([]any{}, rl...), c)
				next = append(next, combined)
			}
		}
		results = next
	}
	return results
}

// MapOptional : (a -> list<b>) -> optional<a> -> list<optional<b>>
func MapOptional(f any, mx any) any {
	o := mx.(util.Optional)
	if !o.IsPresent() {
		return []any{util.None()}
	}
	g := f.(func(any) any)
	out := []any{}
	for _, b := range asList(g(o.Get())) {
		out = append(out, util.Some(b))
	}
	return out
}

// MapSet : (a -> list<b>) -> set<a> -> list<set<b>> (traverse a set in the list
// monad: Cartesian product over elements, each result collected into a set).
func MapSet(f any, s any) any {
	g := f.(func(any) any)
	elems := s.(util.Set).ToList()
	tuples := []any{[]any{}}
	for _, x := range elems {
		choices := asList(g(x))
		next := []any{}
		for _, t := range tuples {
			tl := asList(t)
			for _, c := range choices {
				next = append(next, append(append([]any{}, tl...), c))
			}
		}
		tuples = next
	}
	out := []any{}
	for _, t := range tuples {
		out = append(out, util.NewSet(asList(t)...))
	}
	return out
}
