// Package optionals implements the hydra.core.lib.optionals primitives.
//
// Function-valued arguments follow the coder's curried convention:
// a unary function is func(any) any. The lazy parameters (cases arg 1,
// fromOptional arg 0) arrive as func() any thunks.
package optionals

import "hydra.dev/hydra/overlay/go/util"

// Apply : optional<a -> b> -> optional<a> -> optional<b>
func Apply(mf any, mx any) any {
	f := mf.(util.Optional)
	x := mx.(util.Optional)
	if !f.IsPresent() || !x.IsPresent() {
		return util.None()
	}
	return util.Some(f.Get().(func(any) any)(x.Get()))
}

// Bind : optional<a> -> (a -> optional<b>) -> optional<b>
func Bind(mx any, f any) any {
	x := mx.(util.Optional)
	if !x.IsPresent() {
		return util.None()
	}
	return f.(func(any) any)(x.Get())
}

// Match : optional<a> -> b -> (a -> b) -> b. Argument 1 (the none branch) is
// lazy: a func() any thunk, forced only when the optional is absent.
func Match(mx any, ifNone any, ifGiven any) any {
	x := mx.(util.Optional)
	if !x.IsPresent() {
		return ifNone.(func() any)()
	}
	return ifGiven.(func(any) any)(x.Get())
}

// Cat : list<optional<a>> -> list<a>
func Givens(lst any) any {
	xs := lst.([]any)
	out := make([]any, 0, len(xs))
	for _, o := range xs {
		if opt := o.(util.Optional); opt.IsPresent() {
			out = append(out, opt.Get())
		}
	}
	return out
}

// FromOptional : a -> optional<a> -> a. Argument 0 (the default) is lazy:
// a func() any thunk, forced only when the optional is absent.
func WithDefault(def any, mx any) any {
	x := mx.(util.Optional)
	if x.IsPresent() {
		return x.Get()
	}
	return def.(func() any)()
}

// IsGiven : optional<a> -> boolean
func IsGiven(mx any) any { return mx.(util.Optional).IsPresent() }

// IsNone : optional<a> -> boolean
func IsNone(mx any) any { return !mx.(util.Optional).IsPresent() }

// Map : (a -> b) -> optional<a> -> optional<b>
func Map(f any, mx any) any {
	x := mx.(util.Optional)
	if !x.IsPresent() {
		return util.None()
	}
	return util.Some(f.(func(any) any)(x.Get()))
}

// MapOptional : (a -> optional<b>) -> optional<a> -> optional<b>
func MapOptional(f any, mx any) any {
	x := mx.(util.Optional)
	if !x.IsPresent() {
		return util.None()
	}
	return f.(func(any) any)(x.Get())
}

// Compose : (a -> optional<b>) -> (b -> optional<c>) -> a -> optional<c>.
// Kleisli composition; flat-called with all three arguments, mirroring
// eithers.Compose.
func Compose(f any, g any, x any) any {
	return Bind(f.(func(any) any)(x), g)
}

// FoldList : (a -> b -> optional<a>) -> a -> list<b> -> optional<a>.
// Left fold short-circuiting on the first none. The accumulator function is
// curried: func(any) func(any) any.
func FoldList(f any, acc any, lst any) any {
	var cur any = util.Some(acc)
	for _, x := range lst.([]any) {
		o := cur.(util.Optional)
		if !o.IsPresent() {
			return o
		}
		cur = f.(func(any) func(any) any)(o.Get())(x)
	}
	return cur
}

// MapList : (a -> optional<b>) -> list<a> -> optional<list<b>>. None if any
// element maps to none.
func MapList(f any, lst any) any {
	out := []any{}
	for _, x := range lst.([]any) {
		o := f.(func(any) any)(x).(util.Optional)
		if !o.IsPresent() {
			return util.None()
		}
		out = append(out, o.Get())
	}
	return util.Some(out)
}

// MapSet : (a -> optional<b>) -> set<a> -> optional<set<b>>. None if any
// element maps to none.
func MapSet(f any, s any) any {
	out := []any{}
	for _, x := range s.(util.Set).ToList() {
		o := f.(func(any) any)(x).(util.Optional)
		if !o.IsPresent() {
			return util.None()
		}
		out = append(out, o.Get())
	}
	return util.Some(util.NewSet(out...))
}

// Given : a -> optional<a>
func Given(x any) any { return util.Some(x) }

// ToList : optional<a> -> list<a>
func ToList(mx any) any {
	x := mx.(util.Optional)
	if !x.IsPresent() {
		return []any{}
	}
	return []any{x.Get()}
}
