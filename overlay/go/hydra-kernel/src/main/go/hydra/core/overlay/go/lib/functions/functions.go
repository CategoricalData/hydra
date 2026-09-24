// Package functions implements the hydra.core.lib.functions primitives (basic
// combinators). Higher-order arguments arrive as `func(any) any` (the coder's
// uniform any-erased function representation); each combinator returns a closure
// in the same representation so it composes with the rest of the generated code.
package functions

// Absurd : void -> a. The void type is uninhabited, so this is unreachable by
// construction; reaching it means a void value was fabricated somewhere, which
// is a bug worth failing loudly on rather than papering over with a zero value.
func Absurd(v any) any {
	panic("hydra.core.lib.functions.absurd: called with a value of the uninhabited void type")
}

// Compose : (b -> c) -> (a -> b) -> a -> c
// Flat-called as Compose(f, g); returns the composed function f . g.
func Compose(f any, g any) any {
	ff := f.(func(any) any)
	gg := g.(func(any) any)
	return func(x any) any { return ff(gg(x)) }
}

// Const : a -> b -> a
// Flat-called as Const(a, b); returns the first argument, ignoring the second.
func Const(a any, b any) any { return a }

// Flip : (a -> b -> c) -> b -> a -> c
// Flat-called as Flip(f, b, a); applies f in swapped argument order. f is
// curried (func(a) func(b) c) in the any representation.
func Flip(f any, b any, a any) any {
	return f.(func(any) any)(a).(func(any) any)(b)
}

// Identity : a -> a
func Identity(a any) any { return a }
