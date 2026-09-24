// Package logic implements the hydra.core.lib.logic primitives.
//
// All primitives take and return `any`, matching the Go coder's calling
// convention (Native Contract v1): the coder emits saturated multi-arg calls
// with call-site type assertions, and passes lazy-position arguments as
// zero-argument thunks (func() any). ifElse is the only lazy primitive here;
// its two branches are thunks, and exactly one is forced.
package logic

// And : boolean -> boolean -> boolean
func And(a any, b any) any { return a.(bool) && b.(bool) }

// Not : boolean -> boolean
func Not(a any) any { return !a.(bool) }

// Or : boolean -> boolean -> boolean
func Or(a any, b any) any { return a.(bool) || b.(bool) }

// IfElse : boolean -> x -> x -> x. Arguments 1 and 2 are lazy: the coder passes
// them as func() any thunks, and only the taken branch is forced.
func IfElse(cond any, ifTrue any, ifFalse any) any {
	if cond.(bool) {
		return ifTrue.(func() any)()
	}
	return ifFalse.(func() any)()
}
