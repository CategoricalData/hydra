// Note: this is an automatically generated file. Do not edit.

package logic

// hydra.lib.logic.and : Boolean -> Boolean -> Boolean
func And(a any) any {
	return func(b any) any {
		return a.(bool) && b.(bool)
	}
}

// hydra.lib.logic.or : Boolean -> Boolean -> Boolean
func Or(a any) any {
	return func(b any) any {
		return a.(bool) || b.(bool)
	}
}

// hydra.lib.logic.not : Boolean -> Boolean
func Not(a any) any {
	return !a.(bool)
}

// hydra.lib.logic.ifElse : a -> a -> Boolean -> a
func IfElse(thenVal any) any {
	return func(elseVal any) any {
		return func(cond any) any {
			if cond.(bool) {
				return thenVal
			}
			return elseVal
		}
	}
}
