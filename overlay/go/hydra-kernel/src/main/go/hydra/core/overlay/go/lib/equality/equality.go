// Package equality implements the hydra.core.lib.equality primitives.
//
// Every ordering/equality operation routes through util.Compare, the canonical
// total order (docs/specification/ordering-and-equality.md); none lowers to
// Go's native == or < (which mis-handle NaN and signed zero). The comparison
// result is reported as the generated hydra.core.util.Comparison enum, which the
// kernel type-switches on.
package equality

import (
	"hydra.dev/hydra/overlay/go/util"
	genutil "hydra.dev/hydra/util"
)

// Compare : x -> x -> Comparison
func Compare(a any, b any) any {
	switch c := util.Compare(a, b); {
	case c < 0:
		return genutil.ComparisonLessThan{}
	case c > 0:
		return genutil.ComparisonGreaterThan{}
	default:
		return genutil.ComparisonEqualTo{}
	}
}

// Equal : x -> x -> boolean
func Equal(a any, b any) any { return util.Compare(a, b) == 0 }

// NotEqual : x -> x -> boolean
func NotEqual(a any, b any) any { return util.Compare(a, b) != 0 }

// Gt : x -> x -> boolean
func Gt(a any, b any) any { return util.Compare(a, b) > 0 }

// Gte : x -> x -> boolean
func Gte(a any, b any) any { return util.Compare(a, b) >= 0 }

// Lt : x -> x -> boolean
func Lt(a any, b any) any { return util.Compare(a, b) < 0 }

// Lte : x -> x -> boolean
func Lte(a any, b any) any { return util.Compare(a, b) <= 0 }

// Identity : x -> x
func Identity(a any) any { return a }

// Max : x -> x -> x
func Max(a any, b any) any {
	if util.Compare(a, b) >= 0 {
		return a
	}
	return b
}

// Min : x -> x -> x
func Min(a any, b any) any {
	if util.Compare(a, b) <= 0 {
		return a
	}
	return b
}
