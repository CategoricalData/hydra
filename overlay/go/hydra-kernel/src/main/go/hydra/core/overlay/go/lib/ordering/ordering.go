// Package ordering implements the hydra.core.lib.ordering primitives.
//
// Ordering routes through util.Compare, the canonical total order
// (docs/specification/ordering-and-equality.md); it never lowers to Go's native
// < or > (which mis-handle NaN and signed zero). `compare` reports its result as
// the generated hydra.core.util.Comparison enum, which the kernel type-switches on.
// These primitives were split out of hydra.core.lib.equality in #417/#566 (the
// numeric type class); the implementations mirror the equality overlay.
package ordering

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

// Gt : x -> x -> boolean
func Gt(a any, b any) any { return util.Compare(a, b) > 0 }

// Gte : x -> x -> boolean
func Gte(a any, b any) any { return util.Compare(a, b) >= 0 }

// Lt : x -> x -> boolean
func Lt(a any, b any) any { return util.Compare(a, b) < 0 }

// Lte : x -> x -> boolean
func Lte(a any, b any) any { return util.Compare(a, b) <= 0 }

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
