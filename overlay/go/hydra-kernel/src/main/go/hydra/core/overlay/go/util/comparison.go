package util

import (
	"math"
	"math/big"
	"reflect"
	"sort"
)

// Compare implements Hydra's canonical total order over runtime values, as
// specified by docs/specification/ordering-and-equality.md. It returns -1, 0,
// or +1 for a < b, a == b, a > b. Equality is Compare(a, b) == 0.
//
// Comparison is only defined between values of the same Hydra type; a typed
// Hydra program never compares across types, so the host representations
// reaching a given call site are homogeneous. Compare never lowers to Go's
// native == or < on floats (which mis-handle NaN and signed zero); it routes
// every case through the semantics below.
//
// This is the single most correctness-critical file in the Go runtime: the
// equality and ordering primitives, every map/set's iteration order, and the
// test runner's result comparison all depend on it. It is property-tested
// against the total-order laws in comparison_test.go.
func Compare(a, b any) int {
	// Annotations are transparent — but at the Go value level annotations are
	// already stripped (they live only in the Term data, compared structurally
	// like any other union). So no special handling is needed here.

	switch av := a.(type) {
	case nil:
		if b == nil {
			return 0
		}
		return -1

	case bool:
		return compareBool(av, b.(bool))

	// Signed and unsigned fixed-width integers compare by numeric value within
	// their own type. Distinct widths are distinct Hydra types and never meet.
	case int8:
		return compareInt64(int64(av), int64(b.(int8)))
	case int16:
		return compareInt64(int64(av), int64(b.(int16)))
	case int32:
		return compareInt64(int64(av), int64(b.(int32)))
	case int64:
		return compareInt64(av, b.(int64))
	case uint8:
		return compareUint64(uint64(av), uint64(b.(uint8)))
	case uint16:
		return compareUint64(uint64(av), uint64(b.(uint16)))
	case uint32:
		return compareUint64(uint64(av), uint64(b.(uint32)))
	case uint64:
		return compareUint64(av, b.(uint64))

	case *big.Int:
		return av.Cmp(b.(*big.Int))

	case float32:
		return compareFloat(float64(av), float64(b.(float32)))
	case float64:
		return compareFloat(av, b.(float64))

	case *big.Float:
		// Decimals: numeric value first, then scale (smaller scale first).
		// big.Float has no scale concept; the decimal representation carries it
		// elsewhere. Until a dedicated Decimal type lands, compare by value.
		return av.Cmp(b.(*big.Float))

	case string:
		return compareString(av, b.(string))

	case []byte:
		return compareBytes(av, b.([]byte))

	case Unit:
		return 0

	case Optional:
		return compareOptional(av, b.(Optional))

	case Either:
		return compareEither(av, b.(Either))

	case Pair:
		bp := b.(Pair)
		if c := Compare(av.First, bp.First); c != 0 {
			return c
		}
		return Compare(av.Second, bp.Second)

	case Map:
		return compareMap(av, b.(Map))

	case Set:
		return compareSet(av, b.(Set))
	}

	// Lists are []T for varying T; handle any slice reflectively, plus the
	// general structural fallback for record/union structs.
	return compareReflect(a, b)
}

// Equal is the equivalence induced by Compare.
func Equal(a, b any) bool { return Compare(a, b) == 0 }

func compareBool(a, b bool) int {
	if a == b {
		return 0
	}
	if !a { // false < true
		return -1
	}
	return 1
}

func compareInt64(a, b int64) int {
	switch {
	case a < b:
		return -1
	case a > b:
		return 1
	default:
		return 0
	}
}

func compareUint64(a, b uint64) int {
	switch {
	case a < b:
		return -1
	case a > b:
		return 1
	default:
		return 0
	}
}

// compareFloat implements IEEE 754 §5.10 totalOrder restricted to Hydra's
// single-NaN value space:
//
//	-Inf < negative finite < -0.0 < +0.0 < positive finite < +Inf < NaN
//
// with NaN equal to itself and -0.0 distinct from +0.0.
func compareFloat(a, b float64) int {
	aNaN, bNaN := math.IsNaN(a), math.IsNaN(b)
	if aNaN || bNaN {
		if aNaN && bNaN {
			return 0
		}
		if aNaN { // NaN is greatest
			return 1
		}
		return -1
	}
	if a < b {
		return -1
	}
	if a > b {
		return 1
	}
	// a == b numerically: disambiguate signed zero via the sign bit so that
	// -0.0 < +0.0 (Signbit(-0.0) is true).
	as, bs := math.Signbit(a), math.Signbit(b)
	if as == bs {
		return 0
	}
	if as { // -0.0 < +0.0
		return -1
	}
	return 1
}

func compareString(a, b string) int {
	// Strings compare lexicographically by Unicode code point. Go string
	// comparison is by byte, and UTF-8 byte order coincides with code-point
	// order, so the native comparison is correct here (proper prefix least).
	switch {
	case a < b:
		return -1
	case a > b:
		return 1
	default:
		return 0
	}
}

func compareBytes(a, b []byte) int {
	n := len(a)
	if len(b) < n {
		n = len(b)
	}
	for i := 0; i < n; i++ {
		if a[i] != b[i] {
			if a[i] < b[i] {
				return -1
			}
			return 1
		}
	}
	return compareInt64(int64(len(a)), int64(len(b)))
}

func compareOptional(a, b Optional) int {
	// none < every given x; given x and given y compare as x and y.
	if !a.present || !b.present {
		if a.present == b.present {
			return 0
		}
		if !a.present {
			return -1
		}
		return 1
	}
	return Compare(a.value, b.value)
}

func compareEither(a, b Either) int {
	// every left < every right; same side compares by payload.
	if a.isRight != b.isRight {
		if !a.isRight {
			return -1
		}
		return 1
	}
	return Compare(a.value, b.value)
}

func compareMap(a, b Map) int {
	// Maps compare as their ascending-key binding sequences: each binding by
	// key, then value.
	ae, be := a.sortedEntries(), b.sortedEntries()
	n := len(ae)
	if len(be) < n {
		n = len(be)
	}
	for i := 0; i < n; i++ {
		if c := Compare(ae[i].key, be[i].key); c != 0 {
			return c
		}
		if c := Compare(ae[i].value, be[i].value); c != 0 {
			return c
		}
	}
	return compareInt64(int64(len(ae)), int64(len(be)))
}

func compareSet(a, b Set) int {
	// Sets compare as their ascending element sequences, prefix least.
	ae, be := a.sortedElements(), b.sortedElements()
	n := len(ae)
	if len(be) < n {
		n = len(be)
	}
	for i := 0; i < n; i++ {
		if c := Compare(ae[i], be[i]); c != 0 {
			return c
		}
	}
	return compareInt64(int64(len(ae)), int64(len(be)))
}

// compareReflect handles the two remaining shapes structurally:
//   - slices (Hydra lists): lexicographic, prefix least;
//   - structs (record types, and union variant structs): field by field in
//     declaration order. Two values reaching here share a Hydra type, hence the
//     same Go concrete type; a differing concrete type can only be two variants
//     of one union, ordered by declaration index via the variant registry.
func compareReflect(a, b any) int {
	av, bv := reflect.ValueOf(a), reflect.ValueOf(b)
	at, bt := av.Type(), bv.Type()

	if at != bt {
		// Distinct concrete types under one Hydra type ⇒ distinct union
		// variants. Order by declaration index (earlier variant is lesser).
		return compareInt64(int64(variantIndex(at)), int64(variantIndex(bt)))
	}

	switch av.Kind() {
	case reflect.Slice:
		n := av.Len()
		if bv.Len() < n {
			n = bv.Len()
		}
		for i := 0; i < n; i++ {
			if c := Compare(av.Index(i).Interface(), bv.Index(i).Interface()); c != 0 {
				return c
			}
		}
		return compareInt64(int64(av.Len()), int64(bv.Len()))

	case reflect.Struct:
		for i := 0; i < av.NumField(); i++ {
			if c := Compare(av.Field(i).Interface(), bv.Field(i).Interface()); c != 0 {
				return c
			}
		}
		return 0

	case reflect.Pointer:
		if av.IsNil() || bv.IsNil() {
			an, bn := av.IsNil(), bv.IsNil()
			if an == bn {
				return 0
			}
			if an {
				return -1
			}
			return 1
		}
		return Compare(av.Elem().Interface(), bv.Elem().Interface())

	case reflect.String:
		return compareString(av.String(), bv.String())
	}

	panic("util.Compare: unorderable value of type " + at.String())
}

// SortAny sorts a slice of values in ascending canonical order, in place and
// stably.
func SortAny(xs []any) {
	sort.SliceStable(xs, func(i, j int) bool { return Compare(xs[i], xs[j]) < 0 })
}

// SortByKey stably sorts xs in place by the ascending canonical order of the
// parallel keys slice (keys[i] is the sort key of xs[i]). len(keys) must equal
// len(xs).
func SortByKey(xs []any, keys []any) {
	idx := make([]int, len(xs))
	for i := range idx {
		idx[i] = i
	}
	sort.SliceStable(idx, func(i, j int) bool { return Compare(keys[idx[i]], keys[idx[j]]) < 0 })
	out := make([]any, len(xs))
	for i, k := range idx {
		out[i] = xs[k]
	}
	copy(xs, out)
}
