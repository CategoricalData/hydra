package util

import (
	"math"
	"math/big"
	"testing"
)

// corpus is a heterogeneous-but-per-type-grouped sample of runtime values used
// to exercise the total-order laws. Each inner group holds values of ONE Hydra
// type (Compare is only defined within a type), and the groups are ordered so
// the corpus also documents cross-constructor expectations where they share a
// Go representation.
func corpusGroups() [][]any {
	return [][]any{
		{false, true},
		{int32(-2), int32(-1), int32(0), int32(1), int32(2)},
		{uint8(0), uint8(1), uint8(255)},
		{big.NewInt(-1000000000000), big.NewInt(-1), big.NewInt(0), big.NewInt(1), big.NewInt(1000000000000)},
		// Float special values, in the spec's total order:
		// -Inf < neg finite < -0.0 < +0.0 < pos finite < +Inf < NaN.
		{math.Inf(-1), -1.5, math.Copysign(0, -1), 0.0, 1.5, math.Inf(1), math.NaN()},
		{"", "a", "ab", "b", "á"},
		{[]byte{}, []byte{0}, []byte{0, 1}, []byte{1}},
		{None(), Some(int32(1)), Some(int32(2))},
		{Left(int32(1)), Left(int32(2)), Right(int32(0)), Right(int32(5))},
		{NewPair(int32(1), int32(1)), NewPair(int32(1), int32(2)), NewPair(int32(2), int32(0))},
		{NewSet(), NewSet(int32(1)), NewSet(int32(1), int32(2)), NewSet(int32(2))},
		{EmptyMap(), NewMap(NewPair(int32(1), "a")), NewMap(NewPair(int32(1), "b")), NewMap(NewPair(int32(2), "a"))},
		// Lists (slices): lexicographic, prefix least.
		{[]any{}, []any{int32(1)}, []any{int32(1), int32(2)}, []any{int32(2)}},
	}
}

// TestTotalOrderLaws checks reflexivity, antisymmetry, transitivity, totality,
// and agreement with Equal, over every pair within each type group.
func TestTotalOrderLaws(t *testing.T) {
	for gi, group := range corpusGroups() {
		for _, x := range group {
			for _, y := range group {
				cxy, cyx := Compare(x, y), Compare(y, x)
				// Antisymmetry: Compare(x,y) == -Compare(y,x).
				if cxy != -cyx {
					t.Fatalf("group %d: antisymmetry violated: Compare=%d but reverse=%d (x=%v y=%v)", gi, cxy, cyx, x, y)
				}
				// Agreement with Equal.
				if (cxy == 0) != Equal(x, y) {
					t.Fatalf("group %d: Compare/Equal disagree (x=%v y=%v)", gi, x, y)
				}
				for _, z := range group {
					cyz, cxz := Compare(y, z), Compare(x, z)
					// Transitivity of <=.
					if cxy <= 0 && cyz <= 0 && !(cxz <= 0) {
						t.Fatalf("group %d: transitivity violated (x=%v y=%v z=%v)", gi, x, y, z)
					}
				}
			}
			// Reflexivity.
			if Compare(x, x) != 0 {
				t.Fatalf("group %d: reflexivity violated for %v", gi, x)
			}
		}
	}
}

// TestOrderingIsAscending checks each group is presented in ascending order, so
// the corpus doubles as a golden ordering per the spec.
func TestOrderingIsAscending(t *testing.T) {
	for gi, group := range corpusGroups() {
		for i := 0; i+1 < len(group); i++ {
			if Compare(group[i], group[i+1]) >= 0 {
				t.Fatalf("group %d: not ascending at %d: %v !< %v", gi, i, group[i], group[i+1])
			}
		}
	}
}

// TestFloatSpecialValues pins the exact spec cases: equal NaN NaN is true,
// equal -0.0 0.0 is false, and NaN sorts greatest.
func TestFloatSpecialValues(t *testing.T) {
	nan := math.NaN()
	if !Equal(nan, nan) {
		t.Fatal("Equal(NaN, NaN) must be true")
	}
	if Equal(math.Copysign(0, -1), 0.0) {
		t.Fatal("Equal(-0.0, 0.0) must be false")
	}
	if Compare(nan, math.Inf(1)) <= 0 {
		t.Fatal("NaN must compare greater than +Inf")
	}
	if Compare(math.Copysign(0, -1), 0.0) >= 0 {
		t.Fatal("-0.0 must compare less than +0.0")
	}
}

// TestSetDedupAndOrder checks set construction discards duplicates and orders.
func TestSetDedupAndOrder(t *testing.T) {
	s := NewSet(int32(3), int32(1), int32(2), int32(1), int32(3))
	if s.Size() != 3 {
		t.Fatalf("expected 3 unique elements, got %d", s.Size())
	}
	got := s.ToList()
	want := []any{int32(1), int32(2), int32(3)}
	for i := range want {
		if Compare(got[i], want[i]) != 0 {
			t.Fatalf("set order wrong at %d: got %v want %v", i, got[i], want[i])
		}
	}
}

// TestMapReplaceAndOrder checks last-write-wins and ascending-key iteration.
func TestMapReplaceAndOrder(t *testing.T) {
	m := NewMap(NewPair(int32(2), "two"), NewPair(int32(1), "one"), NewPair(int32(2), "TWO"))
	if m.Size() != 2 {
		t.Fatalf("expected 2 bindings, got %d", m.Size())
	}
	if v := m.Lookup(int32(2)); !v.IsPresent() || v.Get().(string) != "TWO" {
		t.Fatalf("expected last-write-wins TWO, got %v", v)
	}
	keys := m.Keys()
	if Compare(keys[0], int32(1)) != 0 || Compare(keys[1], int32(2)) != 0 {
		t.Fatalf("keys not ascending: %v", keys)
	}
}
