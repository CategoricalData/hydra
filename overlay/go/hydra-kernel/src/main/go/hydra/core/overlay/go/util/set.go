package util

import "sort"

// Set is Hydra's immutable set<T>, identified with the ascending sequence of
// its elements (see comparison.go). Like Map it is a persistent value: every
// operation returns a new Set and never mutates the receiver.
//
// The representation is a sorted, duplicate-free slice. NewSet sorts once;
// membership is a binary search; Insert/Delete copy the backing slice.
type Set struct {
	elements []any // sorted under Compare; no duplicates
}

// NewSet builds a Set from elements, discarding duplicates (an equal element
// adds nothing, matching Hydra's set semantics).
func NewSet(elements ...any) Set {
	es := make([]any, len(elements))
	copy(es, elements)
	sort.SliceStable(es, func(i, j int) bool { return Compare(es[i], es[j]) < 0 })
	out := es[:0]
	for i := 0; i < len(es); i++ {
		if len(out) > 0 && Compare(out[len(out)-1], es[i]) == 0 {
			continue
		}
		out = append(out, es[i])
	}
	return Set{elements: out}
}

// EmptySet is the set with no elements.
func EmptySet() Set { return Set{} }

func (s Set) search(x any) (int, bool) {
	i := sort.Search(len(s.elements), func(i int) bool {
		return Compare(s.elements[i], x) >= 0
	})
	if i < len(s.elements) && Compare(s.elements[i], x) == 0 {
		return i, true
	}
	return i, false
}

// Member reports whether x is in the set.
func (s Set) Member(x any) bool {
	_, ok := s.search(x)
	return ok
}

// Size returns the number of elements.
func (s Set) Size() int { return len(s.elements) }

// Insert returns a new Set containing x (a no-op if already present), leaving
// the receiver unchanged.
func (s Set) Insert(x any) Set {
	i, found := s.search(x)
	if found {
		return s
	}
	next := make([]any, 0, len(s.elements)+1)
	next = append(next, s.elements[:i]...)
	next = append(next, x)
	next = append(next, s.elements[i:]...)
	return Set{elements: next}
}

// Delete returns a new Set without x, leaving the receiver unchanged.
func (s Set) Delete(x any) Set {
	i, found := s.search(x)
	if !found {
		return s
	}
	next := make([]any, 0, len(s.elements)-1)
	next = append(next, s.elements[:i]...)
	next = append(next, s.elements[i+1:]...)
	return Set{elements: next}
}

// ToList returns the elements in ascending order.
func (s Set) ToList() []any {
	out := make([]any, len(s.elements))
	copy(out, s.elements)
	return out
}

// sortedElements exposes the sorted elements for the comparator.
func (s Set) sortedElements() []any { return s.elements }
