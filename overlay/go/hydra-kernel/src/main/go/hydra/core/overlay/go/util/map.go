package util

import "sort"

// Map is Hydra's immutable map<K, V>, keyed and iterated in the canonical
// ascending key order (see comparison.go). It is a persistent value: every
// operation returns a new Map and never mutates the receiver, so a Map may be
// shared freely.
//
// The representation is a slice of key-sorted entries. Construction sorts once
// (NewMap), and lookups are binary searches; single-key Insert/Delete copy the
// backing slice. This keeps the common codegen pattern — build a whole map from
// a literal — at O(n log n) rather than the O(n^2) of repeated tree inserts.
type Map struct {
	entries []mapEntry // sorted by key under Compare; no duplicate keys
}

type mapEntry struct {
	key   any
	value any
}

// NewMap builds a Map from a set of key/value pairs. Later pairs with an equal
// key replace earlier ones, matching Hydra's map-insertion semantics.
func NewMap(pairs ...Pair) Map {
	es := make([]mapEntry, 0, len(pairs))
	for _, p := range pairs {
		es = append(es, mapEntry{key: p.First, value: p.Second})
	}
	sort.SliceStable(es, func(i, j int) bool { return Compare(es[i].key, es[j].key) < 0 })
	// Deduplicate keys, keeping the last occurrence (stable sort preserves the
	// input order among equal keys).
	out := es[:0]
	for i := 0; i < len(es); i++ {
		if len(out) > 0 && Compare(out[len(out)-1].key, es[i].key) == 0 {
			out[len(out)-1] = es[i]
			continue
		}
		out = append(out, es[i])
	}
	return Map{entries: out}
}

// EmptyMap is the map with no bindings.
func EmptyMap() Map { return Map{} }

// search returns the index of key and whether it was found.
func (m Map) search(key any) (int, bool) {
	i := sort.Search(len(m.entries), func(i int) bool {
		return Compare(m.entries[i].key, key) >= 0
	})
	if i < len(m.entries) && Compare(m.entries[i].key, key) == 0 {
		return i, true
	}
	return i, false
}

// Lookup returns the value bound to key, or an absent Optional.
func (m Map) Lookup(key any) Optional {
	if i, ok := m.search(key); ok {
		return Some(m.entries[i].value)
	}
	return None()
}

// Member reports whether key is bound.
func (m Map) Member(key any) bool {
	_, ok := m.search(key)
	return ok
}

// Size returns the number of bindings.
func (m Map) Size() int { return len(m.entries) }

// Insert returns a new Map with key bound to value (replacing any existing
// binding), leaving the receiver unchanged.
func (m Map) Insert(key, value any) Map {
	i, found := m.search(key)
	if found {
		next := make([]mapEntry, len(m.entries))
		copy(next, m.entries)
		next[i].value = value
		return Map{entries: next}
	}
	next := make([]mapEntry, 0, len(m.entries)+1)
	next = append(next, m.entries[:i]...)
	next = append(next, mapEntry{key: key, value: value})
	next = append(next, m.entries[i:]...)
	return Map{entries: next}
}

// Delete returns a new Map without key, leaving the receiver unchanged.
func (m Map) Delete(key any) Map {
	i, found := m.search(key)
	if !found {
		return m
	}
	next := make([]mapEntry, 0, len(m.entries)-1)
	next = append(next, m.entries[:i]...)
	next = append(next, m.entries[i+1:]...)
	return Map{entries: next}
}

// Keys returns the keys in ascending order.
func (m Map) Keys() []any {
	ks := make([]any, len(m.entries))
	for i, e := range m.entries {
		ks[i] = e.key
	}
	return ks
}

// Values returns the values ordered by their keys' ascending order.
func (m Map) Values() []any {
	vs := make([]any, len(m.entries))
	for i, e := range m.entries {
		vs[i] = e.value
	}
	return vs
}

// ToPairs returns the bindings as ascending-key Pairs.
func (m Map) ToPairs() []any {
	ps := make([]any, len(m.entries))
	for i, e := range m.entries {
		ps[i] = NewPair(e.key, e.value)
	}
	return ps
}

// sortedEntries exposes the key-sorted entries for the comparator.
func (m Map) sortedEntries() []mapEntry { return m.entries }
