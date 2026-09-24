package util

// Pair is Hydra's pair<A, B>. Pairs compare lexicographically: by first
// component, then by second (see comparison.go).
type Pair struct {
	First  any
	Second any
}

// NewPair constructs a pair from its two components.
func NewPair(first, second any) Pair { return Pair{First: first, Second: second} }
