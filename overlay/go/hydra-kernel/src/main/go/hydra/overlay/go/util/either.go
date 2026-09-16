package util

// Either is Hydra's either<L, R>: a tagged value that is either a left or a
// right. The zero value is a left holding nil; constructors should always be
// used. Under the canonical order every left precedes every right
// (see comparison.go).
type Either struct {
	isRight bool
	value   any
}

// Left constructs a left-tagged either.
func Left(v any) Either { return Either{isRight: false, value: v} }

// Right constructs a right-tagged either.
func Right(v any) Either { return Either{isRight: true, value: v} }

// IsLeft reports whether the either is left-tagged.
func (e Either) IsLeft() bool { return !e.isRight }

// IsRight reports whether the either is right-tagged.
func (e Either) IsRight() bool { return e.isRight }

// Value returns the wrapped value regardless of side.
func (e Either) Value() any { return e.value }
