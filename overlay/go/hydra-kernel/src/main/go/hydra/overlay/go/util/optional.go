// Package util provides Hydra's host-independent runtime data types for the Go
// head: Optional, Either, Pair, Unit, and the persistent Map/Set together with
// the canonical ordering (see comparison.go). These back the built-in type
// constructors that hydra.core does not define as records or unions, and every
// map/set iterates in the canonical ascending order defined by
// docs/specification/ordering-and-equality.md.
package util

// Optional is Hydra's optional<T>: an explicit presence flag plus a value.
//
// It is deliberately NOT a Go pointer. A pointer cannot distinguish none from
// given(none) once optionals nest, and it conflates absence with a nil payload.
// The zero value is a valid None.
type Optional struct {
	present bool
	value   any
}

// Some constructs a present optional wrapping v.
func Some(v any) Optional { return Optional{present: true, value: v} }

// None constructs an absent optional.
func None() Optional { return Optional{} }

// IsPresent reports whether the optional holds a value.
func (o Optional) IsPresent() bool { return o.present }

// Get returns the wrapped value; it panics if the optional is absent, which
// indicates a coder or primitive-implementation bug rather than a data
// condition (Hydra code guards presence before projecting).
func (o Optional) Get() any {
	if !o.present {
		panic("util.Optional.Get on an absent optional")
	}
	return o.value
}

// OrElse returns the wrapped value if present, else the supplied default.
func (o Optional) OrElse(def any) any {
	if o.present {
		return o.value
	}
	return def
}
