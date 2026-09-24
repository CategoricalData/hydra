package util

import "reflect"

// The canonical order compares two injections of a union type by the
// declaration order of their variants (an earlier-declared variant is lesser;
// see docs/specification/ordering-and-equality.md). Go reflection cannot
// recover a variant struct's declaration position from its type alone, so the
// generated code registers, per union, the ordered list of its variant struct
// types. compareReflect consults variantIndex when two values under one Hydra
// type have different Go concrete types.

var variantIndexByType = map[reflect.Type]int{}

// RegisterVariants records the declaration order of a union's variant structs.
// The generated code calls this once per union type (in an init function),
// passing zero values of each variant struct in declaration order. Indices are
// assigned per registration call, so each union's variants are numbered
// independently — which is sufficient because only variants of the SAME union
// are ever compared against each other.
func RegisterVariants(variants ...any) {
	for i, v := range variants {
		variantIndexByType[reflect.TypeOf(v)] = i
	}
}

// variantIndex returns the registered declaration index of a variant struct
// type, or -1 if unregistered (which indicates the generator failed to emit a
// registration for this union).
func variantIndex(t reflect.Type) int {
	if idx, ok := variantIndexByType[t]; ok {
		return idx
	}
	panic("util.Compare: no variant index registered for " + t.String() +
		" (generator must emit util.RegisterVariants for its union)")
}
