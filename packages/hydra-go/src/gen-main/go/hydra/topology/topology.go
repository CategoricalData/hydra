// Note: this is an automatically generated file. Do not edit.

package topology

type Graph []any

type OrderingIsomorphism [A any] struct {
  Encode func([]any) []any
  Decode func([]any) []any
}

type TarjanState struct {
  Counter int32
  Indices []any
  LowLinks []any
  Stack []any
  OnStack []any
  Sccs []any
}

type Vertex int32
