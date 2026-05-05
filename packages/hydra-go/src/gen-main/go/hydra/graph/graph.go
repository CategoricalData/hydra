// Note: this is an automatically generated file. Do not edit.

package graph

import (
  "hydra.dev/hydra/context"
  "hydra.dev/hydra/core"
)

type Graph struct {
  BoundTerms []any
  BoundTypes []any
  ClassConstraints []any
  LambdaVariables []any
  Metadata []any
  Primitives []any
  SchemaTypes []any
  TypeVariables []any
}

type Primitive struct {
  Name core.Name
  Type_ core.TypeScheme
  Implementation func(context.Context) func(Graph) func([]any) any
}

type TermCoder [A any] struct {
  Type_ core.Type
  Encode func(context.Context) func(Graph) func(core.Term) any
  Decode func(context.Context) func(A) any
}
