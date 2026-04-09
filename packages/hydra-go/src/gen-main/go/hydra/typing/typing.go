// Note: this is an automatically generated file. Do not edit.

package typing

import (
  "hydra.dev/hydra/context"
  "hydra.dev/hydra/core"
)

type FunctionStructure [Env any] struct {
  TypeParams []any
  Params []any
  Bindings []any
  Body core.Term
  Domains []any
  Codomain any
  Environment Env
}

type InferenceResult struct {
  Term core.Term
  Type_ core.Type
  Subst TypeSubst
  ClassConstraints []any
  Context context.Context
}

type TermSubst []any

type TypeConstraint struct {
  Left core.Type
  Right core.Type
  Comment string
}

type TypeSubst []any
