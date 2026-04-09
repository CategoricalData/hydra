// Note: this is an automatically generated file. Do not edit.

package coders

import (
  "hydra.dev/hydra/core"
  "hydra.dev/hydra/graph"
  "hydra.dev/hydra/util"
)

type AdapterContext struct {
  Graph graph.Graph
  Language Language
  Adapters []any
}

type CoderDirection interface {
  isCoderDirection()
}

type CoderDirectionEncode struct{}

func (CoderDirectionEncode) isCoderDirection() {

}

type CoderDirectionDecode struct{}

func (CoderDirectionDecode) isCoderDirection() {

}

type Language struct {
  Name LanguageName
  Constraints LanguageConstraints
}

type LanguageConstraints struct {
  EliminationVariants []any
  LiteralVariants []any
  FloatTypes []any
  FunctionVariants []any
  IntegerTypes []any
  TermVariants []any
  TypeVariants []any
  Types func(core.Type) bool
}

type LanguageName string

type SymmetricAdapter [T, V any] util.Adapter[T, T, V, V]

type TraversalOrder interface {
  isTraversalOrder()
}

type TraversalOrderPre struct{}

func (TraversalOrderPre) isTraversalOrder() {

}

type TraversalOrderPost struct{}

func (TraversalOrderPost) isTraversalOrder() {

}

type TypeAdapter func(AdapterContext) func(core.Type) any
