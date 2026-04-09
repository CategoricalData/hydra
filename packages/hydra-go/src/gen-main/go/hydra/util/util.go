// Note: this is an automatically generated file. Do not edit.

package util

import "hydra.dev/hydra/context"

type Adapter [T1, T2, V1, V2 any] struct {
  IsLossy bool
  Source T1
  Target T2
  Coder Coder[V1, V2]
}

type Bicoder [T1, T2, V1, V2 any] struct {
  Encode func(T1) Adapter[T1, T2, V1, V2]
  Decode func(T2) Adapter[T2, T1, V2, V1]
}

type CaseConvention interface {
  isCaseConvention()
}

type CaseConventionCamel struct{}

func (CaseConventionCamel) isCaseConvention() {

}

type CaseConventionPascal struct{}

func (CaseConventionPascal) isCaseConvention() {

}

type CaseConventionLowerSnake struct{}

func (CaseConventionLowerSnake) isCaseConvention() {

}

type CaseConventionUpperSnake struct{}

func (CaseConventionUpperSnake) isCaseConvention() {

}

type Coder [V1, V2 any] struct {
  Encode func(context.Context) func(V1) any
  Decode func(context.Context) func(V2) any
}

type Comparison interface {
  isComparison()
}

type ComparisonLessThan struct{}

func (ComparisonLessThan) isComparison() {

}

type ComparisonEqualTo struct{}

func (ComparisonEqualTo) isComparison() {

}

type ComparisonGreaterThan struct{}

func (ComparisonGreaterThan) isComparison() {

}

type Precision interface {
  isPrecision()
}

type PrecisionArbitrary struct{}

func (PrecisionArbitrary) isPrecision() {

}

type PrecisionBits struct {
  Value int32
}

func (PrecisionBits) isPrecision() {

}
