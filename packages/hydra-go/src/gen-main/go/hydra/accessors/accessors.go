// Note: this is an automatically generated file. Do not edit.

package accessors

import "hydra.dev/hydra/core"

type AccessorEdge struct {
  Source AccessorNode
  Path AccessorPath
  Target AccessorNode
}

type AccessorGraph struct {
  Nodes []any
  Edges []any
}

type AccessorNode struct {
  Name core.Name
  Label string
  Id string
}

type AccessorPath []any

type TermAccessor interface {
  isTermAccessor()
}

type TermAccessorAnnotatedBody struct{}

func (TermAccessorAnnotatedBody) isTermAccessor() {

}

type TermAccessorApplicationFunction struct{}

func (TermAccessorApplicationFunction) isTermAccessor() {

}

type TermAccessorApplicationArgument struct{}

func (TermAccessorApplicationArgument) isTermAccessor() {

}

type TermAccessorLambdaBody struct{}

func (TermAccessorLambdaBody) isTermAccessor() {

}

type TermAccessorUnionCasesDefault struct{}

func (TermAccessorUnionCasesDefault) isTermAccessor() {

}

type TermAccessorUnionCasesBranch struct {
  Value core.Name
}

func (TermAccessorUnionCasesBranch) isTermAccessor() {

}

type TermAccessorLetBody struct{}

func (TermAccessorLetBody) isTermAccessor() {

}

type TermAccessorLetBinding struct {
  Value core.Name
}

func (TermAccessorLetBinding) isTermAccessor() {

}

type TermAccessorListElement struct {
  Value int32
}

func (TermAccessorListElement) isTermAccessor() {

}

type TermAccessorMapKey struct {
  Value int32
}

func (TermAccessorMapKey) isTermAccessor() {

}

type TermAccessorMapValue struct {
  Value int32
}

func (TermAccessorMapValue) isTermAccessor() {

}

type TermAccessorMaybeTerm struct{}

func (TermAccessorMaybeTerm) isTermAccessor() {

}

type TermAccessorProductTerm struct {
  Value int32
}

func (TermAccessorProductTerm) isTermAccessor() {

}

type TermAccessorRecordField struct {
  Value core.Name
}

func (TermAccessorRecordField) isTermAccessor() {

}

type TermAccessorSetElement struct {
  Value int32
}

func (TermAccessorSetElement) isTermAccessor() {

}

type TermAccessorSumTerm struct{}

func (TermAccessorSumTerm) isTermAccessor() {

}

type TermAccessorTypeLambdaBody struct{}

func (TermAccessorTypeLambdaBody) isTermAccessor() {

}

type TermAccessorTypeApplicationTerm struct{}

func (TermAccessorTypeApplicationTerm) isTermAccessor() {

}

type TermAccessorInjectionTerm struct{}

func (TermAccessorInjectionTerm) isTermAccessor() {

}

type TermAccessorWrappedTerm struct{}

func (TermAccessorWrappedTerm) isTermAccessor() {

}
