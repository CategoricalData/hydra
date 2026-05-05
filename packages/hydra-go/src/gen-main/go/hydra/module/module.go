// Note: this is an automatically generated file. Do not edit.

package hmodule

import "hydra.dev/hydra/core"

type Definition interface {
  isDefinition()
}

type DefinitionTerm struct {
  Value TermDefinition
}

func (DefinitionTerm) isDefinition() {

}

type DefinitionType_ struct {
  Value TypeDefinition
}

func (DefinitionType_) isDefinition() {

}

type FileExtension string

type Library struct {
  Namespace Namespace
  Prefix string
  Primitives []any
}

type Module struct {
  Namespace Namespace
  Elements []any
  TermDependencies []any
  TypeDependencies []any
  Description any
}

type Namespace string

type Namespaces [N any] struct {
  Focus any
  Mapping []any
}

type QualifiedName struct {
  Namespace any
  Local string
}

type TermDefinition struct {
  Name core.Name
  Term core.Term
  Type_ core.TypeScheme
}

type TypeDefinition struct {
  Name core.Name
  Type_ core.Type
}
