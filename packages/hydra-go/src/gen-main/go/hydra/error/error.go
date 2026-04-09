// Note: this is an automatically generated file. Do not edit.

package error

import (
  "hydra.dev/hydra/core"
  "hydra.dev/hydra/typing"
  "hydra.dev/hydra/variants"
)

type CheckingError interface {
  isCheckingError()
}

type CheckingErrorIncorrectUnification struct {
  Value IncorrectUnificationError
}

func (CheckingErrorIncorrectUnification) isCheckingError() {

}

type CheckingErrorNotAForallType struct {
  Value NotAForallTypeError
}

func (CheckingErrorNotAForallType) isCheckingError() {

}

type CheckingErrorNotAFunctionType struct {
  Value NotAFunctionTypeError
}

func (CheckingErrorNotAFunctionType) isCheckingError() {

}

type CheckingErrorTypeArityMismatch struct {
  Value TypeArityMismatchError
}

func (CheckingErrorTypeArityMismatch) isCheckingError() {

}

type CheckingErrorTypeMismatch struct {
  Value TypeMismatchError
}

func (CheckingErrorTypeMismatch) isCheckingError() {

}

type CheckingErrorUnboundTypeVariables struct {
  Value UnboundTypeVariablesError
}

func (CheckingErrorUnboundTypeVariables) isCheckingError() {

}

type CheckingErrorUnequalTypes struct {
  Value UnequalTypesError
}

func (CheckingErrorUnequalTypes) isCheckingError() {

}

type CheckingErrorUnsupportedTermVariant struct {
  Value UnsupportedTermVariantError
}

func (CheckingErrorUnsupportedTermVariant) isCheckingError() {

}

type CheckingErrorUntypedLambda struct {
  Value UntypedLambdaError
}

func (CheckingErrorUntypedLambda) isCheckingError() {

}

type CheckingErrorUntypedLetBinding struct {
  Value UntypedLetBindingError
}

func (CheckingErrorUntypedLetBinding) isCheckingError() {

}

type DecodingError string

type DuplicateBindingError struct {
  Name core.Name
}

type DuplicateFieldError struct {
  Name core.Name
}

type Error interface {
  isError()
}

type ErrorChecking struct {
  Value CheckingError
}

func (ErrorChecking) isError() {

}

type ErrorDecoding struct {
  Value DecodingError
}

func (ErrorDecoding) isError() {

}

type ErrorDuplicateBinding struct {
  Value DuplicateBindingError
}

func (ErrorDuplicateBinding) isError() {

}

type ErrorDuplicateField struct {
  Value DuplicateFieldError
}

func (ErrorDuplicateField) isError() {

}

type ErrorOther struct {
  Value OtherError
}

func (ErrorOther) isError() {

}

type ErrorUndefinedField struct {
  Value UndefinedFieldError
}

func (ErrorUndefinedField) isError() {

}

type ErrorUndefinedTerm struct {
  Value UndefinedTermError
}

func (ErrorUndefinedTerm) isError() {

}

type ErrorUndefinedType struct {
  Value UndefinedTypeError
}

func (ErrorUndefinedType) isError() {

}

type ErrorUnexpectedTermVariant struct {
  Value UnexpectedTermVariantError
}

func (ErrorUnexpectedTermVariant) isError() {

}

type ErrorUnexpectedTypeVariant struct {
  Value UnexpectedTypeVariantError
}

func (ErrorUnexpectedTypeVariant) isError() {

}

type ErrorUnification struct {
  Value UnificationError
}

func (ErrorUnification) isError() {

}

type IncorrectUnificationError struct {
  Substitution typing.TypeSubst
}

type NotAForallTypeError struct {
  Type_ core.Type
  TypeArguments []any
}

type NotAFunctionTypeError struct {
  Type_ core.Type
}

type OtherError string

type TypeArityMismatchError struct {
  Type_ core.Type
  ExpectedArity int32
  ActualArity int32
  TypeArguments []any
}

type TypeMismatchError struct {
  ExpectedType core.Type
  ActualType core.Type
}

type UnboundTypeVariablesError struct {
  Variables []any
  Type_ core.Type
}

type UndefinedFieldError struct {
  FieldName core.Name
  TypeName core.Name
}

type UndefinedTermError struct {
  Name core.Name
}

type UndefinedTypeError struct {
  Name core.Name
}

type UnequalTypesError struct {
  Types []any
  Description string
}

type UnexpectedTermVariantError struct {
  ExpectedVariant variants.TermVariant
  ActualTerm core.Term
}

type UnexpectedTypeVariantError struct {
  ExpectedVariant variants.TypeVariant
  ActualType core.Type
}

type UnificationError struct {
  LeftType core.Type
  RightType core.Type
  Message string
}

type UnsupportedTermVariantError struct {
  TermVariant variants.TermVariant
}

type UntypedLambdaError struct{}

type UntypedLetBindingError struct {
  Binding core.Binding
}
