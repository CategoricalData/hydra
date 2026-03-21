package hydra.error

import hydra.core.*

import hydra.typing.*

import hydra.variants.*

import hydra.core

import hydra.typing

import hydra.variants

enum CheckingError :
   case incorrectUnification(value: hydra.error.IncorrectUnificationError) extends CheckingError
   case notAForallType(value: hydra.error.NotAForallTypeError) extends CheckingError
   case notAFunctionType(value: hydra.error.NotAFunctionTypeError) extends CheckingError
   case typeArityMismatch(value: hydra.error.TypeArityMismatchError) extends CheckingError
   case typeMismatch(value: hydra.error.TypeMismatchError) extends CheckingError
   case unboundTypeVariables(value: hydra.error.UnboundTypeVariablesError) extends CheckingError
   case unequalTypes(value: hydra.error.UnequalTypesError) extends CheckingError
   case unsupportedTermVariant(value: hydra.error.UnsupportedTermVariantError) extends CheckingError
   case untypedLambda(value: hydra.error.UntypedLambdaError) extends CheckingError
   case untypedLetBinding(value: hydra.error.UntypedLetBindingError) extends CheckingError

type DecodingError = scala.Predef.String

case class DuplicateBindingError(name: hydra.core.Name)

case class DuplicateFieldError(name: hydra.core.Name)

enum Error :
   case checking(value: hydra.error.CheckingError) extends Error
   case decoding(value: hydra.error.DecodingError) extends Error
   case duplicateBinding(value: hydra.error.DuplicateBindingError) extends Error
   case duplicateField(value: hydra.error.DuplicateFieldError) extends Error
   case other(value: hydra.error.OtherError) extends Error
   case undefinedField(value: hydra.error.UndefinedFieldError) extends Error
   case undefinedTerm(value: hydra.error.UndefinedTermError) extends Error
   case undefinedType(value: hydra.error.UndefinedTypeError) extends Error
   case unexpectedTermVariant(value: hydra.error.UnexpectedTermVariantError) extends Error
   case unexpectedTypeVariant(value: hydra.error.UnexpectedTypeVariantError) extends Error
   case unification(value: hydra.error.UnificationError) extends Error

case class IncorrectUnificationError(substitution: hydra.typing.TypeSubst)

case class NotAForallTypeError(`type`: hydra.core.Type, typeArguments: Seq[hydra.core.Type])

case class NotAFunctionTypeError(`type`: hydra.core.Type)

type OtherError = scala.Predef.String

case class TypeArityMismatchError(`type`: hydra.core.Type, expectedArity: Int, actualArity: Int, typeArguments: Seq[hydra.core.Type])

case class TypeMismatchError(expectedType: hydra.core.Type, actualType: hydra.core.Type)

case class UnboundTypeVariablesError(variables: scala.collection.immutable.Set[hydra.core.Name], `type`: hydra.core.Type)

case class UndefinedFieldError(fieldName: hydra.core.Name, typeName: hydra.core.Name)

case class UndefinedTermError(name: hydra.core.Name)

case class UndefinedTypeError(name: hydra.core.Name)

case class UnequalTypesError(types: Seq[hydra.core.Type], description: scala.Predef.String)

case class UnexpectedTermVariantError(expectedVariant: hydra.variants.TermVariant, actualTerm: hydra.core.Term)

case class UnexpectedTypeVariantError(expectedVariant: hydra.variants.TypeVariant, actualType: hydra.core.Type)

case class UnificationError(leftType: hydra.core.Type, rightType: hydra.core.Type, message: scala.Predef.String)

case class UnsupportedTermVariantError(termVariant: hydra.variants.TermVariant)

case class UntypedLambdaError()

case class UntypedLetBindingError(binding: hydra.core.Binding)
