package hydra.error.checking

import hydra.core.*

import hydra.paths.*

import hydra.typing.*

import hydra.variants.*

import hydra.core

import hydra.paths

import hydra.typing

import hydra.variants

enum CheckingError :
   case incorrectUnification(value: hydra.error.checking.IncorrectUnificationError) extends CheckingError
   case notAForallType(value: hydra.error.checking.NotAForallTypeError) extends CheckingError
   case notAFunctionType(value: hydra.error.checking.NotAFunctionTypeError) extends CheckingError
   case other(value: hydra.error.checking.OtherCheckingError) extends CheckingError
   case typeArityMismatch(value: hydra.error.checking.TypeArityMismatchError) extends CheckingError
   case typeMismatch(value: hydra.error.checking.TypeMismatchError) extends CheckingError
   case unboundTypeVariables(value: hydra.error.checking.UnboundTypeVariablesError) extends CheckingError
   case undefinedTermVariable(value: hydra.error.checking.UndefinedTermVariableCheckingError) extends CheckingError
   case unequalTypes(value: hydra.error.checking.UnequalTypesError) extends CheckingError
   case unsupportedTermVariant(value: hydra.error.checking.UnsupportedTermVariantError) extends CheckingError
   case untypedLambda(value: hydra.error.checking.UntypedLambdaError) extends CheckingError
   case untypedLetBinding(value: hydra.error.checking.UntypedLetBindingError) extends CheckingError
   case untypedTermVariable(value: hydra.error.checking.UntypedTermVariableCheckingError) extends CheckingError

case class IncorrectUnificationError(substitution: hydra.typing.TypeSubst)

case class NotAForallTypeError(`type`: hydra.core.Type, typeArguments: Seq[hydra.core.Type])

case class NotAFunctionTypeError(`type`: hydra.core.Type)

case class OtherCheckingError(path: hydra.paths.SubtermPath, message: scala.Predef.String)

case class TypeArityMismatchError(`type`: hydra.core.Type, expectedArity: Int, actualArity: Int, typeArguments: Seq[hydra.core.Type])

case class TypeMismatchError(expectedType: hydra.core.Type, actualType: hydra.core.Type)

case class UnboundTypeVariablesError(variables: scala.collection.immutable.Set[hydra.core.Name], `type`: hydra.core.Type)

case class UndefinedTermVariableCheckingError(path: hydra.paths.SubtermPath, name: hydra.core.Name)

case class UnequalTypesError(types: Seq[hydra.core.Type], description: scala.Predef.String)

case class UnsupportedTermVariantError(termVariant: hydra.variants.TermVariant)

case class UntypedLambdaError()

case class UntypedLetBindingError(binding: hydra.core.Binding)

case class UntypedTermVariableCheckingError(path: hydra.paths.SubtermPath, name: hydra.core.Name)
