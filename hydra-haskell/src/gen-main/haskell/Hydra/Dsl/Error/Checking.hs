-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.error.checking

module Hydra.Dsl.Error.Checking where

import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Typing as Typing
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

checkingErrorIncorrectUnification :: Phantoms.TTerm Checking.IncorrectUnificationError -> Phantoms.TTerm Checking.CheckingError
checkingErrorIncorrectUnification x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "incorrectUnification"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

checkingErrorNotAForallType :: Phantoms.TTerm Checking.NotAForallTypeError -> Phantoms.TTerm Checking.CheckingError
checkingErrorNotAForallType x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notAForallType"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

checkingErrorNotAFunctionType :: Phantoms.TTerm Checking.NotAFunctionTypeError -> Phantoms.TTerm Checking.CheckingError
checkingErrorNotAFunctionType x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notAFunctionType"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

checkingErrorTypeArityMismatch :: Phantoms.TTerm Checking.TypeArityMismatchError -> Phantoms.TTerm Checking.CheckingError
checkingErrorTypeArityMismatch x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeArityMismatch"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

checkingErrorTypeMismatch :: Phantoms.TTerm Checking.TypeMismatchError -> Phantoms.TTerm Checking.CheckingError
checkingErrorTypeMismatch x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeMismatch"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

checkingErrorUnboundTypeVariables :: Phantoms.TTerm Checking.UnboundTypeVariablesError -> Phantoms.TTerm Checking.CheckingError
checkingErrorUnboundTypeVariables x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unboundTypeVariables"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

checkingErrorUnequalTypes :: Phantoms.TTerm Checking.UnequalTypesError -> Phantoms.TTerm Checking.CheckingError
checkingErrorUnequalTypes x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unequalTypes"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

checkingErrorUnsupportedTermVariant :: Phantoms.TTerm Checking.UnsupportedTermVariantError -> Phantoms.TTerm Checking.CheckingError
checkingErrorUnsupportedTermVariant x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unsupportedTermVariant"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

checkingErrorUntypedLambda :: Phantoms.TTerm Checking.UntypedLambdaError -> Phantoms.TTerm Checking.CheckingError
checkingErrorUntypedLambda x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "untypedLambda"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

checkingErrorUntypedLetBinding :: Phantoms.TTerm Checking.UntypedLetBindingError -> Phantoms.TTerm Checking.CheckingError
checkingErrorUntypedLetBinding x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "untypedLetBinding"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

incorrectUnificationError :: Phantoms.TTerm Typing.TypeSubst -> Phantoms.TTerm Checking.IncorrectUnificationError
incorrectUnificationError substitution =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.IncorrectUnificationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "substitution"),
          Core.fieldTerm = (Phantoms.unTTerm substitution)}]}))

incorrectUnificationErrorSubstitution :: Phantoms.TTerm Checking.IncorrectUnificationError -> Phantoms.TTerm Typing.TypeSubst
incorrectUnificationErrorSubstitution x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.IncorrectUnificationError"),
        Core.projectionField = (Core.Name "substitution")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

incorrectUnificationErrorWithSubstitution :: Phantoms.TTerm Checking.IncorrectUnificationError -> Phantoms.TTerm Typing.TypeSubst -> Phantoms.TTerm Checking.IncorrectUnificationError
incorrectUnificationErrorWithSubstitution original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.IncorrectUnificationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "substitution"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

notAForallTypeError :: Phantoms.TTerm Core.Type -> Phantoms.TTerm [Core.Type] -> Phantoms.TTerm Checking.NotAForallTypeError
notAForallTypeError type_ typeArguments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.NotAForallTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Phantoms.unTTerm typeArguments)}]}))

notAForallTypeErrorType :: Phantoms.TTerm Checking.NotAForallTypeError -> Phantoms.TTerm Core.Type
notAForallTypeErrorType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.NotAForallTypeError"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

notAForallTypeErrorTypeArguments :: Phantoms.TTerm Checking.NotAForallTypeError -> Phantoms.TTerm [Core.Type]
notAForallTypeErrorTypeArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.NotAForallTypeError"),
        Core.projectionField = (Core.Name "typeArguments")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

notAForallTypeErrorWithType :: Phantoms.TTerm Checking.NotAForallTypeError -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Checking.NotAForallTypeError
notAForallTypeErrorWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.NotAForallTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.NotAForallTypeError"),
              Core.projectionField = (Core.Name "typeArguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

notAForallTypeErrorWithTypeArguments :: Phantoms.TTerm Checking.NotAForallTypeError -> Phantoms.TTerm [Core.Type] -> Phantoms.TTerm Checking.NotAForallTypeError
notAForallTypeErrorWithTypeArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.NotAForallTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.NotAForallTypeError"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

notAFunctionTypeError :: Phantoms.TTerm Core.Type -> Phantoms.TTerm Checking.NotAFunctionTypeError
notAFunctionTypeError type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.NotAFunctionTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))

notAFunctionTypeErrorType :: Phantoms.TTerm Checking.NotAFunctionTypeError -> Phantoms.TTerm Core.Type
notAFunctionTypeErrorType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.NotAFunctionTypeError"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

notAFunctionTypeErrorWithType :: Phantoms.TTerm Checking.NotAFunctionTypeError -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Checking.NotAFunctionTypeError
notAFunctionTypeErrorWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.NotAFunctionTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typeArityMismatchError :: Phantoms.TTerm Core.Type -> Phantoms.TTerm Int -> Phantoms.TTerm Int -> Phantoms.TTerm [Core.Type] -> Phantoms.TTerm Checking.TypeArityMismatchError
typeArityMismatchError type_ expectedArity actualArity typeArguments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "expectedArity"),
          Core.fieldTerm = (Phantoms.unTTerm expectedArity)},
        Core.Field {
          Core.fieldName = (Core.Name "actualArity"),
          Core.fieldTerm = (Phantoms.unTTerm actualArity)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Phantoms.unTTerm typeArguments)}]}))

typeArityMismatchErrorActualArity :: Phantoms.TTerm Checking.TypeArityMismatchError -> Phantoms.TTerm Int
typeArityMismatchErrorActualArity x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
        Core.projectionField = (Core.Name "actualArity")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeArityMismatchErrorExpectedArity :: Phantoms.TTerm Checking.TypeArityMismatchError -> Phantoms.TTerm Int
typeArityMismatchErrorExpectedArity x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
        Core.projectionField = (Core.Name "expectedArity")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeArityMismatchErrorType :: Phantoms.TTerm Checking.TypeArityMismatchError -> Phantoms.TTerm Core.Type
typeArityMismatchErrorType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeArityMismatchErrorTypeArguments :: Phantoms.TTerm Checking.TypeArityMismatchError -> Phantoms.TTerm [Core.Type]
typeArityMismatchErrorTypeArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
        Core.projectionField = (Core.Name "typeArguments")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeArityMismatchErrorWithActualArity :: Phantoms.TTerm Checking.TypeArityMismatchError -> Phantoms.TTerm Int -> Phantoms.TTerm Checking.TypeArityMismatchError
typeArityMismatchErrorWithActualArity original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expectedArity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
              Core.projectionField = (Core.Name "expectedArity")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "actualArity"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
              Core.projectionField = (Core.Name "typeArguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeArityMismatchErrorWithExpectedArity :: Phantoms.TTerm Checking.TypeArityMismatchError -> Phantoms.TTerm Int -> Phantoms.TTerm Checking.TypeArityMismatchError
typeArityMismatchErrorWithExpectedArity original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expectedArity"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "actualArity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
              Core.projectionField = (Core.Name "actualArity")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
              Core.projectionField = (Core.Name "typeArguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeArityMismatchErrorWithType :: Phantoms.TTerm Checking.TypeArityMismatchError -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Checking.TypeArityMismatchError
typeArityMismatchErrorWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expectedArity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
              Core.projectionField = (Core.Name "expectedArity")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "actualArity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
              Core.projectionField = (Core.Name "actualArity")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
              Core.projectionField = (Core.Name "typeArguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeArityMismatchErrorWithTypeArguments :: Phantoms.TTerm Checking.TypeArityMismatchError -> Phantoms.TTerm [Core.Type] -> Phantoms.TTerm Checking.TypeArityMismatchError
typeArityMismatchErrorWithTypeArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expectedArity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
              Core.projectionField = (Core.Name "expectedArity")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "actualArity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
              Core.projectionField = (Core.Name "actualArity")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typeMismatchError :: Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Checking.TypeMismatchError
typeMismatchError expectedType actualType =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.TypeMismatchError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expectedType"),
          Core.fieldTerm = (Phantoms.unTTerm expectedType)},
        Core.Field {
          Core.fieldName = (Core.Name "actualType"),
          Core.fieldTerm = (Phantoms.unTTerm actualType)}]}))

typeMismatchErrorActualType :: Phantoms.TTerm Checking.TypeMismatchError -> Phantoms.TTerm Core.Type
typeMismatchErrorActualType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeMismatchError"),
        Core.projectionField = (Core.Name "actualType")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeMismatchErrorExpectedType :: Phantoms.TTerm Checking.TypeMismatchError -> Phantoms.TTerm Core.Type
typeMismatchErrorExpectedType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeMismatchError"),
        Core.projectionField = (Core.Name "expectedType")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeMismatchErrorWithActualType :: Phantoms.TTerm Checking.TypeMismatchError -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Checking.TypeMismatchError
typeMismatchErrorWithActualType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.TypeMismatchError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expectedType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeMismatchError"),
              Core.projectionField = (Core.Name "expectedType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "actualType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typeMismatchErrorWithExpectedType :: Phantoms.TTerm Checking.TypeMismatchError -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Checking.TypeMismatchError
typeMismatchErrorWithExpectedType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.TypeMismatchError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expectedType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "actualType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeMismatchError"),
              Core.projectionField = (Core.Name "actualType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unboundTypeVariablesError :: Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Checking.UnboundTypeVariablesError
unboundTypeVariablesError variables type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UnboundTypeVariablesError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Phantoms.unTTerm variables)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))

unboundTypeVariablesErrorType :: Phantoms.TTerm Checking.UnboundTypeVariablesError -> Phantoms.TTerm Core.Type
unboundTypeVariablesErrorType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.UnboundTypeVariablesError"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unboundTypeVariablesErrorVariables :: Phantoms.TTerm Checking.UnboundTypeVariablesError -> Phantoms.TTerm (S.Set Core.Name)
unboundTypeVariablesErrorVariables x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.UnboundTypeVariablesError"),
        Core.projectionField = (Core.Name "variables")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unboundTypeVariablesErrorWithType :: Phantoms.TTerm Checking.UnboundTypeVariablesError -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Checking.UnboundTypeVariablesError
unboundTypeVariablesErrorWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UnboundTypeVariablesError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.UnboundTypeVariablesError"),
              Core.projectionField = (Core.Name "variables")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

unboundTypeVariablesErrorWithVariables :: Phantoms.TTerm Checking.UnboundTypeVariablesError -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm Checking.UnboundTypeVariablesError
unboundTypeVariablesErrorWithVariables original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UnboundTypeVariablesError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.UnboundTypeVariablesError"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unequalTypesError :: Phantoms.TTerm [Core.Type] -> Phantoms.TTerm String -> Phantoms.TTerm Checking.UnequalTypesError
unequalTypesError types description =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UnequalTypesError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Phantoms.unTTerm types)},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm description)}]}))

unequalTypesErrorDescription :: Phantoms.TTerm Checking.UnequalTypesError -> Phantoms.TTerm String
unequalTypesErrorDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.UnequalTypesError"),
        Core.projectionField = (Core.Name "description")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unequalTypesErrorTypes :: Phantoms.TTerm Checking.UnequalTypesError -> Phantoms.TTerm [Core.Type]
unequalTypesErrorTypes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.UnequalTypesError"),
        Core.projectionField = (Core.Name "types")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unequalTypesErrorWithDescription :: Phantoms.TTerm Checking.UnequalTypesError -> Phantoms.TTerm String -> Phantoms.TTerm Checking.UnequalTypesError
unequalTypesErrorWithDescription original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UnequalTypesError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.UnequalTypesError"),
              Core.projectionField = (Core.Name "types")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

unequalTypesErrorWithTypes :: Phantoms.TTerm Checking.UnequalTypesError -> Phantoms.TTerm [Core.Type] -> Phantoms.TTerm Checking.UnequalTypesError
unequalTypesErrorWithTypes original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UnequalTypesError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.UnequalTypesError"),
              Core.projectionField = (Core.Name "description")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unsupportedTermVariantError :: Phantoms.TTerm Variants.TermVariant -> Phantoms.TTerm Checking.UnsupportedTermVariantError
unsupportedTermVariantError termVariant =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UnsupportedTermVariantError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "termVariant"),
          Core.fieldTerm = (Phantoms.unTTerm termVariant)}]}))

unsupportedTermVariantErrorTermVariant :: Phantoms.TTerm Checking.UnsupportedTermVariantError -> Phantoms.TTerm Variants.TermVariant
unsupportedTermVariantErrorTermVariant x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.UnsupportedTermVariantError"),
        Core.projectionField = (Core.Name "termVariant")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unsupportedTermVariantErrorWithTermVariant :: Phantoms.TTerm Checking.UnsupportedTermVariantError -> Phantoms.TTerm Variants.TermVariant -> Phantoms.TTerm Checking.UnsupportedTermVariantError
unsupportedTermVariantErrorWithTermVariant original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UnsupportedTermVariantError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "termVariant"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

untypedLambdaError :: Phantoms.TTerm Checking.UntypedLambdaError
untypedLambdaError =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UntypedLambdaError"),
      Core.recordFields = []}))

untypedLetBindingError :: Phantoms.TTerm Core.Binding -> Phantoms.TTerm Checking.UntypedLetBindingError
untypedLetBindingError binding =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UntypedLetBindingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "binding"),
          Core.fieldTerm = (Phantoms.unTTerm binding)}]}))

untypedLetBindingErrorBinding :: Phantoms.TTerm Checking.UntypedLetBindingError -> Phantoms.TTerm Core.Binding
untypedLetBindingErrorBinding x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.UntypedLetBindingError"),
        Core.projectionField = (Core.Name "binding")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

untypedLetBindingErrorWithBinding :: Phantoms.TTerm Checking.UntypedLetBindingError -> Phantoms.TTerm Core.Binding -> Phantoms.TTerm Checking.UntypedLetBindingError
untypedLetBindingErrorWithBinding original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UntypedLetBindingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "binding"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
