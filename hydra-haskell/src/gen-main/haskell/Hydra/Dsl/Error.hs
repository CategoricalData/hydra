-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.error

module Hydra.Dsl.Error where

import qualified Hydra.Core as Core
import qualified Hydra.Error as Error
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Typing as Typing
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

checkingErrorIncorrectUnification :: (Phantoms.TTerm Error.IncorrectUnificationError -> Phantoms.TTerm Error.CheckingError)
checkingErrorIncorrectUnification x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.error.CheckingError"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "incorrectUnification"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

checkingErrorNotAForallType :: (Phantoms.TTerm Error.NotAForallTypeError -> Phantoms.TTerm Error.CheckingError)
checkingErrorNotAForallType x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.error.CheckingError"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "notAForallType"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

checkingErrorNotAFunctionType :: (Phantoms.TTerm Error.NotAFunctionTypeError -> Phantoms.TTerm Error.CheckingError)
checkingErrorNotAFunctionType x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.error.CheckingError"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "notAFunctionType"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

checkingErrorTypeArityMismatch :: (Phantoms.TTerm Error.TypeArityMismatchError -> Phantoms.TTerm Error.CheckingError)
checkingErrorTypeArityMismatch x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.error.CheckingError"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "typeArityMismatch"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

checkingErrorTypeMismatch :: (Phantoms.TTerm Error.TypeMismatchError -> Phantoms.TTerm Error.CheckingError)
checkingErrorTypeMismatch x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.error.CheckingError"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "typeMismatch"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

checkingErrorUnboundTypeVariables :: (Phantoms.TTerm Error.UnboundTypeVariablesError -> Phantoms.TTerm Error.CheckingError)
checkingErrorUnboundTypeVariables x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.error.CheckingError"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "unboundTypeVariables"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

checkingErrorUnequalTypes :: (Phantoms.TTerm Error.UnequalTypesError -> Phantoms.TTerm Error.CheckingError)
checkingErrorUnequalTypes x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.error.CheckingError"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "unequalTypes"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

checkingErrorUnsupportedTermVariant :: (Phantoms.TTerm Error.UnsupportedTermVariantError -> Phantoms.TTerm Error.CheckingError)
checkingErrorUnsupportedTermVariant x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.error.CheckingError"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "unsupportedTermVariant"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

checkingErrorUntypedLambda :: (Phantoms.TTerm Error.UntypedLambdaError -> Phantoms.TTerm Error.CheckingError)
checkingErrorUntypedLambda x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.error.CheckingError"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "untypedLambda"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

checkingErrorUntypedLetBinding :: (Phantoms.TTerm Error.UntypedLetBindingError -> Phantoms.TTerm Error.CheckingError)
checkingErrorUntypedLetBinding x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.error.CheckingError"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "untypedLetBinding"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

decodingError :: (Phantoms.TTerm String -> Phantoms.TTerm Error.DecodingError)
decodingError x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.error.DecodingError"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unDecodingError :: (Phantoms.TTerm Error.DecodingError -> Phantoms.TTerm String)
unDecodingError x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.error.DecodingError")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

duplicateBindingError :: (Phantoms.TTerm Core.Name -> Phantoms.TTerm Error.DuplicateBindingError)
duplicateBindingError name = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.DuplicateBindingError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)}]})))

duplicateBindingErrorName :: (Phantoms.TTerm Error.DuplicateBindingError -> Phantoms.TTerm Core.Name)
duplicateBindingErrorName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.error.DuplicateBindingError"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

duplicateBindingErrorWithName :: (Phantoms.TTerm Error.DuplicateBindingError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Error.DuplicateBindingError)
duplicateBindingErrorWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.DuplicateBindingError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

duplicateFieldError :: (Phantoms.TTerm Core.Name -> Phantoms.TTerm Error.DuplicateFieldError)
duplicateFieldError name = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.DuplicateFieldError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)}]})))

duplicateFieldErrorName :: (Phantoms.TTerm Error.DuplicateFieldError -> Phantoms.TTerm Core.Name)
duplicateFieldErrorName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.error.DuplicateFieldError"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

duplicateFieldErrorWithName :: (Phantoms.TTerm Error.DuplicateFieldError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Error.DuplicateFieldError)
duplicateFieldErrorWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.DuplicateFieldError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

errorChecking :: (Phantoms.TTerm Error.CheckingError -> Phantoms.TTerm Error.Error)
errorChecking x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.error.Error"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "checking"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

errorDecoding :: (Phantoms.TTerm Error.DecodingError -> Phantoms.TTerm Error.Error)
errorDecoding x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.error.Error"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "decoding"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

errorDuplicateBinding :: (Phantoms.TTerm Error.DuplicateBindingError -> Phantoms.TTerm Error.Error)
errorDuplicateBinding x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.error.Error"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "duplicateBinding"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

errorDuplicateField :: (Phantoms.TTerm Error.DuplicateFieldError -> Phantoms.TTerm Error.Error)
errorDuplicateField x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.error.Error"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "duplicateField"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

errorOther :: (Phantoms.TTerm Error.OtherError -> Phantoms.TTerm Error.Error)
errorOther x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.error.Error"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "other"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

errorUndefinedField :: (Phantoms.TTerm Error.UndefinedFieldError -> Phantoms.TTerm Error.Error)
errorUndefinedField x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.error.Error"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "undefinedField"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

errorUndefinedTerm :: (Phantoms.TTerm Error.UndefinedTermError -> Phantoms.TTerm Error.Error)
errorUndefinedTerm x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.error.Error"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "undefinedTerm"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

errorUndefinedType :: (Phantoms.TTerm Error.UndefinedTypeError -> Phantoms.TTerm Error.Error)
errorUndefinedType x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.error.Error"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "undefinedType"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

errorUnexpectedTermVariant :: (Phantoms.TTerm Error.UnexpectedTermVariantError -> Phantoms.TTerm Error.Error)
errorUnexpectedTermVariant x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.error.Error"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "unexpectedTermVariant"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

errorUnexpectedTypeVariant :: (Phantoms.TTerm Error.UnexpectedTypeVariantError -> Phantoms.TTerm Error.Error)
errorUnexpectedTypeVariant x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.error.Error"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "unexpectedTypeVariant"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

errorUnification :: (Phantoms.TTerm Error.UnificationError -> Phantoms.TTerm Error.Error)
errorUnification x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.error.Error"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "unification"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

incorrectUnificationError :: (Phantoms.TTerm Typing.TypeSubst -> Phantoms.TTerm Error.IncorrectUnificationError)
incorrectUnificationError substitution = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.IncorrectUnificationError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "substitution"),
      Core.fieldTerm = (Phantoms.unTTerm substitution)}]})))

incorrectUnificationErrorSubstitution :: (Phantoms.TTerm Error.IncorrectUnificationError -> Phantoms.TTerm Typing.TypeSubst)
incorrectUnificationErrorSubstitution x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.error.IncorrectUnificationError"),
    Core.projectionField = (Core.Name "substitution")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

incorrectUnificationErrorWithSubstitution :: (Phantoms.TTerm Error.IncorrectUnificationError -> Phantoms.TTerm Typing.TypeSubst -> Phantoms.TTerm Error.IncorrectUnificationError)
incorrectUnificationErrorWithSubstitution original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.IncorrectUnificationError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "substitution"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

notAForallTypeError :: (Phantoms.TTerm Core.Type -> Phantoms.TTerm [Core.Type] -> Phantoms.TTerm Error.NotAForallTypeError)
notAForallTypeError type_ typeArguments = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.NotAForallTypeError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm type_)},
    Core.Field {
      Core.fieldName = (Core.Name "typeArguments"),
      Core.fieldTerm = (Phantoms.unTTerm typeArguments)}]})))

notAForallTypeErrorType :: (Phantoms.TTerm Error.NotAForallTypeError -> Phantoms.TTerm Core.Type)
notAForallTypeErrorType x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.error.NotAForallTypeError"),
    Core.projectionField = (Core.Name "type")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

notAForallTypeErrorTypeArguments :: (Phantoms.TTerm Error.NotAForallTypeError -> Phantoms.TTerm [Core.Type])
notAForallTypeErrorTypeArguments x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.error.NotAForallTypeError"),
    Core.projectionField = (Core.Name "typeArguments")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

notAForallTypeErrorWithType :: (Phantoms.TTerm Error.NotAForallTypeError -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Error.NotAForallTypeError)
notAForallTypeErrorWithType original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.NotAForallTypeError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "typeArguments"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.error.NotAForallTypeError"),
          Core.projectionField = (Core.Name "typeArguments")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

notAForallTypeErrorWithTypeArguments :: (Phantoms.TTerm Error.NotAForallTypeError -> Phantoms.TTerm [Core.Type] -> Phantoms.TTerm Error.NotAForallTypeError)
notAForallTypeErrorWithTypeArguments original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.NotAForallTypeError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.error.NotAForallTypeError"),
          Core.projectionField = (Core.Name "type")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeArguments"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

notAFunctionTypeError :: (Phantoms.TTerm Core.Type -> Phantoms.TTerm Error.NotAFunctionTypeError)
notAFunctionTypeError type_ = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.NotAFunctionTypeError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm type_)}]})))

notAFunctionTypeErrorType :: (Phantoms.TTerm Error.NotAFunctionTypeError -> Phantoms.TTerm Core.Type)
notAFunctionTypeErrorType x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.error.NotAFunctionTypeError"),
    Core.projectionField = (Core.Name "type")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

notAFunctionTypeErrorWithType :: (Phantoms.TTerm Error.NotAFunctionTypeError -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Error.NotAFunctionTypeError)
notAFunctionTypeErrorWithType original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.NotAFunctionTypeError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

otherError :: (Phantoms.TTerm String -> Phantoms.TTerm Error.OtherError)
otherError x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.error.OtherError"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unOtherError :: (Phantoms.TTerm Error.OtherError -> Phantoms.TTerm String)
unOtherError x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.error.OtherError")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

typeArityMismatchError :: (Phantoms.TTerm Core.Type -> Phantoms.TTerm Int -> Phantoms.TTerm Int -> Phantoms.TTerm [Core.Type] -> Phantoms.TTerm Error.TypeArityMismatchError)
typeArityMismatchError type_ expectedArity actualArity typeArguments = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.TypeArityMismatchError"),
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
      Core.fieldTerm = (Phantoms.unTTerm typeArguments)}]})))

typeArityMismatchErrorType :: (Phantoms.TTerm Error.TypeArityMismatchError -> Phantoms.TTerm Core.Type)
typeArityMismatchErrorType x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.error.TypeArityMismatchError"),
    Core.projectionField = (Core.Name "type")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

typeArityMismatchErrorExpectedArity :: (Phantoms.TTerm Error.TypeArityMismatchError -> Phantoms.TTerm Int)
typeArityMismatchErrorExpectedArity x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.error.TypeArityMismatchError"),
    Core.projectionField = (Core.Name "expectedArity")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

typeArityMismatchErrorActualArity :: (Phantoms.TTerm Error.TypeArityMismatchError -> Phantoms.TTerm Int)
typeArityMismatchErrorActualArity x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.error.TypeArityMismatchError"),
    Core.projectionField = (Core.Name "actualArity")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

typeArityMismatchErrorTypeArguments :: (Phantoms.TTerm Error.TypeArityMismatchError -> Phantoms.TTerm [Core.Type])
typeArityMismatchErrorTypeArguments x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.error.TypeArityMismatchError"),
    Core.projectionField = (Core.Name "typeArguments")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

typeArityMismatchErrorWithType :: (Phantoms.TTerm Error.TypeArityMismatchError -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Error.TypeArityMismatchError)
typeArityMismatchErrorWithType original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.TypeArityMismatchError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "expectedArity"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.error.TypeArityMismatchError"),
          Core.projectionField = (Core.Name "expectedArity")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "actualArity"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.error.TypeArityMismatchError"),
          Core.projectionField = (Core.Name "actualArity")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeArguments"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.error.TypeArityMismatchError"),
          Core.projectionField = (Core.Name "typeArguments")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

typeArityMismatchErrorWithExpectedArity :: (Phantoms.TTerm Error.TypeArityMismatchError -> Phantoms.TTerm Int -> Phantoms.TTerm Error.TypeArityMismatchError)
typeArityMismatchErrorWithExpectedArity original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.TypeArityMismatchError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.error.TypeArityMismatchError"),
          Core.projectionField = (Core.Name "type")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "expectedArity"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "actualArity"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.error.TypeArityMismatchError"),
          Core.projectionField = (Core.Name "actualArity")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeArguments"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.error.TypeArityMismatchError"),
          Core.projectionField = (Core.Name "typeArguments")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

typeArityMismatchErrorWithActualArity :: (Phantoms.TTerm Error.TypeArityMismatchError -> Phantoms.TTerm Int -> Phantoms.TTerm Error.TypeArityMismatchError)
typeArityMismatchErrorWithActualArity original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.TypeArityMismatchError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.error.TypeArityMismatchError"),
          Core.projectionField = (Core.Name "type")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "expectedArity"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.error.TypeArityMismatchError"),
          Core.projectionField = (Core.Name "expectedArity")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "actualArity"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "typeArguments"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.error.TypeArityMismatchError"),
          Core.projectionField = (Core.Name "typeArguments")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

typeArityMismatchErrorWithTypeArguments :: (Phantoms.TTerm Error.TypeArityMismatchError -> Phantoms.TTerm [Core.Type] -> Phantoms.TTerm Error.TypeArityMismatchError)
typeArityMismatchErrorWithTypeArguments original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.TypeArityMismatchError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.error.TypeArityMismatchError"),
          Core.projectionField = (Core.Name "type")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "expectedArity"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.error.TypeArityMismatchError"),
          Core.projectionField = (Core.Name "expectedArity")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "actualArity"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.error.TypeArityMismatchError"),
          Core.projectionField = (Core.Name "actualArity")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeArguments"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

typeMismatchError :: (Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Error.TypeMismatchError)
typeMismatchError expectedType actualType = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.TypeMismatchError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expectedType"),
      Core.fieldTerm = (Phantoms.unTTerm expectedType)},
    Core.Field {
      Core.fieldName = (Core.Name "actualType"),
      Core.fieldTerm = (Phantoms.unTTerm actualType)}]})))

typeMismatchErrorExpectedType :: (Phantoms.TTerm Error.TypeMismatchError -> Phantoms.TTerm Core.Type)
typeMismatchErrorExpectedType x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.error.TypeMismatchError"),
    Core.projectionField = (Core.Name "expectedType")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

typeMismatchErrorActualType :: (Phantoms.TTerm Error.TypeMismatchError -> Phantoms.TTerm Core.Type)
typeMismatchErrorActualType x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.error.TypeMismatchError"),
    Core.projectionField = (Core.Name "actualType")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

typeMismatchErrorWithExpectedType :: (Phantoms.TTerm Error.TypeMismatchError -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Error.TypeMismatchError)
typeMismatchErrorWithExpectedType original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.TypeMismatchError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expectedType"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "actualType"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.error.TypeMismatchError"),
          Core.projectionField = (Core.Name "actualType")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

typeMismatchErrorWithActualType :: (Phantoms.TTerm Error.TypeMismatchError -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Error.TypeMismatchError)
typeMismatchErrorWithActualType original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.TypeMismatchError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expectedType"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.error.TypeMismatchError"),
          Core.projectionField = (Core.Name "expectedType")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "actualType"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

unboundTypeVariablesError :: (Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Error.UnboundTypeVariablesError)
unboundTypeVariablesError variables type_ = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.UnboundTypeVariablesError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "variables"),
      Core.fieldTerm = (Phantoms.unTTerm variables)},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm type_)}]})))

unboundTypeVariablesErrorVariables :: (Phantoms.TTerm Error.UnboundTypeVariablesError -> Phantoms.TTerm (S.Set Core.Name))
unboundTypeVariablesErrorVariables x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.error.UnboundTypeVariablesError"),
    Core.projectionField = (Core.Name "variables")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

unboundTypeVariablesErrorType :: (Phantoms.TTerm Error.UnboundTypeVariablesError -> Phantoms.TTerm Core.Type)
unboundTypeVariablesErrorType x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.error.UnboundTypeVariablesError"),
    Core.projectionField = (Core.Name "type")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

unboundTypeVariablesErrorWithVariables :: (Phantoms.TTerm Error.UnboundTypeVariablesError -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm Error.UnboundTypeVariablesError)
unboundTypeVariablesErrorWithVariables original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.UnboundTypeVariablesError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "variables"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.error.UnboundTypeVariablesError"),
          Core.projectionField = (Core.Name "type")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

unboundTypeVariablesErrorWithType :: (Phantoms.TTerm Error.UnboundTypeVariablesError -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Error.UnboundTypeVariablesError)
unboundTypeVariablesErrorWithType original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.UnboundTypeVariablesError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "variables"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.error.UnboundTypeVariablesError"),
          Core.projectionField = (Core.Name "variables")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

undefinedFieldError :: (Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Error.UndefinedFieldError)
undefinedFieldError fieldName typeName = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.UndefinedFieldError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "fieldName"),
      Core.fieldTerm = (Phantoms.unTTerm fieldName)},
    Core.Field {
      Core.fieldName = (Core.Name "typeName"),
      Core.fieldTerm = (Phantoms.unTTerm typeName)}]})))

undefinedFieldErrorFieldName :: (Phantoms.TTerm Error.UndefinedFieldError -> Phantoms.TTerm Core.Name)
undefinedFieldErrorFieldName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.error.UndefinedFieldError"),
    Core.projectionField = (Core.Name "fieldName")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

undefinedFieldErrorTypeName :: (Phantoms.TTerm Error.UndefinedFieldError -> Phantoms.TTerm Core.Name)
undefinedFieldErrorTypeName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.error.UndefinedFieldError"),
    Core.projectionField = (Core.Name "typeName")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

undefinedFieldErrorWithFieldName :: (Phantoms.TTerm Error.UndefinedFieldError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Error.UndefinedFieldError)
undefinedFieldErrorWithFieldName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.UndefinedFieldError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "fieldName"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "typeName"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.error.UndefinedFieldError"),
          Core.projectionField = (Core.Name "typeName")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

undefinedFieldErrorWithTypeName :: (Phantoms.TTerm Error.UndefinedFieldError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Error.UndefinedFieldError)
undefinedFieldErrorWithTypeName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.UndefinedFieldError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "fieldName"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.error.UndefinedFieldError"),
          Core.projectionField = (Core.Name "fieldName")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeName"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

undefinedTermError :: (Phantoms.TTerm Core.Name -> Phantoms.TTerm Error.UndefinedTermError)
undefinedTermError name = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.UndefinedTermError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)}]})))

undefinedTermErrorName :: (Phantoms.TTerm Error.UndefinedTermError -> Phantoms.TTerm Core.Name)
undefinedTermErrorName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.error.UndefinedTermError"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

undefinedTermErrorWithName :: (Phantoms.TTerm Error.UndefinedTermError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Error.UndefinedTermError)
undefinedTermErrorWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.UndefinedTermError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

undefinedTypeError :: (Phantoms.TTerm Core.Name -> Phantoms.TTerm Error.UndefinedTypeError)
undefinedTypeError name = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.UndefinedTypeError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)}]})))

undefinedTypeErrorName :: (Phantoms.TTerm Error.UndefinedTypeError -> Phantoms.TTerm Core.Name)
undefinedTypeErrorName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.error.UndefinedTypeError"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

undefinedTypeErrorWithName :: (Phantoms.TTerm Error.UndefinedTypeError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Error.UndefinedTypeError)
undefinedTypeErrorWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.UndefinedTypeError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

unequalTypesError :: (Phantoms.TTerm [Core.Type] -> Phantoms.TTerm String -> Phantoms.TTerm Error.UnequalTypesError)
unequalTypesError types description = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.UnequalTypesError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "types"),
      Core.fieldTerm = (Phantoms.unTTerm types)},
    Core.Field {
      Core.fieldName = (Core.Name "description"),
      Core.fieldTerm = (Phantoms.unTTerm description)}]})))

unequalTypesErrorTypes :: (Phantoms.TTerm Error.UnequalTypesError -> Phantoms.TTerm [Core.Type])
unequalTypesErrorTypes x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.error.UnequalTypesError"),
    Core.projectionField = (Core.Name "types")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

unequalTypesErrorDescription :: (Phantoms.TTerm Error.UnequalTypesError -> Phantoms.TTerm String)
unequalTypesErrorDescription x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.error.UnequalTypesError"),
    Core.projectionField = (Core.Name "description")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

unequalTypesErrorWithTypes :: (Phantoms.TTerm Error.UnequalTypesError -> Phantoms.TTerm [Core.Type] -> Phantoms.TTerm Error.UnequalTypesError)
unequalTypesErrorWithTypes original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.UnequalTypesError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "types"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "description"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.error.UnequalTypesError"),
          Core.projectionField = (Core.Name "description")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

unequalTypesErrorWithDescription :: (Phantoms.TTerm Error.UnequalTypesError -> Phantoms.TTerm String -> Phantoms.TTerm Error.UnequalTypesError)
unequalTypesErrorWithDescription original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.UnequalTypesError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "types"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.error.UnequalTypesError"),
          Core.projectionField = (Core.Name "types")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "description"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

unexpectedTermVariantError :: (Phantoms.TTerm Variants.TermVariant -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Error.UnexpectedTermVariantError)
unexpectedTermVariantError expectedVariant actualTerm = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.UnexpectedTermVariantError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expectedVariant"),
      Core.fieldTerm = (Phantoms.unTTerm expectedVariant)},
    Core.Field {
      Core.fieldName = (Core.Name "actualTerm"),
      Core.fieldTerm = (Phantoms.unTTerm actualTerm)}]})))

unexpectedTermVariantErrorExpectedVariant :: (Phantoms.TTerm Error.UnexpectedTermVariantError -> Phantoms.TTerm Variants.TermVariant)
unexpectedTermVariantErrorExpectedVariant x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.error.UnexpectedTermVariantError"),
    Core.projectionField = (Core.Name "expectedVariant")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

unexpectedTermVariantErrorActualTerm :: (Phantoms.TTerm Error.UnexpectedTermVariantError -> Phantoms.TTerm Core.Term)
unexpectedTermVariantErrorActualTerm x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.error.UnexpectedTermVariantError"),
    Core.projectionField = (Core.Name "actualTerm")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

unexpectedTermVariantErrorWithExpectedVariant :: (Phantoms.TTerm Error.UnexpectedTermVariantError -> Phantoms.TTerm Variants.TermVariant -> Phantoms.TTerm Error.UnexpectedTermVariantError)
unexpectedTermVariantErrorWithExpectedVariant original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.UnexpectedTermVariantError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expectedVariant"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "actualTerm"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.error.UnexpectedTermVariantError"),
          Core.projectionField = (Core.Name "actualTerm")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

unexpectedTermVariantErrorWithActualTerm :: (Phantoms.TTerm Error.UnexpectedTermVariantError -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Error.UnexpectedTermVariantError)
unexpectedTermVariantErrorWithActualTerm original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.UnexpectedTermVariantError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expectedVariant"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.error.UnexpectedTermVariantError"),
          Core.projectionField = (Core.Name "expectedVariant")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "actualTerm"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

unexpectedTypeVariantError :: (Phantoms.TTerm Variants.TypeVariant -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Error.UnexpectedTypeVariantError)
unexpectedTypeVariantError expectedVariant actualType = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.UnexpectedTypeVariantError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expectedVariant"),
      Core.fieldTerm = (Phantoms.unTTerm expectedVariant)},
    Core.Field {
      Core.fieldName = (Core.Name "actualType"),
      Core.fieldTerm = (Phantoms.unTTerm actualType)}]})))

unexpectedTypeVariantErrorExpectedVariant :: (Phantoms.TTerm Error.UnexpectedTypeVariantError -> Phantoms.TTerm Variants.TypeVariant)
unexpectedTypeVariantErrorExpectedVariant x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.error.UnexpectedTypeVariantError"),
    Core.projectionField = (Core.Name "expectedVariant")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

unexpectedTypeVariantErrorActualType :: (Phantoms.TTerm Error.UnexpectedTypeVariantError -> Phantoms.TTerm Core.Type)
unexpectedTypeVariantErrorActualType x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.error.UnexpectedTypeVariantError"),
    Core.projectionField = (Core.Name "actualType")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

unexpectedTypeVariantErrorWithExpectedVariant :: (Phantoms.TTerm Error.UnexpectedTypeVariantError -> Phantoms.TTerm Variants.TypeVariant -> Phantoms.TTerm Error.UnexpectedTypeVariantError)
unexpectedTypeVariantErrorWithExpectedVariant original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.UnexpectedTypeVariantError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expectedVariant"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "actualType"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.error.UnexpectedTypeVariantError"),
          Core.projectionField = (Core.Name "actualType")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

unexpectedTypeVariantErrorWithActualType :: (Phantoms.TTerm Error.UnexpectedTypeVariantError -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Error.UnexpectedTypeVariantError)
unexpectedTypeVariantErrorWithActualType original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.UnexpectedTypeVariantError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expectedVariant"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.error.UnexpectedTypeVariantError"),
          Core.projectionField = (Core.Name "expectedVariant")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "actualType"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

unificationError :: (Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.Type -> Phantoms.TTerm String -> Phantoms.TTerm Error.UnificationError)
unificationError leftType rightType message = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.UnificationError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "leftType"),
      Core.fieldTerm = (Phantoms.unTTerm leftType)},
    Core.Field {
      Core.fieldName = (Core.Name "rightType"),
      Core.fieldTerm = (Phantoms.unTTerm rightType)},
    Core.Field {
      Core.fieldName = (Core.Name "message"),
      Core.fieldTerm = (Phantoms.unTTerm message)}]})))

unificationErrorLeftType :: (Phantoms.TTerm Error.UnificationError -> Phantoms.TTerm Core.Type)
unificationErrorLeftType x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.error.UnificationError"),
    Core.projectionField = (Core.Name "leftType")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

unificationErrorRightType :: (Phantoms.TTerm Error.UnificationError -> Phantoms.TTerm Core.Type)
unificationErrorRightType x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.error.UnificationError"),
    Core.projectionField = (Core.Name "rightType")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

unificationErrorMessage :: (Phantoms.TTerm Error.UnificationError -> Phantoms.TTerm String)
unificationErrorMessage x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.error.UnificationError"),
    Core.projectionField = (Core.Name "message")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

unificationErrorWithLeftType :: (Phantoms.TTerm Error.UnificationError -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Error.UnificationError)
unificationErrorWithLeftType original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.UnificationError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "leftType"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "rightType"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.error.UnificationError"),
          Core.projectionField = (Core.Name "rightType")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "message"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.error.UnificationError"),
          Core.projectionField = (Core.Name "message")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

unificationErrorWithRightType :: (Phantoms.TTerm Error.UnificationError -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Error.UnificationError)
unificationErrorWithRightType original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.UnificationError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "leftType"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.error.UnificationError"),
          Core.projectionField = (Core.Name "leftType")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "rightType"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "message"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.error.UnificationError"),
          Core.projectionField = (Core.Name "message")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

unificationErrorWithMessage :: (Phantoms.TTerm Error.UnificationError -> Phantoms.TTerm String -> Phantoms.TTerm Error.UnificationError)
unificationErrorWithMessage original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.UnificationError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "leftType"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.error.UnificationError"),
          Core.projectionField = (Core.Name "leftType")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "rightType"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.error.UnificationError"),
          Core.projectionField = (Core.Name "rightType")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "message"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

unsupportedTermVariantError :: (Phantoms.TTerm Variants.TermVariant -> Phantoms.TTerm Error.UnsupportedTermVariantError)
unsupportedTermVariantError termVariant = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.UnsupportedTermVariantError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "termVariant"),
      Core.fieldTerm = (Phantoms.unTTerm termVariant)}]})))

unsupportedTermVariantErrorTermVariant :: (Phantoms.TTerm Error.UnsupportedTermVariantError -> Phantoms.TTerm Variants.TermVariant)
unsupportedTermVariantErrorTermVariant x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.error.UnsupportedTermVariantError"),
    Core.projectionField = (Core.Name "termVariant")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

unsupportedTermVariantErrorWithTermVariant :: (Phantoms.TTerm Error.UnsupportedTermVariantError -> Phantoms.TTerm Variants.TermVariant -> Phantoms.TTerm Error.UnsupportedTermVariantError)
unsupportedTermVariantErrorWithTermVariant original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.UnsupportedTermVariantError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "termVariant"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

untypedLambdaError :: (Phantoms.TTerm Error.UntypedLambdaError)
untypedLambdaError = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.UntypedLambdaError"),
  Core.recordFields = []})))

untypedLetBindingError :: (Phantoms.TTerm Core.Binding -> Phantoms.TTerm Error.UntypedLetBindingError)
untypedLetBindingError binding = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.UntypedLetBindingError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "binding"),
      Core.fieldTerm = (Phantoms.unTTerm binding)}]})))

untypedLetBindingErrorBinding :: (Phantoms.TTerm Error.UntypedLetBindingError -> Phantoms.TTerm Core.Binding)
untypedLetBindingErrorBinding x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.error.UntypedLetBindingError"),
    Core.projectionField = (Core.Name "binding")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

untypedLetBindingErrorWithBinding :: (Phantoms.TTerm Error.UntypedLetBindingError -> Phantoms.TTerm Core.Binding -> Phantoms.TTerm Error.UntypedLetBindingError)
untypedLetBindingErrorWithBinding original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.error.UntypedLetBindingError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "binding"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))
