-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.errors

module Hydra.Dsl.Errors where

import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as Core_
import qualified Hydra.Errors as Errors
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

decodingError :: Phantoms.TTerm String -> Phantoms.TTerm Errors.DecodingError
decodingError x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unDecodingError :: Phantoms.TTerm Errors.DecodingError -> Phantoms.TTerm String
unDecodingError x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.errors.DecodingError")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

errorChecking :: Phantoms.TTerm Checking.CheckingError -> Phantoms.TTerm Errors.Error
errorChecking x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "checking"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

errorDecoding :: Phantoms.TTerm Errors.DecodingError -> Phantoms.TTerm Errors.Error
errorDecoding x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "decoding"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

errorDuplicateBinding :: Phantoms.TTerm Core_.DuplicateBindingError -> Phantoms.TTerm Errors.Error
errorDuplicateBinding x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "duplicateBinding"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

errorDuplicateField :: Phantoms.TTerm Core_.DuplicateFieldError -> Phantoms.TTerm Errors.Error
errorDuplicateField x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "duplicateField"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

errorOther :: Phantoms.TTerm Errors.OtherError -> Phantoms.TTerm Errors.Error
errorOther x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "other"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

errorUndefinedField :: Phantoms.TTerm Core_.UndefinedFieldError -> Phantoms.TTerm Errors.Error
errorUndefinedField x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "undefinedField"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

errorUndefinedTermVariable :: Phantoms.TTerm Core_.UndefinedTermVariableError -> Phantoms.TTerm Errors.Error
errorUndefinedTermVariable x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "undefinedTermVariable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

errorUntypedTermVariable :: Phantoms.TTerm Core_.UntypedTermVariableError -> Phantoms.TTerm Errors.Error
errorUntypedTermVariable x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "untypedTermVariable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

errorUnexpectedTermVariant :: Phantoms.TTerm Core_.UnexpectedTermVariantError -> Phantoms.TTerm Errors.Error
errorUnexpectedTermVariant x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unexpectedTermVariant"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

errorUnexpectedTypeVariant :: Phantoms.TTerm Core_.UnexpectedTypeVariantError -> Phantoms.TTerm Errors.Error
errorUnexpectedTypeVariant x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unexpectedTypeVariant"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

errorUnification :: Phantoms.TTerm Errors.UnificationError -> Phantoms.TTerm Errors.Error
errorUnification x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unification"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

otherError :: Phantoms.TTerm String -> Phantoms.TTerm Errors.OtherError
otherError x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.errors.OtherError"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unOtherError :: Phantoms.TTerm Errors.OtherError -> Phantoms.TTerm String
unOtherError x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.errors.OtherError")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unificationError :: Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.Type -> Phantoms.TTerm String -> Phantoms.TTerm Errors.UnificationError
unificationError leftType rightType message =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.UnificationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "leftType"),
          Core.fieldTerm = (Phantoms.unTTerm leftType)},
        Core.Field {
          Core.fieldName = (Core.Name "rightType"),
          Core.fieldTerm = (Phantoms.unTTerm rightType)},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Phantoms.unTTerm message)}]}))

unificationErrorLeftType :: Phantoms.TTerm Errors.UnificationError -> Phantoms.TTerm Core.Type
unificationErrorLeftType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.errors.UnificationError"),
        Core.projectionField = (Core.Name "leftType")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unificationErrorRightType :: Phantoms.TTerm Errors.UnificationError -> Phantoms.TTerm Core.Type
unificationErrorRightType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.errors.UnificationError"),
        Core.projectionField = (Core.Name "rightType")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unificationErrorMessage :: Phantoms.TTerm Errors.UnificationError -> Phantoms.TTerm String
unificationErrorMessage x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.errors.UnificationError"),
        Core.projectionField = (Core.Name "message")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unificationErrorWithLeftType :: Phantoms.TTerm Errors.UnificationError -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Errors.UnificationError
unificationErrorWithLeftType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.UnificationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "leftType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rightType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.errors.UnificationError"),
              Core.projectionField = (Core.Name "rightType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.errors.UnificationError"),
              Core.projectionField = (Core.Name "message")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unificationErrorWithRightType :: Phantoms.TTerm Errors.UnificationError -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Errors.UnificationError
unificationErrorWithRightType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.UnificationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "leftType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.errors.UnificationError"),
              Core.projectionField = (Core.Name "leftType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rightType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.errors.UnificationError"),
              Core.projectionField = (Core.Name "message")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unificationErrorWithMessage :: Phantoms.TTerm Errors.UnificationError -> Phantoms.TTerm String -> Phantoms.TTerm Errors.UnificationError
unificationErrorWithMessage original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.UnificationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "leftType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.errors.UnificationError"),
              Core.projectionField = (Core.Name "leftType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rightType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.errors.UnificationError"),
              Core.projectionField = (Core.Name "rightType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
