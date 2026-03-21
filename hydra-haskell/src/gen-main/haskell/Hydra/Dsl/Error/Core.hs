-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.error.core

module Hydra.Dsl.Error.Core where

import qualified Hydra.Accessors as Accessors
import qualified Hydra.Core as Core
import qualified Hydra.Error.Core as Core_
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

duplicateBindingError :: Phantoms.TTerm Accessors.AccessorPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.DuplicateBindingError
duplicateBindingError location name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.DuplicateBindingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

duplicateBindingErrorLocation :: Phantoms.TTerm Core_.DuplicateBindingError -> Phantoms.TTerm Accessors.AccessorPath
duplicateBindingErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateBindingError"),
        Core.projectionField = (Core.Name "location")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

duplicateBindingErrorName :: Phantoms.TTerm Core_.DuplicateBindingError -> Phantoms.TTerm Core.Name
duplicateBindingErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateBindingError"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

duplicateBindingErrorWithLocation :: Phantoms.TTerm Core_.DuplicateBindingError -> Phantoms.TTerm Accessors.AccessorPath -> Phantoms.TTerm Core_.DuplicateBindingError
duplicateBindingErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.DuplicateBindingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateBindingError"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

duplicateBindingErrorWithName :: Phantoms.TTerm Core_.DuplicateBindingError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.DuplicateBindingError
duplicateBindingErrorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.DuplicateBindingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateBindingError"),
              Core.projectionField = (Core.Name "location")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

duplicateFieldError :: Phantoms.TTerm Accessors.AccessorPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.DuplicateFieldError
duplicateFieldError location name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.DuplicateFieldError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

duplicateFieldErrorLocation :: Phantoms.TTerm Core_.DuplicateFieldError -> Phantoms.TTerm Accessors.AccessorPath
duplicateFieldErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateFieldError"),
        Core.projectionField = (Core.Name "location")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

duplicateFieldErrorName :: Phantoms.TTerm Core_.DuplicateFieldError -> Phantoms.TTerm Core.Name
duplicateFieldErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateFieldError"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

duplicateFieldErrorWithLocation :: Phantoms.TTerm Core_.DuplicateFieldError -> Phantoms.TTerm Accessors.AccessorPath -> Phantoms.TTerm Core_.DuplicateFieldError
duplicateFieldErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.DuplicateFieldError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateFieldError"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

duplicateFieldErrorWithName :: Phantoms.TTerm Core_.DuplicateFieldError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.DuplicateFieldError
duplicateFieldErrorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.DuplicateFieldError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateFieldError"),
              Core.projectionField = (Core.Name "location")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

invalidTermErrorDuplicateBinding :: Phantoms.TTerm Core_.DuplicateBindingError -> Phantoms.TTerm Core_.InvalidTermError
invalidTermErrorDuplicateBinding x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "duplicateBinding"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidTermErrorDuplicateField :: Phantoms.TTerm Core_.DuplicateFieldError -> Phantoms.TTerm Core_.InvalidTermError
invalidTermErrorDuplicateField x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "duplicateField"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

undefinedFieldError :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.UndefinedFieldError
undefinedFieldError fieldName typeName =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedFieldError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fieldName"),
          Core.fieldTerm = (Phantoms.unTTerm fieldName)},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Phantoms.unTTerm typeName)}]}))

undefinedFieldErrorFieldName :: Phantoms.TTerm Core_.UndefinedFieldError -> Phantoms.TTerm Core.Name
undefinedFieldErrorFieldName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedFieldError"),
        Core.projectionField = (Core.Name "fieldName")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

undefinedFieldErrorTypeName :: Phantoms.TTerm Core_.UndefinedFieldError -> Phantoms.TTerm Core.Name
undefinedFieldErrorTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedFieldError"),
        Core.projectionField = (Core.Name "typeName")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

undefinedFieldErrorWithFieldName :: Phantoms.TTerm Core_.UndefinedFieldError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.UndefinedFieldError
undefinedFieldErrorWithFieldName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedFieldError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fieldName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedFieldError"),
              Core.projectionField = (Core.Name "typeName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

undefinedFieldErrorWithTypeName :: Phantoms.TTerm Core_.UndefinedFieldError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.UndefinedFieldError
undefinedFieldErrorWithTypeName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedFieldError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fieldName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedFieldError"),
              Core.projectionField = (Core.Name "fieldName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

undefinedTermError :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.UndefinedTermError
undefinedTermError name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTermError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

undefinedTermErrorName :: Phantoms.TTerm Core_.UndefinedTermError -> Phantoms.TTerm Core.Name
undefinedTermErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTermError"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

undefinedTermErrorWithName :: Phantoms.TTerm Core_.UndefinedTermError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.UndefinedTermError
undefinedTermErrorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTermError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

undefinedTypeError :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.UndefinedTypeError
undefinedTypeError name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

undefinedTypeErrorName :: Phantoms.TTerm Core_.UndefinedTypeError -> Phantoms.TTerm Core.Name
undefinedTypeErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeError"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

undefinedTypeErrorWithName :: Phantoms.TTerm Core_.UndefinedTypeError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.UndefinedTypeError
undefinedTypeErrorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

unexpectedTermVariantError :: Phantoms.TTerm Variants.TermVariant -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Core_.UnexpectedTermVariantError
unexpectedTermVariantError expectedVariant actualTerm =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UnexpectedTermVariantError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expectedVariant"),
          Core.fieldTerm = (Phantoms.unTTerm expectedVariant)},
        Core.Field {
          Core.fieldName = (Core.Name "actualTerm"),
          Core.fieldTerm = (Phantoms.unTTerm actualTerm)}]}))

unexpectedTermVariantErrorExpectedVariant :: Phantoms.TTerm Core_.UnexpectedTermVariantError -> Phantoms.TTerm Variants.TermVariant
unexpectedTermVariantErrorExpectedVariant x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UnexpectedTermVariantError"),
        Core.projectionField = (Core.Name "expectedVariant")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unexpectedTermVariantErrorActualTerm :: Phantoms.TTerm Core_.UnexpectedTermVariantError -> Phantoms.TTerm Core.Term
unexpectedTermVariantErrorActualTerm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UnexpectedTermVariantError"),
        Core.projectionField = (Core.Name "actualTerm")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unexpectedTermVariantErrorWithExpectedVariant :: Phantoms.TTerm Core_.UnexpectedTermVariantError -> Phantoms.TTerm Variants.TermVariant -> Phantoms.TTerm Core_.UnexpectedTermVariantError
unexpectedTermVariantErrorWithExpectedVariant original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UnexpectedTermVariantError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expectedVariant"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "actualTerm"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UnexpectedTermVariantError"),
              Core.projectionField = (Core.Name "actualTerm")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unexpectedTermVariantErrorWithActualTerm :: Phantoms.TTerm Core_.UnexpectedTermVariantError -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Core_.UnexpectedTermVariantError
unexpectedTermVariantErrorWithActualTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UnexpectedTermVariantError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expectedVariant"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UnexpectedTermVariantError"),
              Core.projectionField = (Core.Name "expectedVariant")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "actualTerm"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

unexpectedTypeVariantError :: Phantoms.TTerm Variants.TypeVariant -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Core_.UnexpectedTypeVariantError
unexpectedTypeVariantError expectedVariant actualType =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UnexpectedTypeVariantError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expectedVariant"),
          Core.fieldTerm = (Phantoms.unTTerm expectedVariant)},
        Core.Field {
          Core.fieldName = (Core.Name "actualType"),
          Core.fieldTerm = (Phantoms.unTTerm actualType)}]}))

unexpectedTypeVariantErrorExpectedVariant :: Phantoms.TTerm Core_.UnexpectedTypeVariantError -> Phantoms.TTerm Variants.TypeVariant
unexpectedTypeVariantErrorExpectedVariant x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UnexpectedTypeVariantError"),
        Core.projectionField = (Core.Name "expectedVariant")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unexpectedTypeVariantErrorActualType :: Phantoms.TTerm Core_.UnexpectedTypeVariantError -> Phantoms.TTerm Core.Type
unexpectedTypeVariantErrorActualType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UnexpectedTypeVariantError"),
        Core.projectionField = (Core.Name "actualType")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unexpectedTypeVariantErrorWithExpectedVariant :: Phantoms.TTerm Core_.UnexpectedTypeVariantError -> Phantoms.TTerm Variants.TypeVariant -> Phantoms.TTerm Core_.UnexpectedTypeVariantError
unexpectedTypeVariantErrorWithExpectedVariant original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UnexpectedTypeVariantError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expectedVariant"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "actualType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UnexpectedTypeVariantError"),
              Core.projectionField = (Core.Name "actualType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unexpectedTypeVariantErrorWithActualType :: Phantoms.TTerm Core_.UnexpectedTypeVariantError -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Core_.UnexpectedTypeVariantError
unexpectedTypeVariantErrorWithActualType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UnexpectedTypeVariantError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expectedVariant"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UnexpectedTypeVariantError"),
              Core.projectionField = (Core.Name "expectedVariant")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "actualType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
