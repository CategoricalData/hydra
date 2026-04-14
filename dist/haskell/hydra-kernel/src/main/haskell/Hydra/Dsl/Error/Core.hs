-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.error.core

module Hydra.Dsl.Error.Core where

import qualified Hydra.Core as Core
import qualified Hydra.Error.Core as Core_
import qualified Hydra.Paths as Paths
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

constantConditionError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Bool -> Phantoms.TTerm Core_.ConstantConditionError
constantConditionError location value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.ConstantConditionError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))

constantConditionErrorLocation :: Phantoms.TTerm Core_.ConstantConditionError -> Phantoms.TTerm Paths.SubtermPath
constantConditionErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.ConstantConditionError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

constantConditionErrorValue :: Phantoms.TTerm Core_.ConstantConditionError -> Phantoms.TTerm Bool
constantConditionErrorValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.ConstantConditionError"),
        Core.projectionField = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

constantConditionErrorWithLocation :: Phantoms.TTerm Core_.ConstantConditionError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.ConstantConditionError
constantConditionErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.ConstantConditionError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.ConstantConditionError"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

constantConditionErrorWithValue :: Phantoms.TTerm Core_.ConstantConditionError -> Phantoms.TTerm Bool -> Phantoms.TTerm Core_.ConstantConditionError
constantConditionErrorWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.ConstantConditionError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.ConstantConditionError"),
              Core.projectionField = (Core.Name "location")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

duplicateBindingError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.DuplicateBindingError
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

duplicateBindingErrorLocation :: Phantoms.TTerm Core_.DuplicateBindingError -> Phantoms.TTerm Paths.SubtermPath
duplicateBindingErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateBindingError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

duplicateBindingErrorName :: Phantoms.TTerm Core_.DuplicateBindingError -> Phantoms.TTerm Core.Name
duplicateBindingErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateBindingError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

duplicateBindingErrorWithLocation :: Phantoms.TTerm Core_.DuplicateBindingError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.DuplicateBindingError
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
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateBindingError"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

duplicateBindingErrorWithName :: Phantoms.TTerm Core_.DuplicateBindingError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.DuplicateBindingError
duplicateBindingErrorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.DuplicateBindingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateBindingError"),
              Core.projectionField = (Core.Name "location")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

duplicateFieldError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.DuplicateFieldError
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

duplicateFieldErrorLocation :: Phantoms.TTerm Core_.DuplicateFieldError -> Phantoms.TTerm Paths.SubtermPath
duplicateFieldErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateFieldError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

duplicateFieldErrorName :: Phantoms.TTerm Core_.DuplicateFieldError -> Phantoms.TTerm Core.Name
duplicateFieldErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateFieldError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

duplicateFieldErrorWithLocation :: Phantoms.TTerm Core_.DuplicateFieldError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.DuplicateFieldError
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
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateFieldError"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

duplicateFieldErrorWithName :: Phantoms.TTerm Core_.DuplicateFieldError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.DuplicateFieldError
duplicateFieldErrorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.DuplicateFieldError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateFieldError"),
              Core.projectionField = (Core.Name "location")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

duplicateRecordTypeFieldNamesError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.DuplicateRecordTypeFieldNamesError
duplicateRecordTypeFieldNamesError location name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.DuplicateRecordTypeFieldNamesError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

duplicateRecordTypeFieldNamesErrorLocation :: Phantoms.TTerm Core_.DuplicateRecordTypeFieldNamesError -> Phantoms.TTerm Paths.SubtermPath
duplicateRecordTypeFieldNamesErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateRecordTypeFieldNamesError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

duplicateRecordTypeFieldNamesErrorName :: Phantoms.TTerm Core_.DuplicateRecordTypeFieldNamesError -> Phantoms.TTerm Core.Name
duplicateRecordTypeFieldNamesErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateRecordTypeFieldNamesError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

duplicateRecordTypeFieldNamesErrorWithLocation :: Phantoms.TTerm Core_.DuplicateRecordTypeFieldNamesError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.DuplicateRecordTypeFieldNamesError
duplicateRecordTypeFieldNamesErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.DuplicateRecordTypeFieldNamesError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateRecordTypeFieldNamesError"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

duplicateRecordTypeFieldNamesErrorWithName :: Phantoms.TTerm Core_.DuplicateRecordTypeFieldNamesError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.DuplicateRecordTypeFieldNamesError
duplicateRecordTypeFieldNamesErrorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.DuplicateRecordTypeFieldNamesError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateRecordTypeFieldNamesError"),
              Core.projectionField = (Core.Name "location")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

duplicateUnionTypeFieldNamesError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.DuplicateUnionTypeFieldNamesError
duplicateUnionTypeFieldNamesError location name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.DuplicateUnionTypeFieldNamesError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

duplicateUnionTypeFieldNamesErrorLocation :: Phantoms.TTerm Core_.DuplicateUnionTypeFieldNamesError -> Phantoms.TTerm Paths.SubtermPath
duplicateUnionTypeFieldNamesErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateUnionTypeFieldNamesError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

duplicateUnionTypeFieldNamesErrorName :: Phantoms.TTerm Core_.DuplicateUnionTypeFieldNamesError -> Phantoms.TTerm Core.Name
duplicateUnionTypeFieldNamesErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateUnionTypeFieldNamesError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

duplicateUnionTypeFieldNamesErrorWithLocation :: Phantoms.TTerm Core_.DuplicateUnionTypeFieldNamesError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.DuplicateUnionTypeFieldNamesError
duplicateUnionTypeFieldNamesErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.DuplicateUnionTypeFieldNamesError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateUnionTypeFieldNamesError"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

duplicateUnionTypeFieldNamesErrorWithName :: Phantoms.TTerm Core_.DuplicateUnionTypeFieldNamesError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.DuplicateUnionTypeFieldNamesError
duplicateUnionTypeFieldNamesErrorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.DuplicateUnionTypeFieldNamesError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateUnionTypeFieldNamesError"),
              Core.projectionField = (Core.Name "location")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

emptyCaseStatementError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.EmptyCaseStatementError
emptyCaseStatementError location typeName =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyCaseStatementError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Phantoms.unTTerm typeName)}]}))

emptyCaseStatementErrorLocation :: Phantoms.TTerm Core_.EmptyCaseStatementError -> Phantoms.TTerm Paths.SubtermPath
emptyCaseStatementErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.EmptyCaseStatementError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

emptyCaseStatementErrorTypeName :: Phantoms.TTerm Core_.EmptyCaseStatementError -> Phantoms.TTerm Core.Name
emptyCaseStatementErrorTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.EmptyCaseStatementError"),
        Core.projectionField = (Core.Name "typeName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

emptyCaseStatementErrorWithLocation :: Phantoms.TTerm Core_.EmptyCaseStatementError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.EmptyCaseStatementError
emptyCaseStatementErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyCaseStatementError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.EmptyCaseStatementError"),
              Core.projectionField = (Core.Name "typeName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

emptyCaseStatementErrorWithTypeName :: Phantoms.TTerm Core_.EmptyCaseStatementError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.EmptyCaseStatementError
emptyCaseStatementErrorWithTypeName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyCaseStatementError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.EmptyCaseStatementError"),
              Core.projectionField = (Core.Name "location")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

emptyLetBindingsError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.EmptyLetBindingsError
emptyLetBindingsError location =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyLetBindingsError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)}]}))

emptyLetBindingsErrorLocation :: Phantoms.TTerm Core_.EmptyLetBindingsError -> Phantoms.TTerm Paths.SubtermPath
emptyLetBindingsErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.EmptyLetBindingsError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

emptyLetBindingsErrorWithLocation :: Phantoms.TTerm Core_.EmptyLetBindingsError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.EmptyLetBindingsError
emptyLetBindingsErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyLetBindingsError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

emptyRecordTypeError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.EmptyRecordTypeError
emptyRecordTypeError location =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyRecordTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)}]}))

emptyRecordTypeErrorLocation :: Phantoms.TTerm Core_.EmptyRecordTypeError -> Phantoms.TTerm Paths.SubtermPath
emptyRecordTypeErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.EmptyRecordTypeError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

emptyRecordTypeErrorWithLocation :: Phantoms.TTerm Core_.EmptyRecordTypeError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.EmptyRecordTypeError
emptyRecordTypeErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyRecordTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

emptyTermAnnotationError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.EmptyTermAnnotationError
emptyTermAnnotationError location =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyTermAnnotationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)}]}))

emptyTermAnnotationErrorLocation :: Phantoms.TTerm Core_.EmptyTermAnnotationError -> Phantoms.TTerm Paths.SubtermPath
emptyTermAnnotationErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.EmptyTermAnnotationError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

emptyTermAnnotationErrorWithLocation :: Phantoms.TTerm Core_.EmptyTermAnnotationError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.EmptyTermAnnotationError
emptyTermAnnotationErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyTermAnnotationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

emptyTypeAnnotationError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.EmptyTypeAnnotationError
emptyTypeAnnotationError location =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyTypeAnnotationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)}]}))

emptyTypeAnnotationErrorLocation :: Phantoms.TTerm Core_.EmptyTypeAnnotationError -> Phantoms.TTerm Paths.SubtermPath
emptyTypeAnnotationErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.EmptyTypeAnnotationError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

emptyTypeAnnotationErrorWithLocation :: Phantoms.TTerm Core_.EmptyTypeAnnotationError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.EmptyTypeAnnotationError
emptyTypeAnnotationErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyTypeAnnotationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

emptyTypeNameInTermError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.EmptyTypeNameInTermError
emptyTypeNameInTermError location =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyTypeNameInTermError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)}]}))

emptyTypeNameInTermErrorLocation :: Phantoms.TTerm Core_.EmptyTypeNameInTermError -> Phantoms.TTerm Paths.SubtermPath
emptyTypeNameInTermErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.EmptyTypeNameInTermError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

emptyTypeNameInTermErrorWithLocation :: Phantoms.TTerm Core_.EmptyTypeNameInTermError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.EmptyTypeNameInTermError
emptyTypeNameInTermErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyTypeNameInTermError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

emptyUnionTypeError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.EmptyUnionTypeError
emptyUnionTypeError location =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyUnionTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)}]}))

emptyUnionTypeErrorLocation :: Phantoms.TTerm Core_.EmptyUnionTypeError -> Phantoms.TTerm Paths.SubtermPath
emptyUnionTypeErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.EmptyUnionTypeError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

emptyUnionTypeErrorWithLocation :: Phantoms.TTerm Core_.EmptyUnionTypeError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.EmptyUnionTypeError
emptyUnionTypeErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyUnionTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

invalidForallParameterNameError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.InvalidForallParameterNameError
invalidForallParameterNameError location name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.InvalidForallParameterNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

invalidForallParameterNameErrorLocation :: Phantoms.TTerm Core_.InvalidForallParameterNameError -> Phantoms.TTerm Paths.SubtermPath
invalidForallParameterNameErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidForallParameterNameError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

invalidForallParameterNameErrorName :: Phantoms.TTerm Core_.InvalidForallParameterNameError -> Phantoms.TTerm Core.Name
invalidForallParameterNameErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidForallParameterNameError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

invalidForallParameterNameErrorWithLocation :: Phantoms.TTerm Core_.InvalidForallParameterNameError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.InvalidForallParameterNameError
invalidForallParameterNameErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.InvalidForallParameterNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidForallParameterNameError"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

invalidForallParameterNameErrorWithName :: Phantoms.TTerm Core_.InvalidForallParameterNameError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.InvalidForallParameterNameError
invalidForallParameterNameErrorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.InvalidForallParameterNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidForallParameterNameError"),
              Core.projectionField = (Core.Name "location")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

invalidLambdaParameterNameError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.InvalidLambdaParameterNameError
invalidLambdaParameterNameError location name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.InvalidLambdaParameterNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

invalidLambdaParameterNameErrorLocation :: Phantoms.TTerm Core_.InvalidLambdaParameterNameError -> Phantoms.TTerm Paths.SubtermPath
invalidLambdaParameterNameErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidLambdaParameterNameError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

invalidLambdaParameterNameErrorName :: Phantoms.TTerm Core_.InvalidLambdaParameterNameError -> Phantoms.TTerm Core.Name
invalidLambdaParameterNameErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidLambdaParameterNameError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

invalidLambdaParameterNameErrorWithLocation :: Phantoms.TTerm Core_.InvalidLambdaParameterNameError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.InvalidLambdaParameterNameError
invalidLambdaParameterNameErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.InvalidLambdaParameterNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidLambdaParameterNameError"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

invalidLambdaParameterNameErrorWithName :: Phantoms.TTerm Core_.InvalidLambdaParameterNameError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.InvalidLambdaParameterNameError
invalidLambdaParameterNameErrorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.InvalidLambdaParameterNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidLambdaParameterNameError"),
              Core.projectionField = (Core.Name "location")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

invalidLetBindingNameError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.InvalidLetBindingNameError
invalidLetBindingNameError location name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.InvalidLetBindingNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

invalidLetBindingNameErrorLocation :: Phantoms.TTerm Core_.InvalidLetBindingNameError -> Phantoms.TTerm Paths.SubtermPath
invalidLetBindingNameErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidLetBindingNameError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

invalidLetBindingNameErrorName :: Phantoms.TTerm Core_.InvalidLetBindingNameError -> Phantoms.TTerm Core.Name
invalidLetBindingNameErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidLetBindingNameError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

invalidLetBindingNameErrorWithLocation :: Phantoms.TTerm Core_.InvalidLetBindingNameError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.InvalidLetBindingNameError
invalidLetBindingNameErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.InvalidLetBindingNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidLetBindingNameError"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

invalidLetBindingNameErrorWithName :: Phantoms.TTerm Core_.InvalidLetBindingNameError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.InvalidLetBindingNameError
invalidLetBindingNameErrorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.InvalidLetBindingNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidLetBindingNameError"),
              Core.projectionField = (Core.Name "location")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

invalidTermErrorConstantCondition :: Phantoms.TTerm Core_.ConstantConditionError -> Phantoms.TTerm Core_.InvalidTermError
invalidTermErrorConstantCondition x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "constantCondition"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidTermErrorDuplicateBinding :: Phantoms.TTerm Core_.DuplicateBindingError -> Phantoms.TTerm Core_.InvalidTermError
invalidTermErrorDuplicateBinding x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "duplicateBinding"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidTermErrorDuplicateField :: Phantoms.TTerm Core_.DuplicateFieldError -> Phantoms.TTerm Core_.InvalidTermError
invalidTermErrorDuplicateField x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "duplicateField"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidTermErrorEmptyCaseStatement :: Phantoms.TTerm Core_.EmptyCaseStatementError -> Phantoms.TTerm Core_.InvalidTermError
invalidTermErrorEmptyCaseStatement x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "emptyCaseStatement"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidTermErrorEmptyLetBindings :: Phantoms.TTerm Core_.EmptyLetBindingsError -> Phantoms.TTerm Core_.InvalidTermError
invalidTermErrorEmptyLetBindings x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "emptyLetBindings"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidTermErrorEmptyTermAnnotation :: Phantoms.TTerm Core_.EmptyTermAnnotationError -> Phantoms.TTerm Core_.InvalidTermError
invalidTermErrorEmptyTermAnnotation x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "emptyTermAnnotation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidTermErrorEmptyTypeNameInTerm :: Phantoms.TTerm Core_.EmptyTypeNameInTermError -> Phantoms.TTerm Core_.InvalidTermError
invalidTermErrorEmptyTypeNameInTerm x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "emptyTypeNameInTerm"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidTermErrorInvalidLambdaParameterName :: Phantoms.TTerm Core_.InvalidLambdaParameterNameError -> Phantoms.TTerm Core_.InvalidTermError
invalidTermErrorInvalidLambdaParameterName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "invalidLambdaParameterName"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidTermErrorInvalidLetBindingName :: Phantoms.TTerm Core_.InvalidLetBindingNameError -> Phantoms.TTerm Core_.InvalidTermError
invalidTermErrorInvalidLetBindingName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "invalidLetBindingName"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidTermErrorInvalidTypeLambdaParameterName :: Phantoms.TTerm Core_.InvalidTypeLambdaParameterNameError -> Phantoms.TTerm Core_.InvalidTermError
invalidTermErrorInvalidTypeLambdaParameterName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "invalidTypeLambdaParameterName"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidTermErrorNestedTermAnnotation :: Phantoms.TTerm Core_.NestedTermAnnotationError -> Phantoms.TTerm Core_.InvalidTermError
invalidTermErrorNestedTermAnnotation x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nestedTermAnnotation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidTermErrorRedundantWrapUnwrap :: Phantoms.TTerm Core_.RedundantWrapUnwrapError -> Phantoms.TTerm Core_.InvalidTermError
invalidTermErrorRedundantWrapUnwrap x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "redundantWrapUnwrap"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidTermErrorSelfApplication :: Phantoms.TTerm Core_.SelfApplicationError -> Phantoms.TTerm Core_.InvalidTermError
invalidTermErrorSelfApplication x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "selfApplication"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidTermErrorTermVariableShadowing :: Phantoms.TTerm Core_.TermVariableShadowingError -> Phantoms.TTerm Core_.InvalidTermError
invalidTermErrorTermVariableShadowing x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "termVariableShadowing"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidTermErrorTypeVariableShadowingInTypeLambda :: Phantoms.TTerm Core_.TypeVariableShadowingInTypeLambdaError -> Phantoms.TTerm Core_.InvalidTermError
invalidTermErrorTypeVariableShadowingInTypeLambda x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeVariableShadowingInTypeLambda"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidTermErrorUndefinedTermVariable :: Phantoms.TTerm Core_.UndefinedTermVariableError -> Phantoms.TTerm Core_.InvalidTermError
invalidTermErrorUndefinedTermVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "undefinedTermVariable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidTermErrorUndefinedTypeVariableInBindingType :: Phantoms.TTerm Core_.UndefinedTypeVariableInBindingTypeError -> Phantoms.TTerm Core_.InvalidTermError
invalidTermErrorUndefinedTypeVariableInBindingType x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "undefinedTypeVariableInBindingType"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidTermErrorUndefinedTypeVariableInLambdaDomain :: Phantoms.TTerm Core_.UndefinedTypeVariableInLambdaDomainError -> Phantoms.TTerm Core_.InvalidTermError
invalidTermErrorUndefinedTypeVariableInLambdaDomain x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "undefinedTypeVariableInLambdaDomain"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidTermErrorUndefinedTypeVariableInTypeApplication :: Phantoms.TTerm Core_.UndefinedTypeVariableInTypeApplicationError -> Phantoms.TTerm Core_.InvalidTermError
invalidTermErrorUndefinedTypeVariableInTypeApplication x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "undefinedTypeVariableInTypeApplication"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidTermErrorUnknownPrimitiveName :: Phantoms.TTerm Core_.UnknownPrimitiveNameError -> Phantoms.TTerm Core_.InvalidTermError
invalidTermErrorUnknownPrimitiveName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unknownPrimitiveName"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidTermErrorUnnecessaryIdentityApplication :: Phantoms.TTerm Core_.UnnecessaryIdentityApplicationError -> Phantoms.TTerm Core_.InvalidTermError
invalidTermErrorUnnecessaryIdentityApplication x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unnecessaryIdentityApplication"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidTermErrorUntypedTermVariable :: Phantoms.TTerm Core_.UntypedTermVariableError -> Phantoms.TTerm Core_.InvalidTermError
invalidTermErrorUntypedTermVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "untypedTermVariable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidTypeErrorDuplicateRecordTypeFieldNames :: Phantoms.TTerm Core_.DuplicateRecordTypeFieldNamesError -> Phantoms.TTerm Core_.InvalidTypeError
invalidTypeErrorDuplicateRecordTypeFieldNames x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "duplicateRecordTypeFieldNames"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidTypeErrorDuplicateUnionTypeFieldNames :: Phantoms.TTerm Core_.DuplicateUnionTypeFieldNamesError -> Phantoms.TTerm Core_.InvalidTypeError
invalidTypeErrorDuplicateUnionTypeFieldNames x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "duplicateUnionTypeFieldNames"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidTypeErrorEmptyRecordType :: Phantoms.TTerm Core_.EmptyRecordTypeError -> Phantoms.TTerm Core_.InvalidTypeError
invalidTypeErrorEmptyRecordType x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "emptyRecordType"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidTypeErrorEmptyTypeAnnotation :: Phantoms.TTerm Core_.EmptyTypeAnnotationError -> Phantoms.TTerm Core_.InvalidTypeError
invalidTypeErrorEmptyTypeAnnotation x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "emptyTypeAnnotation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidTypeErrorEmptyUnionType :: Phantoms.TTerm Core_.EmptyUnionTypeError -> Phantoms.TTerm Core_.InvalidTypeError
invalidTypeErrorEmptyUnionType x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "emptyUnionType"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidTypeErrorInvalidForallParameterName :: Phantoms.TTerm Core_.InvalidForallParameterNameError -> Phantoms.TTerm Core_.InvalidTypeError
invalidTypeErrorInvalidForallParameterName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "invalidForallParameterName"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidTypeErrorInvalidTypeSchemeVariableName :: Phantoms.TTerm Core_.InvalidTypeSchemeVariableNameError -> Phantoms.TTerm Core_.InvalidTypeError
invalidTypeErrorInvalidTypeSchemeVariableName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "invalidTypeSchemeVariableName"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidTypeErrorNestedTypeAnnotation :: Phantoms.TTerm Core_.NestedTypeAnnotationError -> Phantoms.TTerm Core_.InvalidTypeError
invalidTypeErrorNestedTypeAnnotation x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nestedTypeAnnotation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidTypeErrorNonComparableMapKeyType :: Phantoms.TTerm Core_.NonComparableMapKeyTypeError -> Phantoms.TTerm Core_.InvalidTypeError
invalidTypeErrorNonComparableMapKeyType x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nonComparableMapKeyType"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidTypeErrorNonComparableSetElementType :: Phantoms.TTerm Core_.NonComparableSetElementTypeError -> Phantoms.TTerm Core_.InvalidTypeError
invalidTypeErrorNonComparableSetElementType x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nonComparableSetElementType"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidTypeErrorSingleVariantUnion :: Phantoms.TTerm Core_.SingleVariantUnionError -> Phantoms.TTerm Core_.InvalidTypeError
invalidTypeErrorSingleVariantUnion x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "singleVariantUnion"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidTypeErrorTypeVariableShadowingInForall :: Phantoms.TTerm Core_.TypeVariableShadowingInForallError -> Phantoms.TTerm Core_.InvalidTypeError
invalidTypeErrorTypeVariableShadowingInForall x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeVariableShadowingInForall"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidTypeErrorUndefinedTypeVariable :: Phantoms.TTerm Core_.UndefinedTypeVariableError -> Phantoms.TTerm Core_.InvalidTypeError
invalidTypeErrorUndefinedTypeVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "undefinedTypeVariable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidTypeErrorVoidInNonBottomPosition :: Phantoms.TTerm Core_.VoidInNonBottomPositionError -> Phantoms.TTerm Core_.InvalidTypeError
invalidTypeErrorVoidInNonBottomPosition x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "voidInNonBottomPosition"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidTypeLambdaParameterNameError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.InvalidTypeLambdaParameterNameError
invalidTypeLambdaParameterNameError location name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.InvalidTypeLambdaParameterNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

invalidTypeLambdaParameterNameErrorLocation :: Phantoms.TTerm Core_.InvalidTypeLambdaParameterNameError -> Phantoms.TTerm Paths.SubtermPath
invalidTypeLambdaParameterNameErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidTypeLambdaParameterNameError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

invalidTypeLambdaParameterNameErrorName :: Phantoms.TTerm Core_.InvalidTypeLambdaParameterNameError -> Phantoms.TTerm Core.Name
invalidTypeLambdaParameterNameErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidTypeLambdaParameterNameError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

invalidTypeLambdaParameterNameErrorWithLocation :: Phantoms.TTerm Core_.InvalidTypeLambdaParameterNameError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.InvalidTypeLambdaParameterNameError
invalidTypeLambdaParameterNameErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.InvalidTypeLambdaParameterNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidTypeLambdaParameterNameError"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

invalidTypeLambdaParameterNameErrorWithName :: Phantoms.TTerm Core_.InvalidTypeLambdaParameterNameError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.InvalidTypeLambdaParameterNameError
invalidTypeLambdaParameterNameErrorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.InvalidTypeLambdaParameterNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidTypeLambdaParameterNameError"),
              Core.projectionField = (Core.Name "location")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

invalidTypeSchemeVariableNameError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.InvalidTypeSchemeVariableNameError
invalidTypeSchemeVariableNameError location name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.InvalidTypeSchemeVariableNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

invalidTypeSchemeVariableNameErrorLocation :: Phantoms.TTerm Core_.InvalidTypeSchemeVariableNameError -> Phantoms.TTerm Paths.SubtermPath
invalidTypeSchemeVariableNameErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidTypeSchemeVariableNameError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

invalidTypeSchemeVariableNameErrorName :: Phantoms.TTerm Core_.InvalidTypeSchemeVariableNameError -> Phantoms.TTerm Core.Name
invalidTypeSchemeVariableNameErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidTypeSchemeVariableNameError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

invalidTypeSchemeVariableNameErrorWithLocation :: Phantoms.TTerm Core_.InvalidTypeSchemeVariableNameError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.InvalidTypeSchemeVariableNameError
invalidTypeSchemeVariableNameErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.InvalidTypeSchemeVariableNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidTypeSchemeVariableNameError"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

invalidTypeSchemeVariableNameErrorWithName :: Phantoms.TTerm Core_.InvalidTypeSchemeVariableNameError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.InvalidTypeSchemeVariableNameError
invalidTypeSchemeVariableNameErrorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.InvalidTypeSchemeVariableNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidTypeSchemeVariableNameError"),
              Core.projectionField = (Core.Name "location")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

nestedTermAnnotationError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.NestedTermAnnotationError
nestedTermAnnotationError location =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.NestedTermAnnotationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)}]}))

nestedTermAnnotationErrorLocation :: Phantoms.TTerm Core_.NestedTermAnnotationError -> Phantoms.TTerm Paths.SubtermPath
nestedTermAnnotationErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.NestedTermAnnotationError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

nestedTermAnnotationErrorWithLocation :: Phantoms.TTerm Core_.NestedTermAnnotationError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.NestedTermAnnotationError
nestedTermAnnotationErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.NestedTermAnnotationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

nestedTypeAnnotationError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.NestedTypeAnnotationError
nestedTypeAnnotationError location =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.NestedTypeAnnotationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)}]}))

nestedTypeAnnotationErrorLocation :: Phantoms.TTerm Core_.NestedTypeAnnotationError -> Phantoms.TTerm Paths.SubtermPath
nestedTypeAnnotationErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.NestedTypeAnnotationError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

nestedTypeAnnotationErrorWithLocation :: Phantoms.TTerm Core_.NestedTypeAnnotationError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.NestedTypeAnnotationError
nestedTypeAnnotationErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.NestedTypeAnnotationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

nonComparableMapKeyTypeError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Core_.NonComparableMapKeyTypeError
nonComparableMapKeyTypeError location keyType =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.NonComparableMapKeyTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "keyType"),
          Core.fieldTerm = (Phantoms.unTTerm keyType)}]}))

nonComparableMapKeyTypeErrorKeyType :: Phantoms.TTerm Core_.NonComparableMapKeyTypeError -> Phantoms.TTerm Core.Type
nonComparableMapKeyTypeErrorKeyType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.NonComparableMapKeyTypeError"),
        Core.projectionField = (Core.Name "keyType")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

nonComparableMapKeyTypeErrorLocation :: Phantoms.TTerm Core_.NonComparableMapKeyTypeError -> Phantoms.TTerm Paths.SubtermPath
nonComparableMapKeyTypeErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.NonComparableMapKeyTypeError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

nonComparableMapKeyTypeErrorWithKeyType :: Phantoms.TTerm Core_.NonComparableMapKeyTypeError -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Core_.NonComparableMapKeyTypeError
nonComparableMapKeyTypeErrorWithKeyType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.NonComparableMapKeyTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.NonComparableMapKeyTypeError"),
              Core.projectionField = (Core.Name "location")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "keyType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

nonComparableMapKeyTypeErrorWithLocation :: Phantoms.TTerm Core_.NonComparableMapKeyTypeError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.NonComparableMapKeyTypeError
nonComparableMapKeyTypeErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.NonComparableMapKeyTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "keyType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.NonComparableMapKeyTypeError"),
              Core.projectionField = (Core.Name "keyType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

nonComparableSetElementTypeError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Core_.NonComparableSetElementTypeError
nonComparableSetElementTypeError location elementType =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.NonComparableSetElementTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "elementType"),
          Core.fieldTerm = (Phantoms.unTTerm elementType)}]}))

nonComparableSetElementTypeErrorElementType :: Phantoms.TTerm Core_.NonComparableSetElementTypeError -> Phantoms.TTerm Core.Type
nonComparableSetElementTypeErrorElementType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.NonComparableSetElementTypeError"),
        Core.projectionField = (Core.Name "elementType")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

nonComparableSetElementTypeErrorLocation :: Phantoms.TTerm Core_.NonComparableSetElementTypeError -> Phantoms.TTerm Paths.SubtermPath
nonComparableSetElementTypeErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.NonComparableSetElementTypeError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

nonComparableSetElementTypeErrorWithElementType :: Phantoms.TTerm Core_.NonComparableSetElementTypeError -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Core_.NonComparableSetElementTypeError
nonComparableSetElementTypeErrorWithElementType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.NonComparableSetElementTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.NonComparableSetElementTypeError"),
              Core.projectionField = (Core.Name "location")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "elementType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

nonComparableSetElementTypeErrorWithLocation :: Phantoms.TTerm Core_.NonComparableSetElementTypeError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.NonComparableSetElementTypeError
nonComparableSetElementTypeErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.NonComparableSetElementTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "elementType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.NonComparableSetElementTypeError"),
              Core.projectionField = (Core.Name "elementType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

redundantWrapUnwrapError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.RedundantWrapUnwrapError
redundantWrapUnwrapError location typeName =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.RedundantWrapUnwrapError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Phantoms.unTTerm typeName)}]}))

redundantWrapUnwrapErrorLocation :: Phantoms.TTerm Core_.RedundantWrapUnwrapError -> Phantoms.TTerm Paths.SubtermPath
redundantWrapUnwrapErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.RedundantWrapUnwrapError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

redundantWrapUnwrapErrorTypeName :: Phantoms.TTerm Core_.RedundantWrapUnwrapError -> Phantoms.TTerm Core.Name
redundantWrapUnwrapErrorTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.RedundantWrapUnwrapError"),
        Core.projectionField = (Core.Name "typeName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

redundantWrapUnwrapErrorWithLocation :: Phantoms.TTerm Core_.RedundantWrapUnwrapError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.RedundantWrapUnwrapError
redundantWrapUnwrapErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.RedundantWrapUnwrapError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.RedundantWrapUnwrapError"),
              Core.projectionField = (Core.Name "typeName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

redundantWrapUnwrapErrorWithTypeName :: Phantoms.TTerm Core_.RedundantWrapUnwrapError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.RedundantWrapUnwrapError
redundantWrapUnwrapErrorWithTypeName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.RedundantWrapUnwrapError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.RedundantWrapUnwrapError"),
              Core.projectionField = (Core.Name "location")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

selfApplicationError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.SelfApplicationError
selfApplicationError location name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.SelfApplicationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

selfApplicationErrorLocation :: Phantoms.TTerm Core_.SelfApplicationError -> Phantoms.TTerm Paths.SubtermPath
selfApplicationErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.SelfApplicationError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

selfApplicationErrorName :: Phantoms.TTerm Core_.SelfApplicationError -> Phantoms.TTerm Core.Name
selfApplicationErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.SelfApplicationError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

selfApplicationErrorWithLocation :: Phantoms.TTerm Core_.SelfApplicationError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.SelfApplicationError
selfApplicationErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.SelfApplicationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.SelfApplicationError"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

selfApplicationErrorWithName :: Phantoms.TTerm Core_.SelfApplicationError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.SelfApplicationError
selfApplicationErrorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.SelfApplicationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.SelfApplicationError"),
              Core.projectionField = (Core.Name "location")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

singleVariantUnionError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.SingleVariantUnionError
singleVariantUnionError location fieldName =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.SingleVariantUnionError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "fieldName"),
          Core.fieldTerm = (Phantoms.unTTerm fieldName)}]}))

singleVariantUnionErrorFieldName :: Phantoms.TTerm Core_.SingleVariantUnionError -> Phantoms.TTerm Core.Name
singleVariantUnionErrorFieldName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.SingleVariantUnionError"),
        Core.projectionField = (Core.Name "fieldName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

singleVariantUnionErrorLocation :: Phantoms.TTerm Core_.SingleVariantUnionError -> Phantoms.TTerm Paths.SubtermPath
singleVariantUnionErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.SingleVariantUnionError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

singleVariantUnionErrorWithFieldName :: Phantoms.TTerm Core_.SingleVariantUnionError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.SingleVariantUnionError
singleVariantUnionErrorWithFieldName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.SingleVariantUnionError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.SingleVariantUnionError"),
              Core.projectionField = (Core.Name "location")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fieldName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

singleVariantUnionErrorWithLocation :: Phantoms.TTerm Core_.SingleVariantUnionError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.SingleVariantUnionError
singleVariantUnionErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.SingleVariantUnionError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fieldName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.SingleVariantUnionError"),
              Core.projectionField = (Core.Name "fieldName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

termVariableShadowingError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.TermVariableShadowingError
termVariableShadowingError location name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.TermVariableShadowingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

termVariableShadowingErrorLocation :: Phantoms.TTerm Core_.TermVariableShadowingError -> Phantoms.TTerm Paths.SubtermPath
termVariableShadowingErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.TermVariableShadowingError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

termVariableShadowingErrorName :: Phantoms.TTerm Core_.TermVariableShadowingError -> Phantoms.TTerm Core.Name
termVariableShadowingErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.TermVariableShadowingError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

termVariableShadowingErrorWithLocation :: Phantoms.TTerm Core_.TermVariableShadowingError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.TermVariableShadowingError
termVariableShadowingErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.TermVariableShadowingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.TermVariableShadowingError"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

termVariableShadowingErrorWithName :: Phantoms.TTerm Core_.TermVariableShadowingError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.TermVariableShadowingError
termVariableShadowingErrorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.TermVariableShadowingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.TermVariableShadowingError"),
              Core.projectionField = (Core.Name "location")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typeVariableShadowingInForallError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.TypeVariableShadowingInForallError
typeVariableShadowingInForallError location name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.TypeVariableShadowingInForallError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

typeVariableShadowingInForallErrorLocation :: Phantoms.TTerm Core_.TypeVariableShadowingInForallError -> Phantoms.TTerm Paths.SubtermPath
typeVariableShadowingInForallErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.TypeVariableShadowingInForallError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeVariableShadowingInForallErrorName :: Phantoms.TTerm Core_.TypeVariableShadowingInForallError -> Phantoms.TTerm Core.Name
typeVariableShadowingInForallErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.TypeVariableShadowingInForallError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeVariableShadowingInForallErrorWithLocation :: Phantoms.TTerm Core_.TypeVariableShadowingInForallError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.TypeVariableShadowingInForallError
typeVariableShadowingInForallErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.TypeVariableShadowingInForallError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.TypeVariableShadowingInForallError"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeVariableShadowingInForallErrorWithName :: Phantoms.TTerm Core_.TypeVariableShadowingInForallError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.TypeVariableShadowingInForallError
typeVariableShadowingInForallErrorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.TypeVariableShadowingInForallError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.TypeVariableShadowingInForallError"),
              Core.projectionField = (Core.Name "location")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typeVariableShadowingInTypeLambdaError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.TypeVariableShadowingInTypeLambdaError
typeVariableShadowingInTypeLambdaError location name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.TypeVariableShadowingInTypeLambdaError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

typeVariableShadowingInTypeLambdaErrorLocation :: Phantoms.TTerm Core_.TypeVariableShadowingInTypeLambdaError -> Phantoms.TTerm Paths.SubtermPath
typeVariableShadowingInTypeLambdaErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.TypeVariableShadowingInTypeLambdaError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeVariableShadowingInTypeLambdaErrorName :: Phantoms.TTerm Core_.TypeVariableShadowingInTypeLambdaError -> Phantoms.TTerm Core.Name
typeVariableShadowingInTypeLambdaErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.TypeVariableShadowingInTypeLambdaError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeVariableShadowingInTypeLambdaErrorWithLocation :: Phantoms.TTerm Core_.TypeVariableShadowingInTypeLambdaError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.TypeVariableShadowingInTypeLambdaError
typeVariableShadowingInTypeLambdaErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.TypeVariableShadowingInTypeLambdaError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.TypeVariableShadowingInTypeLambdaError"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeVariableShadowingInTypeLambdaErrorWithName :: Phantoms.TTerm Core_.TypeVariableShadowingInTypeLambdaError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.TypeVariableShadowingInTypeLambdaError
typeVariableShadowingInTypeLambdaErrorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.TypeVariableShadowingInTypeLambdaError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.TypeVariableShadowingInTypeLambdaError"),
              Core.projectionField = (Core.Name "location")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

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
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedFieldError"),
        Core.projectionField = (Core.Name "fieldName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

undefinedFieldErrorTypeName :: Phantoms.TTerm Core_.UndefinedFieldError -> Phantoms.TTerm Core.Name
undefinedFieldErrorTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedFieldError"),
        Core.projectionField = (Core.Name "typeName")})),
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
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedFieldError"),
              Core.projectionField = (Core.Name "typeName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

undefinedFieldErrorWithTypeName :: Phantoms.TTerm Core_.UndefinedFieldError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.UndefinedFieldError
undefinedFieldErrorWithTypeName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedFieldError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fieldName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedFieldError"),
              Core.projectionField = (Core.Name "fieldName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

undefinedTermVariableError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.UndefinedTermVariableError
undefinedTermVariableError location name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTermVariableError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

undefinedTermVariableErrorLocation :: Phantoms.TTerm Core_.UndefinedTermVariableError -> Phantoms.TTerm Paths.SubtermPath
undefinedTermVariableErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTermVariableError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

undefinedTermVariableErrorName :: Phantoms.TTerm Core_.UndefinedTermVariableError -> Phantoms.TTerm Core.Name
undefinedTermVariableErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTermVariableError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

undefinedTermVariableErrorWithLocation :: Phantoms.TTerm Core_.UndefinedTermVariableError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.UndefinedTermVariableError
undefinedTermVariableErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTermVariableError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTermVariableError"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

undefinedTermVariableErrorWithName :: Phantoms.TTerm Core_.UndefinedTermVariableError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.UndefinedTermVariableError
undefinedTermVariableErrorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTermVariableError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTermVariableError"),
              Core.projectionField = (Core.Name "location")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

undefinedTypeVariableError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.UndefinedTypeVariableError
undefinedTypeVariableError location name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

undefinedTypeVariableErrorLocation :: Phantoms.TTerm Core_.UndefinedTypeVariableError -> Phantoms.TTerm Paths.SubtermPath
undefinedTypeVariableErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

undefinedTypeVariableErrorName :: Phantoms.TTerm Core_.UndefinedTypeVariableError -> Phantoms.TTerm Core.Name
undefinedTypeVariableErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

undefinedTypeVariableErrorWithLocation :: Phantoms.TTerm Core_.UndefinedTypeVariableError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.UndefinedTypeVariableError
undefinedTypeVariableErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableError"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

undefinedTypeVariableErrorWithName :: Phantoms.TTerm Core_.UndefinedTypeVariableError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.UndefinedTypeVariableError
undefinedTypeVariableErrorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableError"),
              Core.projectionField = (Core.Name "location")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

undefinedTypeVariableInBindingTypeError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.UndefinedTypeVariableInBindingTypeError
undefinedTypeVariableInBindingTypeError location name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInBindingTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

undefinedTypeVariableInBindingTypeErrorLocation :: Phantoms.TTerm Core_.UndefinedTypeVariableInBindingTypeError -> Phantoms.TTerm Paths.SubtermPath
undefinedTypeVariableInBindingTypeErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInBindingTypeError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

undefinedTypeVariableInBindingTypeErrorName :: Phantoms.TTerm Core_.UndefinedTypeVariableInBindingTypeError -> Phantoms.TTerm Core.Name
undefinedTypeVariableInBindingTypeErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInBindingTypeError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

undefinedTypeVariableInBindingTypeErrorWithLocation :: Phantoms.TTerm Core_.UndefinedTypeVariableInBindingTypeError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.UndefinedTypeVariableInBindingTypeError
undefinedTypeVariableInBindingTypeErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInBindingTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInBindingTypeError"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

undefinedTypeVariableInBindingTypeErrorWithName :: Phantoms.TTerm Core_.UndefinedTypeVariableInBindingTypeError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.UndefinedTypeVariableInBindingTypeError
undefinedTypeVariableInBindingTypeErrorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInBindingTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInBindingTypeError"),
              Core.projectionField = (Core.Name "location")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

undefinedTypeVariableInLambdaDomainError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.UndefinedTypeVariableInLambdaDomainError
undefinedTypeVariableInLambdaDomainError location name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInLambdaDomainError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

undefinedTypeVariableInLambdaDomainErrorLocation :: Phantoms.TTerm Core_.UndefinedTypeVariableInLambdaDomainError -> Phantoms.TTerm Paths.SubtermPath
undefinedTypeVariableInLambdaDomainErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInLambdaDomainError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

undefinedTypeVariableInLambdaDomainErrorName :: Phantoms.TTerm Core_.UndefinedTypeVariableInLambdaDomainError -> Phantoms.TTerm Core.Name
undefinedTypeVariableInLambdaDomainErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInLambdaDomainError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

undefinedTypeVariableInLambdaDomainErrorWithLocation :: Phantoms.TTerm Core_.UndefinedTypeVariableInLambdaDomainError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.UndefinedTypeVariableInLambdaDomainError
undefinedTypeVariableInLambdaDomainErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInLambdaDomainError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInLambdaDomainError"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

undefinedTypeVariableInLambdaDomainErrorWithName :: Phantoms.TTerm Core_.UndefinedTypeVariableInLambdaDomainError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.UndefinedTypeVariableInLambdaDomainError
undefinedTypeVariableInLambdaDomainErrorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInLambdaDomainError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInLambdaDomainError"),
              Core.projectionField = (Core.Name "location")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

undefinedTypeVariableInTypeApplicationError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.UndefinedTypeVariableInTypeApplicationError
undefinedTypeVariableInTypeApplicationError location name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInTypeApplicationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

undefinedTypeVariableInTypeApplicationErrorLocation :: Phantoms.TTerm Core_.UndefinedTypeVariableInTypeApplicationError -> Phantoms.TTerm Paths.SubtermPath
undefinedTypeVariableInTypeApplicationErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInTypeApplicationError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

undefinedTypeVariableInTypeApplicationErrorName :: Phantoms.TTerm Core_.UndefinedTypeVariableInTypeApplicationError -> Phantoms.TTerm Core.Name
undefinedTypeVariableInTypeApplicationErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInTypeApplicationError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

undefinedTypeVariableInTypeApplicationErrorWithLocation :: Phantoms.TTerm Core_.UndefinedTypeVariableInTypeApplicationError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.UndefinedTypeVariableInTypeApplicationError
undefinedTypeVariableInTypeApplicationErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInTypeApplicationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInTypeApplicationError"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

undefinedTypeVariableInTypeApplicationErrorWithName :: Phantoms.TTerm Core_.UndefinedTypeVariableInTypeApplicationError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.UndefinedTypeVariableInTypeApplicationError
undefinedTypeVariableInTypeApplicationErrorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInTypeApplicationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInTypeApplicationError"),
              Core.projectionField = (Core.Name "location")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
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

unexpectedTermVariantErrorActualTerm :: Phantoms.TTerm Core_.UnexpectedTermVariantError -> Phantoms.TTerm Core.Term
unexpectedTermVariantErrorActualTerm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UnexpectedTermVariantError"),
        Core.projectionField = (Core.Name "actualTerm")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unexpectedTermVariantErrorExpectedVariant :: Phantoms.TTerm Core_.UnexpectedTermVariantError -> Phantoms.TTerm Variants.TermVariant
unexpectedTermVariantErrorExpectedVariant x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UnexpectedTermVariantError"),
        Core.projectionField = (Core.Name "expectedVariant")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unexpectedTermVariantErrorWithActualTerm :: Phantoms.TTerm Core_.UnexpectedTermVariantError -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Core_.UnexpectedTermVariantError
unexpectedTermVariantErrorWithActualTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UnexpectedTermVariantError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expectedVariant"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UnexpectedTermVariantError"),
              Core.projectionField = (Core.Name "expectedVariant")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "actualTerm"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

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
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UnexpectedTermVariantError"),
              Core.projectionField = (Core.Name "actualTerm")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

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

unexpectedTypeVariantErrorActualType :: Phantoms.TTerm Core_.UnexpectedTypeVariantError -> Phantoms.TTerm Core.Type
unexpectedTypeVariantErrorActualType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UnexpectedTypeVariantError"),
        Core.projectionField = (Core.Name "actualType")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unexpectedTypeVariantErrorExpectedVariant :: Phantoms.TTerm Core_.UnexpectedTypeVariantError -> Phantoms.TTerm Variants.TypeVariant
unexpectedTypeVariantErrorExpectedVariant x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UnexpectedTypeVariantError"),
        Core.projectionField = (Core.Name "expectedVariant")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unexpectedTypeVariantErrorWithActualType :: Phantoms.TTerm Core_.UnexpectedTypeVariantError -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Core_.UnexpectedTypeVariantError
unexpectedTypeVariantErrorWithActualType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UnexpectedTypeVariantError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expectedVariant"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UnexpectedTypeVariantError"),
              Core.projectionField = (Core.Name "expectedVariant")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "actualType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

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
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UnexpectedTypeVariantError"),
              Core.projectionField = (Core.Name "actualType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unknownPrimitiveNameError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.UnknownPrimitiveNameError
unknownPrimitiveNameError location name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UnknownPrimitiveNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

unknownPrimitiveNameErrorLocation :: Phantoms.TTerm Core_.UnknownPrimitiveNameError -> Phantoms.TTerm Paths.SubtermPath
unknownPrimitiveNameErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UnknownPrimitiveNameError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unknownPrimitiveNameErrorName :: Phantoms.TTerm Core_.UnknownPrimitiveNameError -> Phantoms.TTerm Core.Name
unknownPrimitiveNameErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UnknownPrimitiveNameError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unknownPrimitiveNameErrorWithLocation :: Phantoms.TTerm Core_.UnknownPrimitiveNameError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.UnknownPrimitiveNameError
unknownPrimitiveNameErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UnknownPrimitiveNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UnknownPrimitiveNameError"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unknownPrimitiveNameErrorWithName :: Phantoms.TTerm Core_.UnknownPrimitiveNameError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.UnknownPrimitiveNameError
unknownPrimitiveNameErrorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UnknownPrimitiveNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UnknownPrimitiveNameError"),
              Core.projectionField = (Core.Name "location")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

unnecessaryIdentityApplicationError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.UnnecessaryIdentityApplicationError
unnecessaryIdentityApplicationError location =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UnnecessaryIdentityApplicationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)}]}))

unnecessaryIdentityApplicationErrorLocation :: Phantoms.TTerm Core_.UnnecessaryIdentityApplicationError -> Phantoms.TTerm Paths.SubtermPath
unnecessaryIdentityApplicationErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UnnecessaryIdentityApplicationError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unnecessaryIdentityApplicationErrorWithLocation :: Phantoms.TTerm Core_.UnnecessaryIdentityApplicationError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.UnnecessaryIdentityApplicationError
unnecessaryIdentityApplicationErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UnnecessaryIdentityApplicationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

untypedTermVariableError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.UntypedTermVariableError
untypedTermVariableError location name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UntypedTermVariableError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

untypedTermVariableErrorLocation :: Phantoms.TTerm Core_.UntypedTermVariableError -> Phantoms.TTerm Paths.SubtermPath
untypedTermVariableErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UntypedTermVariableError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

untypedTermVariableErrorName :: Phantoms.TTerm Core_.UntypedTermVariableError -> Phantoms.TTerm Core.Name
untypedTermVariableErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UntypedTermVariableError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

untypedTermVariableErrorWithLocation :: Phantoms.TTerm Core_.UntypedTermVariableError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.UntypedTermVariableError
untypedTermVariableErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UntypedTermVariableError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UntypedTermVariableError"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

untypedTermVariableErrorWithName :: Phantoms.TTerm Core_.UntypedTermVariableError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core_.UntypedTermVariableError
untypedTermVariableErrorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UntypedTermVariableError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UntypedTermVariableError"),
              Core.projectionField = (Core.Name "location")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

voidInNonBottomPositionError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.VoidInNonBottomPositionError
voidInNonBottomPositionError location =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.VoidInNonBottomPositionError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)}]}))

voidInNonBottomPositionErrorLocation :: Phantoms.TTerm Core_.VoidInNonBottomPositionError -> Phantoms.TTerm Paths.SubtermPath
voidInNonBottomPositionErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.VoidInNonBottomPositionError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

voidInNonBottomPositionErrorWithLocation :: Phantoms.TTerm Core_.VoidInNonBottomPositionError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core_.VoidInNonBottomPositionError
voidInNonBottomPositionErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.VoidInNonBottomPositionError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
