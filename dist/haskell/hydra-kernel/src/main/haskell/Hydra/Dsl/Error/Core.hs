-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.error.core

module Hydra.Dsl.Error.Core where
import qualified Hydra.Core as Core
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Paths as Paths
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | DSL constructor for hydra.error.core.ConstantConditionError
constantConditionError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Bool -> Phantoms.TTerm ErrorCore.ConstantConditionError
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
-- | DSL accessor for the location field of hydra.error.core.ConstantConditionError
constantConditionErrorLocation :: Phantoms.TTerm ErrorCore.ConstantConditionError -> Phantoms.TTerm Paths.SubtermPath
constantConditionErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.ConstantConditionError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the value field of hydra.error.core.ConstantConditionError
constantConditionErrorValue :: Phantoms.TTerm ErrorCore.ConstantConditionError -> Phantoms.TTerm Bool
constantConditionErrorValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.ConstantConditionError"),
        Core.projectionField = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the location field of hydra.error.core.ConstantConditionError
constantConditionErrorWithLocation :: Phantoms.TTerm ErrorCore.ConstantConditionError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.ConstantConditionError
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
-- | DSL updater for the value field of hydra.error.core.ConstantConditionError
constantConditionErrorWithValue :: Phantoms.TTerm ErrorCore.ConstantConditionError -> Phantoms.TTerm Bool -> Phantoms.TTerm ErrorCore.ConstantConditionError
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
-- | DSL constructor for hydra.error.core.DuplicateBindingError
duplicateBindingError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.DuplicateBindingError
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
-- | DSL accessor for the location field of hydra.error.core.DuplicateBindingError
duplicateBindingErrorLocation :: Phantoms.TTerm ErrorCore.DuplicateBindingError -> Phantoms.TTerm Paths.SubtermPath
duplicateBindingErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateBindingError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.error.core.DuplicateBindingError
duplicateBindingErrorName :: Phantoms.TTerm ErrorCore.DuplicateBindingError -> Phantoms.TTerm Core.Name
duplicateBindingErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateBindingError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the location field of hydra.error.core.DuplicateBindingError
duplicateBindingErrorWithLocation :: Phantoms.TTerm ErrorCore.DuplicateBindingError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.DuplicateBindingError
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
-- | DSL updater for the name field of hydra.error.core.DuplicateBindingError
duplicateBindingErrorWithName :: Phantoms.TTerm ErrorCore.DuplicateBindingError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.DuplicateBindingError
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
-- | DSL constructor for hydra.error.core.DuplicateFieldError
duplicateFieldError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.DuplicateFieldError
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
-- | DSL accessor for the location field of hydra.error.core.DuplicateFieldError
duplicateFieldErrorLocation :: Phantoms.TTerm ErrorCore.DuplicateFieldError -> Phantoms.TTerm Paths.SubtermPath
duplicateFieldErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateFieldError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.error.core.DuplicateFieldError
duplicateFieldErrorName :: Phantoms.TTerm ErrorCore.DuplicateFieldError -> Phantoms.TTerm Core.Name
duplicateFieldErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateFieldError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the location field of hydra.error.core.DuplicateFieldError
duplicateFieldErrorWithLocation :: Phantoms.TTerm ErrorCore.DuplicateFieldError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.DuplicateFieldError
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
-- | DSL updater for the name field of hydra.error.core.DuplicateFieldError
duplicateFieldErrorWithName :: Phantoms.TTerm ErrorCore.DuplicateFieldError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.DuplicateFieldError
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
-- | DSL constructor for hydra.error.core.DuplicateRecordTypeFieldNamesError
duplicateRecordTypeFieldNamesError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.DuplicateRecordTypeFieldNamesError
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
-- | DSL accessor for the location field of hydra.error.core.DuplicateRecordTypeFieldNamesError
duplicateRecordTypeFieldNamesErrorLocation :: Phantoms.TTerm ErrorCore.DuplicateRecordTypeFieldNamesError -> Phantoms.TTerm Paths.SubtermPath
duplicateRecordTypeFieldNamesErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateRecordTypeFieldNamesError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.error.core.DuplicateRecordTypeFieldNamesError
duplicateRecordTypeFieldNamesErrorName :: Phantoms.TTerm ErrorCore.DuplicateRecordTypeFieldNamesError -> Phantoms.TTerm Core.Name
duplicateRecordTypeFieldNamesErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateRecordTypeFieldNamesError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the location field of hydra.error.core.DuplicateRecordTypeFieldNamesError
duplicateRecordTypeFieldNamesErrorWithLocation :: Phantoms.TTerm ErrorCore.DuplicateRecordTypeFieldNamesError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.DuplicateRecordTypeFieldNamesError
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
-- | DSL updater for the name field of hydra.error.core.DuplicateRecordTypeFieldNamesError
duplicateRecordTypeFieldNamesErrorWithName :: Phantoms.TTerm ErrorCore.DuplicateRecordTypeFieldNamesError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.DuplicateRecordTypeFieldNamesError
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
-- | DSL constructor for hydra.error.core.DuplicateUnionTypeFieldNamesError
duplicateUnionTypeFieldNamesError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.DuplicateUnionTypeFieldNamesError
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
-- | DSL accessor for the location field of hydra.error.core.DuplicateUnionTypeFieldNamesError
duplicateUnionTypeFieldNamesErrorLocation :: Phantoms.TTerm ErrorCore.DuplicateUnionTypeFieldNamesError -> Phantoms.TTerm Paths.SubtermPath
duplicateUnionTypeFieldNamesErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateUnionTypeFieldNamesError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.error.core.DuplicateUnionTypeFieldNamesError
duplicateUnionTypeFieldNamesErrorName :: Phantoms.TTerm ErrorCore.DuplicateUnionTypeFieldNamesError -> Phantoms.TTerm Core.Name
duplicateUnionTypeFieldNamesErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateUnionTypeFieldNamesError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the location field of hydra.error.core.DuplicateUnionTypeFieldNamesError
duplicateUnionTypeFieldNamesErrorWithLocation :: Phantoms.TTerm ErrorCore.DuplicateUnionTypeFieldNamesError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.DuplicateUnionTypeFieldNamesError
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
-- | DSL updater for the name field of hydra.error.core.DuplicateUnionTypeFieldNamesError
duplicateUnionTypeFieldNamesErrorWithName :: Phantoms.TTerm ErrorCore.DuplicateUnionTypeFieldNamesError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.DuplicateUnionTypeFieldNamesError
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
-- | DSL constructor for hydra.error.core.EmptyCaseStatementError
emptyCaseStatementError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.EmptyCaseStatementError
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
-- | DSL accessor for the location field of hydra.error.core.EmptyCaseStatementError
emptyCaseStatementErrorLocation :: Phantoms.TTerm ErrorCore.EmptyCaseStatementError -> Phantoms.TTerm Paths.SubtermPath
emptyCaseStatementErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.EmptyCaseStatementError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typeName field of hydra.error.core.EmptyCaseStatementError
emptyCaseStatementErrorTypeName :: Phantoms.TTerm ErrorCore.EmptyCaseStatementError -> Phantoms.TTerm Core.Name
emptyCaseStatementErrorTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.EmptyCaseStatementError"),
        Core.projectionField = (Core.Name "typeName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the location field of hydra.error.core.EmptyCaseStatementError
emptyCaseStatementErrorWithLocation :: Phantoms.TTerm ErrorCore.EmptyCaseStatementError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.EmptyCaseStatementError
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
-- | DSL updater for the typeName field of hydra.error.core.EmptyCaseStatementError
emptyCaseStatementErrorWithTypeName :: Phantoms.TTerm ErrorCore.EmptyCaseStatementError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.EmptyCaseStatementError
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
-- | DSL constructor for hydra.error.core.EmptyLetBindingsError
emptyLetBindingsError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.EmptyLetBindingsError
emptyLetBindingsError location =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyLetBindingsError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)}]}))
-- | DSL accessor for the location field of hydra.error.core.EmptyLetBindingsError
emptyLetBindingsErrorLocation :: Phantoms.TTerm ErrorCore.EmptyLetBindingsError -> Phantoms.TTerm Paths.SubtermPath
emptyLetBindingsErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.EmptyLetBindingsError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the location field of hydra.error.core.EmptyLetBindingsError
emptyLetBindingsErrorWithLocation :: Phantoms.TTerm ErrorCore.EmptyLetBindingsError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.EmptyLetBindingsError
emptyLetBindingsErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyLetBindingsError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.error.core.EmptyRecordTypeError
emptyRecordTypeError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.EmptyRecordTypeError
emptyRecordTypeError location =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyRecordTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)}]}))
-- | DSL accessor for the location field of hydra.error.core.EmptyRecordTypeError
emptyRecordTypeErrorLocation :: Phantoms.TTerm ErrorCore.EmptyRecordTypeError -> Phantoms.TTerm Paths.SubtermPath
emptyRecordTypeErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.EmptyRecordTypeError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the location field of hydra.error.core.EmptyRecordTypeError
emptyRecordTypeErrorWithLocation :: Phantoms.TTerm ErrorCore.EmptyRecordTypeError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.EmptyRecordTypeError
emptyRecordTypeErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyRecordTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.error.core.EmptyTermAnnotationError
emptyTermAnnotationError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.EmptyTermAnnotationError
emptyTermAnnotationError location =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyTermAnnotationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)}]}))
-- | DSL accessor for the location field of hydra.error.core.EmptyTermAnnotationError
emptyTermAnnotationErrorLocation :: Phantoms.TTerm ErrorCore.EmptyTermAnnotationError -> Phantoms.TTerm Paths.SubtermPath
emptyTermAnnotationErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.EmptyTermAnnotationError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the location field of hydra.error.core.EmptyTermAnnotationError
emptyTermAnnotationErrorWithLocation :: Phantoms.TTerm ErrorCore.EmptyTermAnnotationError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.EmptyTermAnnotationError
emptyTermAnnotationErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyTermAnnotationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.error.core.EmptyTypeAnnotationError
emptyTypeAnnotationError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.EmptyTypeAnnotationError
emptyTypeAnnotationError location =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyTypeAnnotationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)}]}))
-- | DSL accessor for the location field of hydra.error.core.EmptyTypeAnnotationError
emptyTypeAnnotationErrorLocation :: Phantoms.TTerm ErrorCore.EmptyTypeAnnotationError -> Phantoms.TTerm Paths.SubtermPath
emptyTypeAnnotationErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.EmptyTypeAnnotationError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the location field of hydra.error.core.EmptyTypeAnnotationError
emptyTypeAnnotationErrorWithLocation :: Phantoms.TTerm ErrorCore.EmptyTypeAnnotationError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.EmptyTypeAnnotationError
emptyTypeAnnotationErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyTypeAnnotationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.error.core.EmptyTypeNameInTermError
emptyTypeNameInTermError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.EmptyTypeNameInTermError
emptyTypeNameInTermError location =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyTypeNameInTermError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)}]}))
-- | DSL accessor for the location field of hydra.error.core.EmptyTypeNameInTermError
emptyTypeNameInTermErrorLocation :: Phantoms.TTerm ErrorCore.EmptyTypeNameInTermError -> Phantoms.TTerm Paths.SubtermPath
emptyTypeNameInTermErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.EmptyTypeNameInTermError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the location field of hydra.error.core.EmptyTypeNameInTermError
emptyTypeNameInTermErrorWithLocation :: Phantoms.TTerm ErrorCore.EmptyTypeNameInTermError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.EmptyTypeNameInTermError
emptyTypeNameInTermErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyTypeNameInTermError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.error.core.EmptyUnionTypeError
emptyUnionTypeError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.EmptyUnionTypeError
emptyUnionTypeError location =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyUnionTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)}]}))
-- | DSL accessor for the location field of hydra.error.core.EmptyUnionTypeError
emptyUnionTypeErrorLocation :: Phantoms.TTerm ErrorCore.EmptyUnionTypeError -> Phantoms.TTerm Paths.SubtermPath
emptyUnionTypeErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.EmptyUnionTypeError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the location field of hydra.error.core.EmptyUnionTypeError
emptyUnionTypeErrorWithLocation :: Phantoms.TTerm ErrorCore.EmptyUnionTypeError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.EmptyUnionTypeError
emptyUnionTypeErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyUnionTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.error.core.InvalidForallParameterNameError
invalidForallParameterNameError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.InvalidForallParameterNameError
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
-- | DSL accessor for the location field of hydra.error.core.InvalidForallParameterNameError
invalidForallParameterNameErrorLocation :: Phantoms.TTerm ErrorCore.InvalidForallParameterNameError -> Phantoms.TTerm Paths.SubtermPath
invalidForallParameterNameErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidForallParameterNameError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.error.core.InvalidForallParameterNameError
invalidForallParameterNameErrorName :: Phantoms.TTerm ErrorCore.InvalidForallParameterNameError -> Phantoms.TTerm Core.Name
invalidForallParameterNameErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidForallParameterNameError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the location field of hydra.error.core.InvalidForallParameterNameError
invalidForallParameterNameErrorWithLocation :: Phantoms.TTerm ErrorCore.InvalidForallParameterNameError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.InvalidForallParameterNameError
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
-- | DSL updater for the name field of hydra.error.core.InvalidForallParameterNameError
invalidForallParameterNameErrorWithName :: Phantoms.TTerm ErrorCore.InvalidForallParameterNameError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.InvalidForallParameterNameError
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
-- | DSL constructor for hydra.error.core.InvalidLambdaParameterNameError
invalidLambdaParameterNameError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.InvalidLambdaParameterNameError
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
-- | DSL accessor for the location field of hydra.error.core.InvalidLambdaParameterNameError
invalidLambdaParameterNameErrorLocation :: Phantoms.TTerm ErrorCore.InvalidLambdaParameterNameError -> Phantoms.TTerm Paths.SubtermPath
invalidLambdaParameterNameErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidLambdaParameterNameError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.error.core.InvalidLambdaParameterNameError
invalidLambdaParameterNameErrorName :: Phantoms.TTerm ErrorCore.InvalidLambdaParameterNameError -> Phantoms.TTerm Core.Name
invalidLambdaParameterNameErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidLambdaParameterNameError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the location field of hydra.error.core.InvalidLambdaParameterNameError
invalidLambdaParameterNameErrorWithLocation :: Phantoms.TTerm ErrorCore.InvalidLambdaParameterNameError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.InvalidLambdaParameterNameError
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
-- | DSL updater for the name field of hydra.error.core.InvalidLambdaParameterNameError
invalidLambdaParameterNameErrorWithName :: Phantoms.TTerm ErrorCore.InvalidLambdaParameterNameError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.InvalidLambdaParameterNameError
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
-- | DSL constructor for hydra.error.core.InvalidLetBindingNameError
invalidLetBindingNameError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.InvalidLetBindingNameError
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
-- | DSL accessor for the location field of hydra.error.core.InvalidLetBindingNameError
invalidLetBindingNameErrorLocation :: Phantoms.TTerm ErrorCore.InvalidLetBindingNameError -> Phantoms.TTerm Paths.SubtermPath
invalidLetBindingNameErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidLetBindingNameError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.error.core.InvalidLetBindingNameError
invalidLetBindingNameErrorName :: Phantoms.TTerm ErrorCore.InvalidLetBindingNameError -> Phantoms.TTerm Core.Name
invalidLetBindingNameErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidLetBindingNameError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the location field of hydra.error.core.InvalidLetBindingNameError
invalidLetBindingNameErrorWithLocation :: Phantoms.TTerm ErrorCore.InvalidLetBindingNameError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.InvalidLetBindingNameError
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
-- | DSL updater for the name field of hydra.error.core.InvalidLetBindingNameError
invalidLetBindingNameErrorWithName :: Phantoms.TTerm ErrorCore.InvalidLetBindingNameError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.InvalidLetBindingNameError
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
-- | DSL injection for the constantCondition variant of hydra.error.core.InvalidTermError
invalidTermErrorConstantCondition :: Phantoms.TTerm ErrorCore.ConstantConditionError -> Phantoms.TTerm ErrorCore.InvalidTermError
invalidTermErrorConstantCondition x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "constantCondition"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the duplicateBinding variant of hydra.error.core.InvalidTermError
invalidTermErrorDuplicateBinding :: Phantoms.TTerm ErrorCore.DuplicateBindingError -> Phantoms.TTerm ErrorCore.InvalidTermError
invalidTermErrorDuplicateBinding x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "duplicateBinding"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the duplicateField variant of hydra.error.core.InvalidTermError
invalidTermErrorDuplicateField :: Phantoms.TTerm ErrorCore.DuplicateFieldError -> Phantoms.TTerm ErrorCore.InvalidTermError
invalidTermErrorDuplicateField x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "duplicateField"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the emptyCaseStatement variant of hydra.error.core.InvalidTermError
invalidTermErrorEmptyCaseStatement :: Phantoms.TTerm ErrorCore.EmptyCaseStatementError -> Phantoms.TTerm ErrorCore.InvalidTermError
invalidTermErrorEmptyCaseStatement x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "emptyCaseStatement"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the emptyLetBindings variant of hydra.error.core.InvalidTermError
invalidTermErrorEmptyLetBindings :: Phantoms.TTerm ErrorCore.EmptyLetBindingsError -> Phantoms.TTerm ErrorCore.InvalidTermError
invalidTermErrorEmptyLetBindings x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "emptyLetBindings"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the emptyTermAnnotation variant of hydra.error.core.InvalidTermError
invalidTermErrorEmptyTermAnnotation :: Phantoms.TTerm ErrorCore.EmptyTermAnnotationError -> Phantoms.TTerm ErrorCore.InvalidTermError
invalidTermErrorEmptyTermAnnotation x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "emptyTermAnnotation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the emptyTypeNameInTerm variant of hydra.error.core.InvalidTermError
invalidTermErrorEmptyTypeNameInTerm :: Phantoms.TTerm ErrorCore.EmptyTypeNameInTermError -> Phantoms.TTerm ErrorCore.InvalidTermError
invalidTermErrorEmptyTypeNameInTerm x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "emptyTypeNameInTerm"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the invalidLambdaParameterName variant of hydra.error.core.InvalidTermError
invalidTermErrorInvalidLambdaParameterName :: Phantoms.TTerm ErrorCore.InvalidLambdaParameterNameError -> Phantoms.TTerm ErrorCore.InvalidTermError
invalidTermErrorInvalidLambdaParameterName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "invalidLambdaParameterName"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the invalidLetBindingName variant of hydra.error.core.InvalidTermError
invalidTermErrorInvalidLetBindingName :: Phantoms.TTerm ErrorCore.InvalidLetBindingNameError -> Phantoms.TTerm ErrorCore.InvalidTermError
invalidTermErrorInvalidLetBindingName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "invalidLetBindingName"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the invalidTypeLambdaParameterName variant of hydra.error.core.InvalidTermError
invalidTermErrorInvalidTypeLambdaParameterName :: Phantoms.TTerm ErrorCore.InvalidTypeLambdaParameterNameError -> Phantoms.TTerm ErrorCore.InvalidTermError
invalidTermErrorInvalidTypeLambdaParameterName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "invalidTypeLambdaParameterName"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the nestedTermAnnotation variant of hydra.error.core.InvalidTermError
invalidTermErrorNestedTermAnnotation :: Phantoms.TTerm ErrorCore.NestedTermAnnotationError -> Phantoms.TTerm ErrorCore.InvalidTermError
invalidTermErrorNestedTermAnnotation x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nestedTermAnnotation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the redundantWrapUnwrap variant of hydra.error.core.InvalidTermError
invalidTermErrorRedundantWrapUnwrap :: Phantoms.TTerm ErrorCore.RedundantWrapUnwrapError -> Phantoms.TTerm ErrorCore.InvalidTermError
invalidTermErrorRedundantWrapUnwrap x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "redundantWrapUnwrap"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the selfApplication variant of hydra.error.core.InvalidTermError
invalidTermErrorSelfApplication :: Phantoms.TTerm ErrorCore.SelfApplicationError -> Phantoms.TTerm ErrorCore.InvalidTermError
invalidTermErrorSelfApplication x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "selfApplication"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the termVariableShadowing variant of hydra.error.core.InvalidTermError
invalidTermErrorTermVariableShadowing :: Phantoms.TTerm ErrorCore.TermVariableShadowingError -> Phantoms.TTerm ErrorCore.InvalidTermError
invalidTermErrorTermVariableShadowing x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "termVariableShadowing"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the typeVariableShadowingInTypeLambda variant of hydra.error.core.InvalidTermError
invalidTermErrorTypeVariableShadowingInTypeLambda :: Phantoms.TTerm ErrorCore.TypeVariableShadowingInTypeLambdaError -> Phantoms.TTerm ErrorCore.InvalidTermError
invalidTermErrorTypeVariableShadowingInTypeLambda x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeVariableShadowingInTypeLambda"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the undefinedTermVariable variant of hydra.error.core.InvalidTermError
invalidTermErrorUndefinedTermVariable :: Phantoms.TTerm ErrorCore.UndefinedTermVariableError -> Phantoms.TTerm ErrorCore.InvalidTermError
invalidTermErrorUndefinedTermVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "undefinedTermVariable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the undefinedTypeVariableInBindingType variant of hydra.error.core.InvalidTermError
invalidTermErrorUndefinedTypeVariableInBindingType :: Phantoms.TTerm ErrorCore.UndefinedTypeVariableInBindingTypeError -> Phantoms.TTerm ErrorCore.InvalidTermError
invalidTermErrorUndefinedTypeVariableInBindingType x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "undefinedTypeVariableInBindingType"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the undefinedTypeVariableInLambdaDomain variant of hydra.error.core.InvalidTermError
invalidTermErrorUndefinedTypeVariableInLambdaDomain :: Phantoms.TTerm ErrorCore.UndefinedTypeVariableInLambdaDomainError -> Phantoms.TTerm ErrorCore.InvalidTermError
invalidTermErrorUndefinedTypeVariableInLambdaDomain x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "undefinedTypeVariableInLambdaDomain"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the undefinedTypeVariableInTypeApplication variant of hydra.error.core.InvalidTermError
invalidTermErrorUndefinedTypeVariableInTypeApplication :: Phantoms.TTerm ErrorCore.UndefinedTypeVariableInTypeApplicationError -> Phantoms.TTerm ErrorCore.InvalidTermError
invalidTermErrorUndefinedTypeVariableInTypeApplication x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "undefinedTypeVariableInTypeApplication"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the unknownPrimitiveName variant of hydra.error.core.InvalidTermError
invalidTermErrorUnknownPrimitiveName :: Phantoms.TTerm ErrorCore.UnknownPrimitiveNameError -> Phantoms.TTerm ErrorCore.InvalidTermError
invalidTermErrorUnknownPrimitiveName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unknownPrimitiveName"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the unnecessaryIdentityApplication variant of hydra.error.core.InvalidTermError
invalidTermErrorUnnecessaryIdentityApplication :: Phantoms.TTerm ErrorCore.UnnecessaryIdentityApplicationError -> Phantoms.TTerm ErrorCore.InvalidTermError
invalidTermErrorUnnecessaryIdentityApplication x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unnecessaryIdentityApplication"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the untypedTermVariable variant of hydra.error.core.InvalidTermError
invalidTermErrorUntypedTermVariable :: Phantoms.TTerm ErrorCore.UntypedTermVariableError -> Phantoms.TTerm ErrorCore.InvalidTermError
invalidTermErrorUntypedTermVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "untypedTermVariable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the duplicateRecordTypeFieldNames variant of hydra.error.core.InvalidTypeError
invalidTypeErrorDuplicateRecordTypeFieldNames :: Phantoms.TTerm ErrorCore.DuplicateRecordTypeFieldNamesError -> Phantoms.TTerm ErrorCore.InvalidTypeError
invalidTypeErrorDuplicateRecordTypeFieldNames x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "duplicateRecordTypeFieldNames"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the duplicateUnionTypeFieldNames variant of hydra.error.core.InvalidTypeError
invalidTypeErrorDuplicateUnionTypeFieldNames :: Phantoms.TTerm ErrorCore.DuplicateUnionTypeFieldNamesError -> Phantoms.TTerm ErrorCore.InvalidTypeError
invalidTypeErrorDuplicateUnionTypeFieldNames x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "duplicateUnionTypeFieldNames"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the emptyRecordType variant of hydra.error.core.InvalidTypeError
invalidTypeErrorEmptyRecordType :: Phantoms.TTerm ErrorCore.EmptyRecordTypeError -> Phantoms.TTerm ErrorCore.InvalidTypeError
invalidTypeErrorEmptyRecordType x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "emptyRecordType"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the emptyTypeAnnotation variant of hydra.error.core.InvalidTypeError
invalidTypeErrorEmptyTypeAnnotation :: Phantoms.TTerm ErrorCore.EmptyTypeAnnotationError -> Phantoms.TTerm ErrorCore.InvalidTypeError
invalidTypeErrorEmptyTypeAnnotation x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "emptyTypeAnnotation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the emptyUnionType variant of hydra.error.core.InvalidTypeError
invalidTypeErrorEmptyUnionType :: Phantoms.TTerm ErrorCore.EmptyUnionTypeError -> Phantoms.TTerm ErrorCore.InvalidTypeError
invalidTypeErrorEmptyUnionType x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "emptyUnionType"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the invalidForallParameterName variant of hydra.error.core.InvalidTypeError
invalidTypeErrorInvalidForallParameterName :: Phantoms.TTerm ErrorCore.InvalidForallParameterNameError -> Phantoms.TTerm ErrorCore.InvalidTypeError
invalidTypeErrorInvalidForallParameterName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "invalidForallParameterName"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the invalidTypeSchemeVariableName variant of hydra.error.core.InvalidTypeError
invalidTypeErrorInvalidTypeSchemeVariableName :: Phantoms.TTerm ErrorCore.InvalidTypeSchemeVariableNameError -> Phantoms.TTerm ErrorCore.InvalidTypeError
invalidTypeErrorInvalidTypeSchemeVariableName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "invalidTypeSchemeVariableName"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the nestedTypeAnnotation variant of hydra.error.core.InvalidTypeError
invalidTypeErrorNestedTypeAnnotation :: Phantoms.TTerm ErrorCore.NestedTypeAnnotationError -> Phantoms.TTerm ErrorCore.InvalidTypeError
invalidTypeErrorNestedTypeAnnotation x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nestedTypeAnnotation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the nonComparableMapKeyType variant of hydra.error.core.InvalidTypeError
invalidTypeErrorNonComparableMapKeyType :: Phantoms.TTerm ErrorCore.NonComparableMapKeyTypeError -> Phantoms.TTerm ErrorCore.InvalidTypeError
invalidTypeErrorNonComparableMapKeyType x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nonComparableMapKeyType"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the nonComparableSetElementType variant of hydra.error.core.InvalidTypeError
invalidTypeErrorNonComparableSetElementType :: Phantoms.TTerm ErrorCore.NonComparableSetElementTypeError -> Phantoms.TTerm ErrorCore.InvalidTypeError
invalidTypeErrorNonComparableSetElementType x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nonComparableSetElementType"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the singleVariantUnion variant of hydra.error.core.InvalidTypeError
invalidTypeErrorSingleVariantUnion :: Phantoms.TTerm ErrorCore.SingleVariantUnionError -> Phantoms.TTerm ErrorCore.InvalidTypeError
invalidTypeErrorSingleVariantUnion x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "singleVariantUnion"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the typeVariableShadowingInForall variant of hydra.error.core.InvalidTypeError
invalidTypeErrorTypeVariableShadowingInForall :: Phantoms.TTerm ErrorCore.TypeVariableShadowingInForallError -> Phantoms.TTerm ErrorCore.InvalidTypeError
invalidTypeErrorTypeVariableShadowingInForall x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeVariableShadowingInForall"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the undefinedTypeVariable variant of hydra.error.core.InvalidTypeError
invalidTypeErrorUndefinedTypeVariable :: Phantoms.TTerm ErrorCore.UndefinedTypeVariableError -> Phantoms.TTerm ErrorCore.InvalidTypeError
invalidTypeErrorUndefinedTypeVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "undefinedTypeVariable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the voidInNonBottomPosition variant of hydra.error.core.InvalidTypeError
invalidTypeErrorVoidInNonBottomPosition :: Phantoms.TTerm ErrorCore.VoidInNonBottomPositionError -> Phantoms.TTerm ErrorCore.InvalidTypeError
invalidTypeErrorVoidInNonBottomPosition x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "voidInNonBottomPosition"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.error.core.InvalidTypeLambdaParameterNameError
invalidTypeLambdaParameterNameError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.InvalidTypeLambdaParameterNameError
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
-- | DSL accessor for the location field of hydra.error.core.InvalidTypeLambdaParameterNameError
invalidTypeLambdaParameterNameErrorLocation :: Phantoms.TTerm ErrorCore.InvalidTypeLambdaParameterNameError -> Phantoms.TTerm Paths.SubtermPath
invalidTypeLambdaParameterNameErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidTypeLambdaParameterNameError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.error.core.InvalidTypeLambdaParameterNameError
invalidTypeLambdaParameterNameErrorName :: Phantoms.TTerm ErrorCore.InvalidTypeLambdaParameterNameError -> Phantoms.TTerm Core.Name
invalidTypeLambdaParameterNameErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidTypeLambdaParameterNameError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the location field of hydra.error.core.InvalidTypeLambdaParameterNameError
invalidTypeLambdaParameterNameErrorWithLocation :: Phantoms.TTerm ErrorCore.InvalidTypeLambdaParameterNameError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.InvalidTypeLambdaParameterNameError
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
-- | DSL updater for the name field of hydra.error.core.InvalidTypeLambdaParameterNameError
invalidTypeLambdaParameterNameErrorWithName :: Phantoms.TTerm ErrorCore.InvalidTypeLambdaParameterNameError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.InvalidTypeLambdaParameterNameError
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
-- | DSL constructor for hydra.error.core.InvalidTypeSchemeVariableNameError
invalidTypeSchemeVariableNameError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.InvalidTypeSchemeVariableNameError
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
-- | DSL accessor for the location field of hydra.error.core.InvalidTypeSchemeVariableNameError
invalidTypeSchemeVariableNameErrorLocation :: Phantoms.TTerm ErrorCore.InvalidTypeSchemeVariableNameError -> Phantoms.TTerm Paths.SubtermPath
invalidTypeSchemeVariableNameErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidTypeSchemeVariableNameError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.error.core.InvalidTypeSchemeVariableNameError
invalidTypeSchemeVariableNameErrorName :: Phantoms.TTerm ErrorCore.InvalidTypeSchemeVariableNameError -> Phantoms.TTerm Core.Name
invalidTypeSchemeVariableNameErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidTypeSchemeVariableNameError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the location field of hydra.error.core.InvalidTypeSchemeVariableNameError
invalidTypeSchemeVariableNameErrorWithLocation :: Phantoms.TTerm ErrorCore.InvalidTypeSchemeVariableNameError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.InvalidTypeSchemeVariableNameError
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
-- | DSL updater for the name field of hydra.error.core.InvalidTypeSchemeVariableNameError
invalidTypeSchemeVariableNameErrorWithName :: Phantoms.TTerm ErrorCore.InvalidTypeSchemeVariableNameError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.InvalidTypeSchemeVariableNameError
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
-- | DSL constructor for hydra.error.core.NestedTermAnnotationError
nestedTermAnnotationError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.NestedTermAnnotationError
nestedTermAnnotationError location =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.NestedTermAnnotationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)}]}))
-- | DSL accessor for the location field of hydra.error.core.NestedTermAnnotationError
nestedTermAnnotationErrorLocation :: Phantoms.TTerm ErrorCore.NestedTermAnnotationError -> Phantoms.TTerm Paths.SubtermPath
nestedTermAnnotationErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.NestedTermAnnotationError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the location field of hydra.error.core.NestedTermAnnotationError
nestedTermAnnotationErrorWithLocation :: Phantoms.TTerm ErrorCore.NestedTermAnnotationError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.NestedTermAnnotationError
nestedTermAnnotationErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.NestedTermAnnotationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.error.core.NestedTypeAnnotationError
nestedTypeAnnotationError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.NestedTypeAnnotationError
nestedTypeAnnotationError location =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.NestedTypeAnnotationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)}]}))
-- | DSL accessor for the location field of hydra.error.core.NestedTypeAnnotationError
nestedTypeAnnotationErrorLocation :: Phantoms.TTerm ErrorCore.NestedTypeAnnotationError -> Phantoms.TTerm Paths.SubtermPath
nestedTypeAnnotationErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.NestedTypeAnnotationError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the location field of hydra.error.core.NestedTypeAnnotationError
nestedTypeAnnotationErrorWithLocation :: Phantoms.TTerm ErrorCore.NestedTypeAnnotationError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.NestedTypeAnnotationError
nestedTypeAnnotationErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.NestedTypeAnnotationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.error.core.NonComparableMapKeyTypeError
nonComparableMapKeyTypeError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Type -> Phantoms.TTerm ErrorCore.NonComparableMapKeyTypeError
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
-- | DSL accessor for the keyType field of hydra.error.core.NonComparableMapKeyTypeError
nonComparableMapKeyTypeErrorKeyType :: Phantoms.TTerm ErrorCore.NonComparableMapKeyTypeError -> Phantoms.TTerm Core.Type
nonComparableMapKeyTypeErrorKeyType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.NonComparableMapKeyTypeError"),
        Core.projectionField = (Core.Name "keyType")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the location field of hydra.error.core.NonComparableMapKeyTypeError
nonComparableMapKeyTypeErrorLocation :: Phantoms.TTerm ErrorCore.NonComparableMapKeyTypeError -> Phantoms.TTerm Paths.SubtermPath
nonComparableMapKeyTypeErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.NonComparableMapKeyTypeError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the keyType field of hydra.error.core.NonComparableMapKeyTypeError
nonComparableMapKeyTypeErrorWithKeyType :: Phantoms.TTerm ErrorCore.NonComparableMapKeyTypeError -> Phantoms.TTerm Core.Type -> Phantoms.TTerm ErrorCore.NonComparableMapKeyTypeError
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
-- | DSL updater for the location field of hydra.error.core.NonComparableMapKeyTypeError
nonComparableMapKeyTypeErrorWithLocation :: Phantoms.TTerm ErrorCore.NonComparableMapKeyTypeError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.NonComparableMapKeyTypeError
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
-- | DSL constructor for hydra.error.core.NonComparableSetElementTypeError
nonComparableSetElementTypeError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Type -> Phantoms.TTerm ErrorCore.NonComparableSetElementTypeError
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
-- | DSL accessor for the elementType field of hydra.error.core.NonComparableSetElementTypeError
nonComparableSetElementTypeErrorElementType :: Phantoms.TTerm ErrorCore.NonComparableSetElementTypeError -> Phantoms.TTerm Core.Type
nonComparableSetElementTypeErrorElementType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.NonComparableSetElementTypeError"),
        Core.projectionField = (Core.Name "elementType")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the location field of hydra.error.core.NonComparableSetElementTypeError
nonComparableSetElementTypeErrorLocation :: Phantoms.TTerm ErrorCore.NonComparableSetElementTypeError -> Phantoms.TTerm Paths.SubtermPath
nonComparableSetElementTypeErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.NonComparableSetElementTypeError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the elementType field of hydra.error.core.NonComparableSetElementTypeError
nonComparableSetElementTypeErrorWithElementType :: Phantoms.TTerm ErrorCore.NonComparableSetElementTypeError -> Phantoms.TTerm Core.Type -> Phantoms.TTerm ErrorCore.NonComparableSetElementTypeError
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
-- | DSL updater for the location field of hydra.error.core.NonComparableSetElementTypeError
nonComparableSetElementTypeErrorWithLocation :: Phantoms.TTerm ErrorCore.NonComparableSetElementTypeError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.NonComparableSetElementTypeError
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
-- | DSL constructor for hydra.error.core.RedundantWrapUnwrapError
redundantWrapUnwrapError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.RedundantWrapUnwrapError
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
-- | DSL accessor for the location field of hydra.error.core.RedundantWrapUnwrapError
redundantWrapUnwrapErrorLocation :: Phantoms.TTerm ErrorCore.RedundantWrapUnwrapError -> Phantoms.TTerm Paths.SubtermPath
redundantWrapUnwrapErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.RedundantWrapUnwrapError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typeName field of hydra.error.core.RedundantWrapUnwrapError
redundantWrapUnwrapErrorTypeName :: Phantoms.TTerm ErrorCore.RedundantWrapUnwrapError -> Phantoms.TTerm Core.Name
redundantWrapUnwrapErrorTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.RedundantWrapUnwrapError"),
        Core.projectionField = (Core.Name "typeName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the location field of hydra.error.core.RedundantWrapUnwrapError
redundantWrapUnwrapErrorWithLocation :: Phantoms.TTerm ErrorCore.RedundantWrapUnwrapError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.RedundantWrapUnwrapError
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
-- | DSL updater for the typeName field of hydra.error.core.RedundantWrapUnwrapError
redundantWrapUnwrapErrorWithTypeName :: Phantoms.TTerm ErrorCore.RedundantWrapUnwrapError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.RedundantWrapUnwrapError
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
-- | DSL constructor for hydra.error.core.SelfApplicationError
selfApplicationError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.SelfApplicationError
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
-- | DSL accessor for the location field of hydra.error.core.SelfApplicationError
selfApplicationErrorLocation :: Phantoms.TTerm ErrorCore.SelfApplicationError -> Phantoms.TTerm Paths.SubtermPath
selfApplicationErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.SelfApplicationError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.error.core.SelfApplicationError
selfApplicationErrorName :: Phantoms.TTerm ErrorCore.SelfApplicationError -> Phantoms.TTerm Core.Name
selfApplicationErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.SelfApplicationError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the location field of hydra.error.core.SelfApplicationError
selfApplicationErrorWithLocation :: Phantoms.TTerm ErrorCore.SelfApplicationError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.SelfApplicationError
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
-- | DSL updater for the name field of hydra.error.core.SelfApplicationError
selfApplicationErrorWithName :: Phantoms.TTerm ErrorCore.SelfApplicationError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.SelfApplicationError
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
-- | DSL constructor for hydra.error.core.SingleVariantUnionError
singleVariantUnionError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.SingleVariantUnionError
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
-- | DSL accessor for the fieldName field of hydra.error.core.SingleVariantUnionError
singleVariantUnionErrorFieldName :: Phantoms.TTerm ErrorCore.SingleVariantUnionError -> Phantoms.TTerm Core.Name
singleVariantUnionErrorFieldName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.SingleVariantUnionError"),
        Core.projectionField = (Core.Name "fieldName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the location field of hydra.error.core.SingleVariantUnionError
singleVariantUnionErrorLocation :: Phantoms.TTerm ErrorCore.SingleVariantUnionError -> Phantoms.TTerm Paths.SubtermPath
singleVariantUnionErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.SingleVariantUnionError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the fieldName field of hydra.error.core.SingleVariantUnionError
singleVariantUnionErrorWithFieldName :: Phantoms.TTerm ErrorCore.SingleVariantUnionError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.SingleVariantUnionError
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
-- | DSL updater for the location field of hydra.error.core.SingleVariantUnionError
singleVariantUnionErrorWithLocation :: Phantoms.TTerm ErrorCore.SingleVariantUnionError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.SingleVariantUnionError
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
-- | DSL constructor for hydra.error.core.TermVariableShadowingError
termVariableShadowingError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.TermVariableShadowingError
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
-- | DSL accessor for the location field of hydra.error.core.TermVariableShadowingError
termVariableShadowingErrorLocation :: Phantoms.TTerm ErrorCore.TermVariableShadowingError -> Phantoms.TTerm Paths.SubtermPath
termVariableShadowingErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.TermVariableShadowingError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.error.core.TermVariableShadowingError
termVariableShadowingErrorName :: Phantoms.TTerm ErrorCore.TermVariableShadowingError -> Phantoms.TTerm Core.Name
termVariableShadowingErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.TermVariableShadowingError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the location field of hydra.error.core.TermVariableShadowingError
termVariableShadowingErrorWithLocation :: Phantoms.TTerm ErrorCore.TermVariableShadowingError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.TermVariableShadowingError
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
-- | DSL updater for the name field of hydra.error.core.TermVariableShadowingError
termVariableShadowingErrorWithName :: Phantoms.TTerm ErrorCore.TermVariableShadowingError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.TermVariableShadowingError
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
-- | DSL constructor for hydra.error.core.TypeVariableShadowingInForallError
typeVariableShadowingInForallError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.TypeVariableShadowingInForallError
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
-- | DSL accessor for the location field of hydra.error.core.TypeVariableShadowingInForallError
typeVariableShadowingInForallErrorLocation :: Phantoms.TTerm ErrorCore.TypeVariableShadowingInForallError -> Phantoms.TTerm Paths.SubtermPath
typeVariableShadowingInForallErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.TypeVariableShadowingInForallError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.error.core.TypeVariableShadowingInForallError
typeVariableShadowingInForallErrorName :: Phantoms.TTerm ErrorCore.TypeVariableShadowingInForallError -> Phantoms.TTerm Core.Name
typeVariableShadowingInForallErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.TypeVariableShadowingInForallError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the location field of hydra.error.core.TypeVariableShadowingInForallError
typeVariableShadowingInForallErrorWithLocation :: Phantoms.TTerm ErrorCore.TypeVariableShadowingInForallError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.TypeVariableShadowingInForallError
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
-- | DSL updater for the name field of hydra.error.core.TypeVariableShadowingInForallError
typeVariableShadowingInForallErrorWithName :: Phantoms.TTerm ErrorCore.TypeVariableShadowingInForallError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.TypeVariableShadowingInForallError
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
-- | DSL constructor for hydra.error.core.TypeVariableShadowingInTypeLambdaError
typeVariableShadowingInTypeLambdaError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.TypeVariableShadowingInTypeLambdaError
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
-- | DSL accessor for the location field of hydra.error.core.TypeVariableShadowingInTypeLambdaError
typeVariableShadowingInTypeLambdaErrorLocation :: Phantoms.TTerm ErrorCore.TypeVariableShadowingInTypeLambdaError -> Phantoms.TTerm Paths.SubtermPath
typeVariableShadowingInTypeLambdaErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.TypeVariableShadowingInTypeLambdaError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.error.core.TypeVariableShadowingInTypeLambdaError
typeVariableShadowingInTypeLambdaErrorName :: Phantoms.TTerm ErrorCore.TypeVariableShadowingInTypeLambdaError -> Phantoms.TTerm Core.Name
typeVariableShadowingInTypeLambdaErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.TypeVariableShadowingInTypeLambdaError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the location field of hydra.error.core.TypeVariableShadowingInTypeLambdaError
typeVariableShadowingInTypeLambdaErrorWithLocation :: Phantoms.TTerm ErrorCore.TypeVariableShadowingInTypeLambdaError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.TypeVariableShadowingInTypeLambdaError
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
-- | DSL updater for the name field of hydra.error.core.TypeVariableShadowingInTypeLambdaError
typeVariableShadowingInTypeLambdaErrorWithName :: Phantoms.TTerm ErrorCore.TypeVariableShadowingInTypeLambdaError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.TypeVariableShadowingInTypeLambdaError
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
-- | DSL constructor for hydra.error.core.UndefinedFieldError
undefinedFieldError :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.UndefinedFieldError
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
-- | DSL accessor for the fieldName field of hydra.error.core.UndefinedFieldError
undefinedFieldErrorFieldName :: Phantoms.TTerm ErrorCore.UndefinedFieldError -> Phantoms.TTerm Core.Name
undefinedFieldErrorFieldName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedFieldError"),
        Core.projectionField = (Core.Name "fieldName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typeName field of hydra.error.core.UndefinedFieldError
undefinedFieldErrorTypeName :: Phantoms.TTerm ErrorCore.UndefinedFieldError -> Phantoms.TTerm Core.Name
undefinedFieldErrorTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedFieldError"),
        Core.projectionField = (Core.Name "typeName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the fieldName field of hydra.error.core.UndefinedFieldError
undefinedFieldErrorWithFieldName :: Phantoms.TTerm ErrorCore.UndefinedFieldError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.UndefinedFieldError
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
-- | DSL updater for the typeName field of hydra.error.core.UndefinedFieldError
undefinedFieldErrorWithTypeName :: Phantoms.TTerm ErrorCore.UndefinedFieldError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.UndefinedFieldError
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
-- | DSL constructor for hydra.error.core.UndefinedTermVariableError
undefinedTermVariableError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.UndefinedTermVariableError
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
-- | DSL accessor for the location field of hydra.error.core.UndefinedTermVariableError
undefinedTermVariableErrorLocation :: Phantoms.TTerm ErrorCore.UndefinedTermVariableError -> Phantoms.TTerm Paths.SubtermPath
undefinedTermVariableErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTermVariableError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.error.core.UndefinedTermVariableError
undefinedTermVariableErrorName :: Phantoms.TTerm ErrorCore.UndefinedTermVariableError -> Phantoms.TTerm Core.Name
undefinedTermVariableErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTermVariableError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the location field of hydra.error.core.UndefinedTermVariableError
undefinedTermVariableErrorWithLocation :: Phantoms.TTerm ErrorCore.UndefinedTermVariableError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.UndefinedTermVariableError
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
-- | DSL updater for the name field of hydra.error.core.UndefinedTermVariableError
undefinedTermVariableErrorWithName :: Phantoms.TTerm ErrorCore.UndefinedTermVariableError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.UndefinedTermVariableError
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
-- | DSL constructor for hydra.error.core.UndefinedTypeVariableError
undefinedTypeVariableError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.UndefinedTypeVariableError
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
-- | DSL accessor for the location field of hydra.error.core.UndefinedTypeVariableError
undefinedTypeVariableErrorLocation :: Phantoms.TTerm ErrorCore.UndefinedTypeVariableError -> Phantoms.TTerm Paths.SubtermPath
undefinedTypeVariableErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.error.core.UndefinedTypeVariableError
undefinedTypeVariableErrorName :: Phantoms.TTerm ErrorCore.UndefinedTypeVariableError -> Phantoms.TTerm Core.Name
undefinedTypeVariableErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the location field of hydra.error.core.UndefinedTypeVariableError
undefinedTypeVariableErrorWithLocation :: Phantoms.TTerm ErrorCore.UndefinedTypeVariableError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.UndefinedTypeVariableError
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
-- | DSL updater for the name field of hydra.error.core.UndefinedTypeVariableError
undefinedTypeVariableErrorWithName :: Phantoms.TTerm ErrorCore.UndefinedTypeVariableError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.UndefinedTypeVariableError
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
-- | DSL constructor for hydra.error.core.UndefinedTypeVariableInBindingTypeError
undefinedTypeVariableInBindingTypeError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.UndefinedTypeVariableInBindingTypeError
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
-- | DSL accessor for the location field of hydra.error.core.UndefinedTypeVariableInBindingTypeError
undefinedTypeVariableInBindingTypeErrorLocation :: Phantoms.TTerm ErrorCore.UndefinedTypeVariableInBindingTypeError -> Phantoms.TTerm Paths.SubtermPath
undefinedTypeVariableInBindingTypeErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInBindingTypeError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.error.core.UndefinedTypeVariableInBindingTypeError
undefinedTypeVariableInBindingTypeErrorName :: Phantoms.TTerm ErrorCore.UndefinedTypeVariableInBindingTypeError -> Phantoms.TTerm Core.Name
undefinedTypeVariableInBindingTypeErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInBindingTypeError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the location field of hydra.error.core.UndefinedTypeVariableInBindingTypeError
undefinedTypeVariableInBindingTypeErrorWithLocation :: Phantoms.TTerm ErrorCore.UndefinedTypeVariableInBindingTypeError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.UndefinedTypeVariableInBindingTypeError
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
-- | DSL updater for the name field of hydra.error.core.UndefinedTypeVariableInBindingTypeError
undefinedTypeVariableInBindingTypeErrorWithName :: Phantoms.TTerm ErrorCore.UndefinedTypeVariableInBindingTypeError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.UndefinedTypeVariableInBindingTypeError
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
-- | DSL constructor for hydra.error.core.UndefinedTypeVariableInLambdaDomainError
undefinedTypeVariableInLambdaDomainError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.UndefinedTypeVariableInLambdaDomainError
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
-- | DSL accessor for the location field of hydra.error.core.UndefinedTypeVariableInLambdaDomainError
undefinedTypeVariableInLambdaDomainErrorLocation :: Phantoms.TTerm ErrorCore.UndefinedTypeVariableInLambdaDomainError -> Phantoms.TTerm Paths.SubtermPath
undefinedTypeVariableInLambdaDomainErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInLambdaDomainError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.error.core.UndefinedTypeVariableInLambdaDomainError
undefinedTypeVariableInLambdaDomainErrorName :: Phantoms.TTerm ErrorCore.UndefinedTypeVariableInLambdaDomainError -> Phantoms.TTerm Core.Name
undefinedTypeVariableInLambdaDomainErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInLambdaDomainError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the location field of hydra.error.core.UndefinedTypeVariableInLambdaDomainError
undefinedTypeVariableInLambdaDomainErrorWithLocation :: Phantoms.TTerm ErrorCore.UndefinedTypeVariableInLambdaDomainError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.UndefinedTypeVariableInLambdaDomainError
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
-- | DSL updater for the name field of hydra.error.core.UndefinedTypeVariableInLambdaDomainError
undefinedTypeVariableInLambdaDomainErrorWithName :: Phantoms.TTerm ErrorCore.UndefinedTypeVariableInLambdaDomainError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.UndefinedTypeVariableInLambdaDomainError
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
-- | DSL constructor for hydra.error.core.UndefinedTypeVariableInTypeApplicationError
undefinedTypeVariableInTypeApplicationError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.UndefinedTypeVariableInTypeApplicationError
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
-- | DSL accessor for the location field of hydra.error.core.UndefinedTypeVariableInTypeApplicationError
undefinedTypeVariableInTypeApplicationErrorLocation :: Phantoms.TTerm ErrorCore.UndefinedTypeVariableInTypeApplicationError -> Phantoms.TTerm Paths.SubtermPath
undefinedTypeVariableInTypeApplicationErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInTypeApplicationError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.error.core.UndefinedTypeVariableInTypeApplicationError
undefinedTypeVariableInTypeApplicationErrorName :: Phantoms.TTerm ErrorCore.UndefinedTypeVariableInTypeApplicationError -> Phantoms.TTerm Core.Name
undefinedTypeVariableInTypeApplicationErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInTypeApplicationError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the location field of hydra.error.core.UndefinedTypeVariableInTypeApplicationError
undefinedTypeVariableInTypeApplicationErrorWithLocation :: Phantoms.TTerm ErrorCore.UndefinedTypeVariableInTypeApplicationError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.UndefinedTypeVariableInTypeApplicationError
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
-- | DSL updater for the name field of hydra.error.core.UndefinedTypeVariableInTypeApplicationError
undefinedTypeVariableInTypeApplicationErrorWithName :: Phantoms.TTerm ErrorCore.UndefinedTypeVariableInTypeApplicationError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.UndefinedTypeVariableInTypeApplicationError
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
-- | DSL constructor for hydra.error.core.UnexpectedTermVariantError
unexpectedTermVariantError :: Phantoms.TTerm Variants.TermVariant -> Phantoms.TTerm Core.Term -> Phantoms.TTerm ErrorCore.UnexpectedTermVariantError
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
-- | DSL accessor for the actualTerm field of hydra.error.core.UnexpectedTermVariantError
unexpectedTermVariantErrorActualTerm :: Phantoms.TTerm ErrorCore.UnexpectedTermVariantError -> Phantoms.TTerm Core.Term
unexpectedTermVariantErrorActualTerm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UnexpectedTermVariantError"),
        Core.projectionField = (Core.Name "actualTerm")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the expectedVariant field of hydra.error.core.UnexpectedTermVariantError
unexpectedTermVariantErrorExpectedVariant :: Phantoms.TTerm ErrorCore.UnexpectedTermVariantError -> Phantoms.TTerm Variants.TermVariant
unexpectedTermVariantErrorExpectedVariant x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UnexpectedTermVariantError"),
        Core.projectionField = (Core.Name "expectedVariant")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the actualTerm field of hydra.error.core.UnexpectedTermVariantError
unexpectedTermVariantErrorWithActualTerm :: Phantoms.TTerm ErrorCore.UnexpectedTermVariantError -> Phantoms.TTerm Core.Term -> Phantoms.TTerm ErrorCore.UnexpectedTermVariantError
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
-- | DSL updater for the expectedVariant field of hydra.error.core.UnexpectedTermVariantError
unexpectedTermVariantErrorWithExpectedVariant :: Phantoms.TTerm ErrorCore.UnexpectedTermVariantError -> Phantoms.TTerm Variants.TermVariant -> Phantoms.TTerm ErrorCore.UnexpectedTermVariantError
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
-- | DSL constructor for hydra.error.core.UnexpectedTypeVariantError
unexpectedTypeVariantError :: Phantoms.TTerm Variants.TypeVariant -> Phantoms.TTerm Core.Type -> Phantoms.TTerm ErrorCore.UnexpectedTypeVariantError
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
-- | DSL accessor for the actualType field of hydra.error.core.UnexpectedTypeVariantError
unexpectedTypeVariantErrorActualType :: Phantoms.TTerm ErrorCore.UnexpectedTypeVariantError -> Phantoms.TTerm Core.Type
unexpectedTypeVariantErrorActualType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UnexpectedTypeVariantError"),
        Core.projectionField = (Core.Name "actualType")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the expectedVariant field of hydra.error.core.UnexpectedTypeVariantError
unexpectedTypeVariantErrorExpectedVariant :: Phantoms.TTerm ErrorCore.UnexpectedTypeVariantError -> Phantoms.TTerm Variants.TypeVariant
unexpectedTypeVariantErrorExpectedVariant x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UnexpectedTypeVariantError"),
        Core.projectionField = (Core.Name "expectedVariant")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the actualType field of hydra.error.core.UnexpectedTypeVariantError
unexpectedTypeVariantErrorWithActualType :: Phantoms.TTerm ErrorCore.UnexpectedTypeVariantError -> Phantoms.TTerm Core.Type -> Phantoms.TTerm ErrorCore.UnexpectedTypeVariantError
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
-- | DSL updater for the expectedVariant field of hydra.error.core.UnexpectedTypeVariantError
unexpectedTypeVariantErrorWithExpectedVariant :: Phantoms.TTerm ErrorCore.UnexpectedTypeVariantError -> Phantoms.TTerm Variants.TypeVariant -> Phantoms.TTerm ErrorCore.UnexpectedTypeVariantError
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
-- | DSL constructor for hydra.error.core.UnknownPrimitiveNameError
unknownPrimitiveNameError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.UnknownPrimitiveNameError
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
-- | DSL accessor for the location field of hydra.error.core.UnknownPrimitiveNameError
unknownPrimitiveNameErrorLocation :: Phantoms.TTerm ErrorCore.UnknownPrimitiveNameError -> Phantoms.TTerm Paths.SubtermPath
unknownPrimitiveNameErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UnknownPrimitiveNameError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.error.core.UnknownPrimitiveNameError
unknownPrimitiveNameErrorName :: Phantoms.TTerm ErrorCore.UnknownPrimitiveNameError -> Phantoms.TTerm Core.Name
unknownPrimitiveNameErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UnknownPrimitiveNameError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the location field of hydra.error.core.UnknownPrimitiveNameError
unknownPrimitiveNameErrorWithLocation :: Phantoms.TTerm ErrorCore.UnknownPrimitiveNameError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.UnknownPrimitiveNameError
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
-- | DSL updater for the name field of hydra.error.core.UnknownPrimitiveNameError
unknownPrimitiveNameErrorWithName :: Phantoms.TTerm ErrorCore.UnknownPrimitiveNameError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.UnknownPrimitiveNameError
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
-- | DSL constructor for hydra.error.core.UnnecessaryIdentityApplicationError
unnecessaryIdentityApplicationError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.UnnecessaryIdentityApplicationError
unnecessaryIdentityApplicationError location =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UnnecessaryIdentityApplicationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)}]}))
-- | DSL accessor for the location field of hydra.error.core.UnnecessaryIdentityApplicationError
unnecessaryIdentityApplicationErrorLocation :: Phantoms.TTerm ErrorCore.UnnecessaryIdentityApplicationError -> Phantoms.TTerm Paths.SubtermPath
unnecessaryIdentityApplicationErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UnnecessaryIdentityApplicationError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the location field of hydra.error.core.UnnecessaryIdentityApplicationError
unnecessaryIdentityApplicationErrorWithLocation :: Phantoms.TTerm ErrorCore.UnnecessaryIdentityApplicationError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.UnnecessaryIdentityApplicationError
unnecessaryIdentityApplicationErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UnnecessaryIdentityApplicationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.error.core.UntypedTermVariableError
untypedTermVariableError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.UntypedTermVariableError
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
-- | DSL accessor for the location field of hydra.error.core.UntypedTermVariableError
untypedTermVariableErrorLocation :: Phantoms.TTerm ErrorCore.UntypedTermVariableError -> Phantoms.TTerm Paths.SubtermPath
untypedTermVariableErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UntypedTermVariableError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.error.core.UntypedTermVariableError
untypedTermVariableErrorName :: Phantoms.TTerm ErrorCore.UntypedTermVariableError -> Phantoms.TTerm Core.Name
untypedTermVariableErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UntypedTermVariableError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the location field of hydra.error.core.UntypedTermVariableError
untypedTermVariableErrorWithLocation :: Phantoms.TTerm ErrorCore.UntypedTermVariableError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.UntypedTermVariableError
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
-- | DSL updater for the name field of hydra.error.core.UntypedTermVariableError
untypedTermVariableErrorWithName :: Phantoms.TTerm ErrorCore.UntypedTermVariableError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorCore.UntypedTermVariableError
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
-- | DSL constructor for hydra.error.core.VoidInNonBottomPositionError
voidInNonBottomPositionError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.VoidInNonBottomPositionError
voidInNonBottomPositionError location =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.VoidInNonBottomPositionError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm location)}]}))
-- | DSL accessor for the location field of hydra.error.core.VoidInNonBottomPositionError
voidInNonBottomPositionErrorLocation :: Phantoms.TTerm ErrorCore.VoidInNonBottomPositionError -> Phantoms.TTerm Paths.SubtermPath
voidInNonBottomPositionErrorLocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.VoidInNonBottomPositionError"),
        Core.projectionField = (Core.Name "location")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the location field of hydra.error.core.VoidInNonBottomPositionError
voidInNonBottomPositionErrorWithLocation :: Phantoms.TTerm ErrorCore.VoidInNonBottomPositionError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm ErrorCore.VoidInNonBottomPositionError
voidInNonBottomPositionErrorWithLocation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.VoidInNonBottomPositionError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
