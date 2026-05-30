-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.error.core

module Hydra.Dsl.Error.Core where
import qualified Hydra.Core as Core
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Dsl.Paths as DslPaths
import qualified Hydra.Dsl.Variants as DslVariants
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Paths as Paths
import qualified Hydra.Typed as Typed
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | DSL constructor for hydra.error.core.ConstantConditionError
constantConditionError :: Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm Bool -> Typed.TypedTerm ErrorCore.ConstantConditionError
constantConditionError location value =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.ConstantConditionError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm value)}]}))
-- | DSL accessor for the location field of hydra.error.core.ConstantConditionError
constantConditionErrorLocation :: Typed.TypedTerm ErrorCore.ConstantConditionError -> Typed.TypedTerm Paths.SubtermPath
constantConditionErrorLocation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.ConstantConditionError"),
        Core.projectionFieldName = (Core.Name "location")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the value field of hydra.error.core.ConstantConditionError
constantConditionErrorValue :: Typed.TypedTerm ErrorCore.ConstantConditionError -> Typed.TypedTerm Bool
constantConditionErrorValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.ConstantConditionError"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the location field of hydra.error.core.ConstantConditionError
constantConditionErrorWithLocation :: Typed.TypedTerm ErrorCore.ConstantConditionError -> Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.ConstantConditionError
constantConditionErrorWithLocation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.ConstantConditionError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.ConstantConditionError"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the value field of hydra.error.core.ConstantConditionError
constantConditionErrorWithValue :: Typed.TypedTerm ErrorCore.ConstantConditionError -> Typed.TypedTerm Bool -> Typed.TypedTerm ErrorCore.ConstantConditionError
constantConditionErrorWithValue original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.ConstantConditionError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.ConstantConditionError"),
              Core.projectionFieldName = (Core.Name "location")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.error.core.DuplicateBindingError
duplicateBindingError :: Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.DuplicateBindingError
duplicateBindingError location name =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.DuplicateBindingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)}]}))
-- | DSL accessor for the location field of hydra.error.core.DuplicateBindingError
duplicateBindingErrorLocation :: Typed.TypedTerm ErrorCore.DuplicateBindingError -> Typed.TypedTerm Paths.SubtermPath
duplicateBindingErrorLocation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateBindingError"),
        Core.projectionFieldName = (Core.Name "location")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.error.core.DuplicateBindingError
duplicateBindingErrorName :: Typed.TypedTerm ErrorCore.DuplicateBindingError -> Typed.TypedTerm Core.Name
duplicateBindingErrorName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateBindingError"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the location field of hydra.error.core.DuplicateBindingError
duplicateBindingErrorWithLocation :: Typed.TypedTerm ErrorCore.DuplicateBindingError -> Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.DuplicateBindingError
duplicateBindingErrorWithLocation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.DuplicateBindingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateBindingError"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.error.core.DuplicateBindingError
duplicateBindingErrorWithName :: Typed.TypedTerm ErrorCore.DuplicateBindingError -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.DuplicateBindingError
duplicateBindingErrorWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.DuplicateBindingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateBindingError"),
              Core.projectionFieldName = (Core.Name "location")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.error.core.DuplicateFieldError
duplicateFieldError :: Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.DuplicateFieldError
duplicateFieldError location name =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.DuplicateFieldError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)}]}))
-- | DSL accessor for the location field of hydra.error.core.DuplicateFieldError
duplicateFieldErrorLocation :: Typed.TypedTerm ErrorCore.DuplicateFieldError -> Typed.TypedTerm Paths.SubtermPath
duplicateFieldErrorLocation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateFieldError"),
        Core.projectionFieldName = (Core.Name "location")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.error.core.DuplicateFieldError
duplicateFieldErrorName :: Typed.TypedTerm ErrorCore.DuplicateFieldError -> Typed.TypedTerm Core.Name
duplicateFieldErrorName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateFieldError"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the location field of hydra.error.core.DuplicateFieldError
duplicateFieldErrorWithLocation :: Typed.TypedTerm ErrorCore.DuplicateFieldError -> Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.DuplicateFieldError
duplicateFieldErrorWithLocation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.DuplicateFieldError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateFieldError"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.error.core.DuplicateFieldError
duplicateFieldErrorWithName :: Typed.TypedTerm ErrorCore.DuplicateFieldError -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.DuplicateFieldError
duplicateFieldErrorWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.DuplicateFieldError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateFieldError"),
              Core.projectionFieldName = (Core.Name "location")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.error.core.DuplicateRecordTypeFieldNamesError
duplicateRecordTypeFieldNamesError :: Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.DuplicateRecordTypeFieldNamesError
duplicateRecordTypeFieldNamesError location name =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.DuplicateRecordTypeFieldNamesError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)}]}))
-- | DSL accessor for the location field of hydra.error.core.DuplicateRecordTypeFieldNamesError
duplicateRecordTypeFieldNamesErrorLocation :: Typed.TypedTerm ErrorCore.DuplicateRecordTypeFieldNamesError -> Typed.TypedTerm Paths.SubtermPath
duplicateRecordTypeFieldNamesErrorLocation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateRecordTypeFieldNamesError"),
        Core.projectionFieldName = (Core.Name "location")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.error.core.DuplicateRecordTypeFieldNamesError
duplicateRecordTypeFieldNamesErrorName :: Typed.TypedTerm ErrorCore.DuplicateRecordTypeFieldNamesError -> Typed.TypedTerm Core.Name
duplicateRecordTypeFieldNamesErrorName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateRecordTypeFieldNamesError"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the location field of hydra.error.core.DuplicateRecordTypeFieldNamesError
duplicateRecordTypeFieldNamesErrorWithLocation :: Typed.TypedTerm ErrorCore.DuplicateRecordTypeFieldNamesError -> Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.DuplicateRecordTypeFieldNamesError
duplicateRecordTypeFieldNamesErrorWithLocation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.DuplicateRecordTypeFieldNamesError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateRecordTypeFieldNamesError"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.error.core.DuplicateRecordTypeFieldNamesError
duplicateRecordTypeFieldNamesErrorWithName :: Typed.TypedTerm ErrorCore.DuplicateRecordTypeFieldNamesError -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.DuplicateRecordTypeFieldNamesError
duplicateRecordTypeFieldNamesErrorWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.DuplicateRecordTypeFieldNamesError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateRecordTypeFieldNamesError"),
              Core.projectionFieldName = (Core.Name "location")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.error.core.DuplicateUnionTypeFieldNamesError
duplicateUnionTypeFieldNamesError :: Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.DuplicateUnionTypeFieldNamesError
duplicateUnionTypeFieldNamesError location name =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.DuplicateUnionTypeFieldNamesError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)}]}))
-- | DSL accessor for the location field of hydra.error.core.DuplicateUnionTypeFieldNamesError
duplicateUnionTypeFieldNamesErrorLocation :: Typed.TypedTerm ErrorCore.DuplicateUnionTypeFieldNamesError -> Typed.TypedTerm Paths.SubtermPath
duplicateUnionTypeFieldNamesErrorLocation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateUnionTypeFieldNamesError"),
        Core.projectionFieldName = (Core.Name "location")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.error.core.DuplicateUnionTypeFieldNamesError
duplicateUnionTypeFieldNamesErrorName :: Typed.TypedTerm ErrorCore.DuplicateUnionTypeFieldNamesError -> Typed.TypedTerm Core.Name
duplicateUnionTypeFieldNamesErrorName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateUnionTypeFieldNamesError"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the location field of hydra.error.core.DuplicateUnionTypeFieldNamesError
duplicateUnionTypeFieldNamesErrorWithLocation :: Typed.TypedTerm ErrorCore.DuplicateUnionTypeFieldNamesError -> Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.DuplicateUnionTypeFieldNamesError
duplicateUnionTypeFieldNamesErrorWithLocation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.DuplicateUnionTypeFieldNamesError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateUnionTypeFieldNamesError"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.error.core.DuplicateUnionTypeFieldNamesError
duplicateUnionTypeFieldNamesErrorWithName :: Typed.TypedTerm ErrorCore.DuplicateUnionTypeFieldNamesError -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.DuplicateUnionTypeFieldNamesError
duplicateUnionTypeFieldNamesErrorWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.DuplicateUnionTypeFieldNamesError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateUnionTypeFieldNamesError"),
              Core.projectionFieldName = (Core.Name "location")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.error.core.EmptyCaseStatementError
emptyCaseStatementError :: Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.EmptyCaseStatementError
emptyCaseStatementError location typeName =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyCaseStatementError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Typed.unTypedTerm typeName)}]}))
-- | DSL accessor for the location field of hydra.error.core.EmptyCaseStatementError
emptyCaseStatementErrorLocation :: Typed.TypedTerm ErrorCore.EmptyCaseStatementError -> Typed.TypedTerm Paths.SubtermPath
emptyCaseStatementErrorLocation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.EmptyCaseStatementError"),
        Core.projectionFieldName = (Core.Name "location")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeName field of hydra.error.core.EmptyCaseStatementError
emptyCaseStatementErrorTypeName :: Typed.TypedTerm ErrorCore.EmptyCaseStatementError -> Typed.TypedTerm Core.Name
emptyCaseStatementErrorTypeName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.EmptyCaseStatementError"),
        Core.projectionFieldName = (Core.Name "typeName")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the location field of hydra.error.core.EmptyCaseStatementError
emptyCaseStatementErrorWithLocation :: Typed.TypedTerm ErrorCore.EmptyCaseStatementError -> Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.EmptyCaseStatementError
emptyCaseStatementErrorWithLocation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyCaseStatementError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.EmptyCaseStatementError"),
              Core.projectionFieldName = (Core.Name "typeName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the typeName field of hydra.error.core.EmptyCaseStatementError
emptyCaseStatementErrorWithTypeName :: Typed.TypedTerm ErrorCore.EmptyCaseStatementError -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.EmptyCaseStatementError
emptyCaseStatementErrorWithTypeName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyCaseStatementError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.EmptyCaseStatementError"),
              Core.projectionFieldName = (Core.Name "location")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.error.core.EmptyLetBindingsError
emptyLetBindingsError :: Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.EmptyLetBindingsError
emptyLetBindingsError location =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyLetBindingsError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm location)}]}))
-- | DSL accessor for the location field of hydra.error.core.EmptyLetBindingsError
emptyLetBindingsErrorLocation :: Typed.TypedTerm ErrorCore.EmptyLetBindingsError -> Typed.TypedTerm Paths.SubtermPath
emptyLetBindingsErrorLocation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.EmptyLetBindingsError"),
        Core.projectionFieldName = (Core.Name "location")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the location field of hydra.error.core.EmptyLetBindingsError
emptyLetBindingsErrorWithLocation :: Typed.TypedTerm ErrorCore.EmptyLetBindingsError -> Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.EmptyLetBindingsError
emptyLetBindingsErrorWithLocation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyLetBindingsError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.error.core.EmptyRecordTypeError
emptyRecordTypeError :: Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.EmptyRecordTypeError
emptyRecordTypeError location =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyRecordTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm location)}]}))
-- | DSL accessor for the location field of hydra.error.core.EmptyRecordTypeError
emptyRecordTypeErrorLocation :: Typed.TypedTerm ErrorCore.EmptyRecordTypeError -> Typed.TypedTerm Paths.SubtermPath
emptyRecordTypeErrorLocation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.EmptyRecordTypeError"),
        Core.projectionFieldName = (Core.Name "location")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the location field of hydra.error.core.EmptyRecordTypeError
emptyRecordTypeErrorWithLocation :: Typed.TypedTerm ErrorCore.EmptyRecordTypeError -> Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.EmptyRecordTypeError
emptyRecordTypeErrorWithLocation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyRecordTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.error.core.EmptyTermAnnotationError
emptyTermAnnotationError :: Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.EmptyTermAnnotationError
emptyTermAnnotationError location =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyTermAnnotationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm location)}]}))
-- | DSL accessor for the location field of hydra.error.core.EmptyTermAnnotationError
emptyTermAnnotationErrorLocation :: Typed.TypedTerm ErrorCore.EmptyTermAnnotationError -> Typed.TypedTerm Paths.SubtermPath
emptyTermAnnotationErrorLocation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.EmptyTermAnnotationError"),
        Core.projectionFieldName = (Core.Name "location")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the location field of hydra.error.core.EmptyTermAnnotationError
emptyTermAnnotationErrorWithLocation :: Typed.TypedTerm ErrorCore.EmptyTermAnnotationError -> Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.EmptyTermAnnotationError
emptyTermAnnotationErrorWithLocation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyTermAnnotationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.error.core.EmptyTypeAnnotationError
emptyTypeAnnotationError :: Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.EmptyTypeAnnotationError
emptyTypeAnnotationError location =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyTypeAnnotationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm location)}]}))
-- | DSL accessor for the location field of hydra.error.core.EmptyTypeAnnotationError
emptyTypeAnnotationErrorLocation :: Typed.TypedTerm ErrorCore.EmptyTypeAnnotationError -> Typed.TypedTerm Paths.SubtermPath
emptyTypeAnnotationErrorLocation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.EmptyTypeAnnotationError"),
        Core.projectionFieldName = (Core.Name "location")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the location field of hydra.error.core.EmptyTypeAnnotationError
emptyTypeAnnotationErrorWithLocation :: Typed.TypedTerm ErrorCore.EmptyTypeAnnotationError -> Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.EmptyTypeAnnotationError
emptyTypeAnnotationErrorWithLocation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyTypeAnnotationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.error.core.EmptyTypeNameInTermError
emptyTypeNameInTermError :: Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.EmptyTypeNameInTermError
emptyTypeNameInTermError location =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyTypeNameInTermError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm location)}]}))
-- | DSL accessor for the location field of hydra.error.core.EmptyTypeNameInTermError
emptyTypeNameInTermErrorLocation :: Typed.TypedTerm ErrorCore.EmptyTypeNameInTermError -> Typed.TypedTerm Paths.SubtermPath
emptyTypeNameInTermErrorLocation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.EmptyTypeNameInTermError"),
        Core.projectionFieldName = (Core.Name "location")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the location field of hydra.error.core.EmptyTypeNameInTermError
emptyTypeNameInTermErrorWithLocation :: Typed.TypedTerm ErrorCore.EmptyTypeNameInTermError -> Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.EmptyTypeNameInTermError
emptyTypeNameInTermErrorWithLocation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyTypeNameInTermError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.error.core.EmptyUnionTypeError
emptyUnionTypeError :: Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.EmptyUnionTypeError
emptyUnionTypeError location =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyUnionTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm location)}]}))
-- | DSL accessor for the location field of hydra.error.core.EmptyUnionTypeError
emptyUnionTypeErrorLocation :: Typed.TypedTerm ErrorCore.EmptyUnionTypeError -> Typed.TypedTerm Paths.SubtermPath
emptyUnionTypeErrorLocation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.EmptyUnionTypeError"),
        Core.projectionFieldName = (Core.Name "location")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the location field of hydra.error.core.EmptyUnionTypeError
emptyUnionTypeErrorWithLocation :: Typed.TypedTerm ErrorCore.EmptyUnionTypeError -> Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.EmptyUnionTypeError
emptyUnionTypeErrorWithLocation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.EmptyUnionTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.error.core.InvalidForallParameterNameError
invalidForallParameterNameError :: Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.InvalidForallParameterNameError
invalidForallParameterNameError location name =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.InvalidForallParameterNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)}]}))
-- | DSL accessor for the location field of hydra.error.core.InvalidForallParameterNameError
invalidForallParameterNameErrorLocation :: Typed.TypedTerm ErrorCore.InvalidForallParameterNameError -> Typed.TypedTerm Paths.SubtermPath
invalidForallParameterNameErrorLocation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidForallParameterNameError"),
        Core.projectionFieldName = (Core.Name "location")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.error.core.InvalidForallParameterNameError
invalidForallParameterNameErrorName :: Typed.TypedTerm ErrorCore.InvalidForallParameterNameError -> Typed.TypedTerm Core.Name
invalidForallParameterNameErrorName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidForallParameterNameError"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the location field of hydra.error.core.InvalidForallParameterNameError
invalidForallParameterNameErrorWithLocation :: Typed.TypedTerm ErrorCore.InvalidForallParameterNameError -> Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.InvalidForallParameterNameError
invalidForallParameterNameErrorWithLocation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.InvalidForallParameterNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidForallParameterNameError"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.error.core.InvalidForallParameterNameError
invalidForallParameterNameErrorWithName :: Typed.TypedTerm ErrorCore.InvalidForallParameterNameError -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.InvalidForallParameterNameError
invalidForallParameterNameErrorWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.InvalidForallParameterNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidForallParameterNameError"),
              Core.projectionFieldName = (Core.Name "location")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.error.core.InvalidLambdaParameterNameError
invalidLambdaParameterNameError :: Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.InvalidLambdaParameterNameError
invalidLambdaParameterNameError location name =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.InvalidLambdaParameterNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)}]}))
-- | DSL accessor for the location field of hydra.error.core.InvalidLambdaParameterNameError
invalidLambdaParameterNameErrorLocation :: Typed.TypedTerm ErrorCore.InvalidLambdaParameterNameError -> Typed.TypedTerm Paths.SubtermPath
invalidLambdaParameterNameErrorLocation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidLambdaParameterNameError"),
        Core.projectionFieldName = (Core.Name "location")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.error.core.InvalidLambdaParameterNameError
invalidLambdaParameterNameErrorName :: Typed.TypedTerm ErrorCore.InvalidLambdaParameterNameError -> Typed.TypedTerm Core.Name
invalidLambdaParameterNameErrorName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidLambdaParameterNameError"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the location field of hydra.error.core.InvalidLambdaParameterNameError
invalidLambdaParameterNameErrorWithLocation :: Typed.TypedTerm ErrorCore.InvalidLambdaParameterNameError -> Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.InvalidLambdaParameterNameError
invalidLambdaParameterNameErrorWithLocation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.InvalidLambdaParameterNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidLambdaParameterNameError"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.error.core.InvalidLambdaParameterNameError
invalidLambdaParameterNameErrorWithName :: Typed.TypedTerm ErrorCore.InvalidLambdaParameterNameError -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.InvalidLambdaParameterNameError
invalidLambdaParameterNameErrorWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.InvalidLambdaParameterNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidLambdaParameterNameError"),
              Core.projectionFieldName = (Core.Name "location")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.error.core.InvalidLetBindingNameError
invalidLetBindingNameError :: Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.InvalidLetBindingNameError
invalidLetBindingNameError location name =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.InvalidLetBindingNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)}]}))
-- | DSL accessor for the location field of hydra.error.core.InvalidLetBindingNameError
invalidLetBindingNameErrorLocation :: Typed.TypedTerm ErrorCore.InvalidLetBindingNameError -> Typed.TypedTerm Paths.SubtermPath
invalidLetBindingNameErrorLocation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidLetBindingNameError"),
        Core.projectionFieldName = (Core.Name "location")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.error.core.InvalidLetBindingNameError
invalidLetBindingNameErrorName :: Typed.TypedTerm ErrorCore.InvalidLetBindingNameError -> Typed.TypedTerm Core.Name
invalidLetBindingNameErrorName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidLetBindingNameError"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the location field of hydra.error.core.InvalidLetBindingNameError
invalidLetBindingNameErrorWithLocation :: Typed.TypedTerm ErrorCore.InvalidLetBindingNameError -> Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.InvalidLetBindingNameError
invalidLetBindingNameErrorWithLocation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.InvalidLetBindingNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidLetBindingNameError"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.error.core.InvalidLetBindingNameError
invalidLetBindingNameErrorWithName :: Typed.TypedTerm ErrorCore.InvalidLetBindingNameError -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.InvalidLetBindingNameError
invalidLetBindingNameErrorWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.InvalidLetBindingNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidLetBindingNameError"),
              Core.projectionFieldName = (Core.Name "location")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the typeMismatch variant of hydra.error.core.InvalidLiteralError
invalidLiteralErrorTypeMismatch :: Typed.TypedTerm ErrorCore.LiteralTypeMismatchError -> Typed.TypedTerm ErrorCore.InvalidLiteralError
invalidLiteralErrorTypeMismatch x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidLiteralError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeMismatch"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the constantCondition variant of hydra.error.core.InvalidTermError
invalidTermErrorConstantCondition :: Typed.TypedTerm ErrorCore.ConstantConditionError -> Typed.TypedTerm ErrorCore.InvalidTermError
invalidTermErrorConstantCondition x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "constantCondition"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the duplicateBinding variant of hydra.error.core.InvalidTermError
invalidTermErrorDuplicateBinding :: Typed.TypedTerm ErrorCore.DuplicateBindingError -> Typed.TypedTerm ErrorCore.InvalidTermError
invalidTermErrorDuplicateBinding x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "duplicateBinding"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the duplicateField variant of hydra.error.core.InvalidTermError
invalidTermErrorDuplicateField :: Typed.TypedTerm ErrorCore.DuplicateFieldError -> Typed.TypedTerm ErrorCore.InvalidTermError
invalidTermErrorDuplicateField x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "duplicateField"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the emptyCaseStatement variant of hydra.error.core.InvalidTermError
invalidTermErrorEmptyCaseStatement :: Typed.TypedTerm ErrorCore.EmptyCaseStatementError -> Typed.TypedTerm ErrorCore.InvalidTermError
invalidTermErrorEmptyCaseStatement x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "emptyCaseStatement"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the emptyLetBindings variant of hydra.error.core.InvalidTermError
invalidTermErrorEmptyLetBindings :: Typed.TypedTerm ErrorCore.EmptyLetBindingsError -> Typed.TypedTerm ErrorCore.InvalidTermError
invalidTermErrorEmptyLetBindings x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "emptyLetBindings"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the emptyTermAnnotation variant of hydra.error.core.InvalidTermError
invalidTermErrorEmptyTermAnnotation :: Typed.TypedTerm ErrorCore.EmptyTermAnnotationError -> Typed.TypedTerm ErrorCore.InvalidTermError
invalidTermErrorEmptyTermAnnotation x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "emptyTermAnnotation"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the emptyTypeNameInTerm variant of hydra.error.core.InvalidTermError
invalidTermErrorEmptyTypeNameInTerm :: Typed.TypedTerm ErrorCore.EmptyTypeNameInTermError -> Typed.TypedTerm ErrorCore.InvalidTermError
invalidTermErrorEmptyTypeNameInTerm x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "emptyTypeNameInTerm"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the invalidLambdaParameterName variant of hydra.error.core.InvalidTermError
invalidTermErrorInvalidLambdaParameterName :: Typed.TypedTerm ErrorCore.InvalidLambdaParameterNameError -> Typed.TypedTerm ErrorCore.InvalidTermError
invalidTermErrorInvalidLambdaParameterName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "invalidLambdaParameterName"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the invalidLetBindingName variant of hydra.error.core.InvalidTermError
invalidTermErrorInvalidLetBindingName :: Typed.TypedTerm ErrorCore.InvalidLetBindingNameError -> Typed.TypedTerm ErrorCore.InvalidTermError
invalidTermErrorInvalidLetBindingName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "invalidLetBindingName"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the invalidTypeLambdaParameterName variant of hydra.error.core.InvalidTermError
invalidTermErrorInvalidTypeLambdaParameterName :: Typed.TypedTerm ErrorCore.InvalidTypeLambdaParameterNameError -> Typed.TypedTerm ErrorCore.InvalidTermError
invalidTermErrorInvalidTypeLambdaParameterName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "invalidTypeLambdaParameterName"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the nestedTermAnnotation variant of hydra.error.core.InvalidTermError
invalidTermErrorNestedTermAnnotation :: Typed.TypedTerm ErrorCore.NestedTermAnnotationError -> Typed.TypedTerm ErrorCore.InvalidTermError
invalidTermErrorNestedTermAnnotation x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nestedTermAnnotation"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the redundantWrapUnwrap variant of hydra.error.core.InvalidTermError
invalidTermErrorRedundantWrapUnwrap :: Typed.TypedTerm ErrorCore.RedundantWrapUnwrapError -> Typed.TypedTerm ErrorCore.InvalidTermError
invalidTermErrorRedundantWrapUnwrap x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "redundantWrapUnwrap"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the selfApplication variant of hydra.error.core.InvalidTermError
invalidTermErrorSelfApplication :: Typed.TypedTerm ErrorCore.SelfApplicationError -> Typed.TypedTerm ErrorCore.InvalidTermError
invalidTermErrorSelfApplication x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "selfApplication"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the termVariableShadowing variant of hydra.error.core.InvalidTermError
invalidTermErrorTermVariableShadowing :: Typed.TypedTerm ErrorCore.TermVariableShadowingError -> Typed.TypedTerm ErrorCore.InvalidTermError
invalidTermErrorTermVariableShadowing x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "termVariableShadowing"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the typeVariableShadowingInTypeLambda variant of hydra.error.core.InvalidTermError
invalidTermErrorTypeVariableShadowingInTypeLambda :: Typed.TypedTerm ErrorCore.TypeVariableShadowingInTypeLambdaError -> Typed.TypedTerm ErrorCore.InvalidTermError
invalidTermErrorTypeVariableShadowingInTypeLambda x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeVariableShadowingInTypeLambda"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the undefinedTermVariable variant of hydra.error.core.InvalidTermError
invalidTermErrorUndefinedTermVariable :: Typed.TypedTerm ErrorCore.UndefinedTermVariableError -> Typed.TypedTerm ErrorCore.InvalidTermError
invalidTermErrorUndefinedTermVariable x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "undefinedTermVariable"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the undefinedTypeVariableInBindingType variant of hydra.error.core.InvalidTermError
invalidTermErrorUndefinedTypeVariableInBindingType :: Typed.TypedTerm ErrorCore.UndefinedTypeVariableInBindingTypeError -> Typed.TypedTerm ErrorCore.InvalidTermError
invalidTermErrorUndefinedTypeVariableInBindingType x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "undefinedTypeVariableInBindingType"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the undefinedTypeVariableInLambdaDomain variant of hydra.error.core.InvalidTermError
invalidTermErrorUndefinedTypeVariableInLambdaDomain :: Typed.TypedTerm ErrorCore.UndefinedTypeVariableInLambdaDomainError -> Typed.TypedTerm ErrorCore.InvalidTermError
invalidTermErrorUndefinedTypeVariableInLambdaDomain x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "undefinedTypeVariableInLambdaDomain"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the undefinedTypeVariableInTypeApplication variant of hydra.error.core.InvalidTermError
invalidTermErrorUndefinedTypeVariableInTypeApplication :: Typed.TypedTerm ErrorCore.UndefinedTypeVariableInTypeApplicationError -> Typed.TypedTerm ErrorCore.InvalidTermError
invalidTermErrorUndefinedTypeVariableInTypeApplication x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "undefinedTypeVariableInTypeApplication"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the unknownPrimitiveName variant of hydra.error.core.InvalidTermError
invalidTermErrorUnknownPrimitiveName :: Typed.TypedTerm ErrorCore.UnknownPrimitiveNameError -> Typed.TypedTerm ErrorCore.InvalidTermError
invalidTermErrorUnknownPrimitiveName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unknownPrimitiveName"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the unnecessaryIdentityApplication variant of hydra.error.core.InvalidTermError
invalidTermErrorUnnecessaryIdentityApplication :: Typed.TypedTerm ErrorCore.UnnecessaryIdentityApplicationError -> Typed.TypedTerm ErrorCore.InvalidTermError
invalidTermErrorUnnecessaryIdentityApplication x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unnecessaryIdentityApplication"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the untypedTermVariable variant of hydra.error.core.InvalidTermError
invalidTermErrorUntypedTermVariable :: Typed.TypedTerm ErrorCore.UntypedTermVariableError -> Typed.TypedTerm ErrorCore.InvalidTermError
invalidTermErrorUntypedTermVariable x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "untypedTermVariable"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the duplicateRecordTypeFieldNames variant of hydra.error.core.InvalidTypeError
invalidTypeErrorDuplicateRecordTypeFieldNames :: Typed.TypedTerm ErrorCore.DuplicateRecordTypeFieldNamesError -> Typed.TypedTerm ErrorCore.InvalidTypeError
invalidTypeErrorDuplicateRecordTypeFieldNames x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "duplicateRecordTypeFieldNames"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the duplicateUnionTypeFieldNames variant of hydra.error.core.InvalidTypeError
invalidTypeErrorDuplicateUnionTypeFieldNames :: Typed.TypedTerm ErrorCore.DuplicateUnionTypeFieldNamesError -> Typed.TypedTerm ErrorCore.InvalidTypeError
invalidTypeErrorDuplicateUnionTypeFieldNames x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "duplicateUnionTypeFieldNames"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the emptyRecordType variant of hydra.error.core.InvalidTypeError
invalidTypeErrorEmptyRecordType :: Typed.TypedTerm ErrorCore.EmptyRecordTypeError -> Typed.TypedTerm ErrorCore.InvalidTypeError
invalidTypeErrorEmptyRecordType x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "emptyRecordType"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the emptyTypeAnnotation variant of hydra.error.core.InvalidTypeError
invalidTypeErrorEmptyTypeAnnotation :: Typed.TypedTerm ErrorCore.EmptyTypeAnnotationError -> Typed.TypedTerm ErrorCore.InvalidTypeError
invalidTypeErrorEmptyTypeAnnotation x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "emptyTypeAnnotation"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the emptyUnionType variant of hydra.error.core.InvalidTypeError
invalidTypeErrorEmptyUnionType :: Typed.TypedTerm ErrorCore.EmptyUnionTypeError -> Typed.TypedTerm ErrorCore.InvalidTypeError
invalidTypeErrorEmptyUnionType x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "emptyUnionType"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the invalidForallParameterName variant of hydra.error.core.InvalidTypeError
invalidTypeErrorInvalidForallParameterName :: Typed.TypedTerm ErrorCore.InvalidForallParameterNameError -> Typed.TypedTerm ErrorCore.InvalidTypeError
invalidTypeErrorInvalidForallParameterName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "invalidForallParameterName"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the invalidTypeSchemeVariableName variant of hydra.error.core.InvalidTypeError
invalidTypeErrorInvalidTypeSchemeVariableName :: Typed.TypedTerm ErrorCore.InvalidTypeSchemeVariableNameError -> Typed.TypedTerm ErrorCore.InvalidTypeError
invalidTypeErrorInvalidTypeSchemeVariableName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "invalidTypeSchemeVariableName"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the nestedTypeAnnotation variant of hydra.error.core.InvalidTypeError
invalidTypeErrorNestedTypeAnnotation :: Typed.TypedTerm ErrorCore.NestedTypeAnnotationError -> Typed.TypedTerm ErrorCore.InvalidTypeError
invalidTypeErrorNestedTypeAnnotation x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nestedTypeAnnotation"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the nonComparableMapKeyType variant of hydra.error.core.InvalidTypeError
invalidTypeErrorNonComparableMapKeyType :: Typed.TypedTerm ErrorCore.NonComparableMapKeyTypeError -> Typed.TypedTerm ErrorCore.InvalidTypeError
invalidTypeErrorNonComparableMapKeyType x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nonComparableMapKeyType"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the nonComparableSetElementType variant of hydra.error.core.InvalidTypeError
invalidTypeErrorNonComparableSetElementType :: Typed.TypedTerm ErrorCore.NonComparableSetElementTypeError -> Typed.TypedTerm ErrorCore.InvalidTypeError
invalidTypeErrorNonComparableSetElementType x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nonComparableSetElementType"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the singleVariantUnion variant of hydra.error.core.InvalidTypeError
invalidTypeErrorSingleVariantUnion :: Typed.TypedTerm ErrorCore.SingleVariantUnionError -> Typed.TypedTerm ErrorCore.InvalidTypeError
invalidTypeErrorSingleVariantUnion x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "singleVariantUnion"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the typeVariableShadowingInForall variant of hydra.error.core.InvalidTypeError
invalidTypeErrorTypeVariableShadowingInForall :: Typed.TypedTerm ErrorCore.TypeVariableShadowingInForallError -> Typed.TypedTerm ErrorCore.InvalidTypeError
invalidTypeErrorTypeVariableShadowingInForall x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeVariableShadowingInForall"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the undefinedTypeVariable variant of hydra.error.core.InvalidTypeError
invalidTypeErrorUndefinedTypeVariable :: Typed.TypedTerm ErrorCore.UndefinedTypeVariableError -> Typed.TypedTerm ErrorCore.InvalidTypeError
invalidTypeErrorUndefinedTypeVariable x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "undefinedTypeVariable"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the voidInNonBottomPosition variant of hydra.error.core.InvalidTypeError
invalidTypeErrorVoidInNonBottomPosition :: Typed.TypedTerm ErrorCore.VoidInNonBottomPositionError -> Typed.TypedTerm ErrorCore.InvalidTypeError
invalidTypeErrorVoidInNonBottomPosition x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "voidInNonBottomPosition"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.error.core.InvalidTypeLambdaParameterNameError
invalidTypeLambdaParameterNameError :: Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.InvalidTypeLambdaParameterNameError
invalidTypeLambdaParameterNameError location name =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.InvalidTypeLambdaParameterNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)}]}))
-- | DSL accessor for the location field of hydra.error.core.InvalidTypeLambdaParameterNameError
invalidTypeLambdaParameterNameErrorLocation :: Typed.TypedTerm ErrorCore.InvalidTypeLambdaParameterNameError -> Typed.TypedTerm Paths.SubtermPath
invalidTypeLambdaParameterNameErrorLocation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidTypeLambdaParameterNameError"),
        Core.projectionFieldName = (Core.Name "location")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.error.core.InvalidTypeLambdaParameterNameError
invalidTypeLambdaParameterNameErrorName :: Typed.TypedTerm ErrorCore.InvalidTypeLambdaParameterNameError -> Typed.TypedTerm Core.Name
invalidTypeLambdaParameterNameErrorName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidTypeLambdaParameterNameError"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the location field of hydra.error.core.InvalidTypeLambdaParameterNameError
invalidTypeLambdaParameterNameErrorWithLocation :: Typed.TypedTerm ErrorCore.InvalidTypeLambdaParameterNameError -> Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.InvalidTypeLambdaParameterNameError
invalidTypeLambdaParameterNameErrorWithLocation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.InvalidTypeLambdaParameterNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidTypeLambdaParameterNameError"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.error.core.InvalidTypeLambdaParameterNameError
invalidTypeLambdaParameterNameErrorWithName :: Typed.TypedTerm ErrorCore.InvalidTypeLambdaParameterNameError -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.InvalidTypeLambdaParameterNameError
invalidTypeLambdaParameterNameErrorWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.InvalidTypeLambdaParameterNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidTypeLambdaParameterNameError"),
              Core.projectionFieldName = (Core.Name "location")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.error.core.InvalidTypeSchemeVariableNameError
invalidTypeSchemeVariableNameError :: Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.InvalidTypeSchemeVariableNameError
invalidTypeSchemeVariableNameError location name =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.InvalidTypeSchemeVariableNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)}]}))
-- | DSL accessor for the location field of hydra.error.core.InvalidTypeSchemeVariableNameError
invalidTypeSchemeVariableNameErrorLocation :: Typed.TypedTerm ErrorCore.InvalidTypeSchemeVariableNameError -> Typed.TypedTerm Paths.SubtermPath
invalidTypeSchemeVariableNameErrorLocation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidTypeSchemeVariableNameError"),
        Core.projectionFieldName = (Core.Name "location")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.error.core.InvalidTypeSchemeVariableNameError
invalidTypeSchemeVariableNameErrorName :: Typed.TypedTerm ErrorCore.InvalidTypeSchemeVariableNameError -> Typed.TypedTerm Core.Name
invalidTypeSchemeVariableNameErrorName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidTypeSchemeVariableNameError"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the location field of hydra.error.core.InvalidTypeSchemeVariableNameError
invalidTypeSchemeVariableNameErrorWithLocation :: Typed.TypedTerm ErrorCore.InvalidTypeSchemeVariableNameError -> Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.InvalidTypeSchemeVariableNameError
invalidTypeSchemeVariableNameErrorWithLocation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.InvalidTypeSchemeVariableNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidTypeSchemeVariableNameError"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.error.core.InvalidTypeSchemeVariableNameError
invalidTypeSchemeVariableNameErrorWithName :: Typed.TypedTerm ErrorCore.InvalidTypeSchemeVariableNameError -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.InvalidTypeSchemeVariableNameError
invalidTypeSchemeVariableNameErrorWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.InvalidTypeSchemeVariableNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidTypeSchemeVariableNameError"),
              Core.projectionFieldName = (Core.Name "location")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.error.core.LiteralTypeMismatchError
literalTypeMismatchError :: Typed.TypedTerm Core.LiteralType -> Typed.TypedTerm Core.LiteralType -> Typed.TypedTerm ErrorCore.LiteralTypeMismatchError
literalTypeMismatchError expectedType actualType =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.LiteralTypeMismatchError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expectedType"),
          Core.fieldTerm = (Typed.unTypedTerm expectedType)},
        Core.Field {
          Core.fieldName = (Core.Name "actualType"),
          Core.fieldTerm = (Typed.unTypedTerm actualType)}]}))
-- | DSL accessor for the actualType field of hydra.error.core.LiteralTypeMismatchError
literalTypeMismatchErrorActualType :: Typed.TypedTerm ErrorCore.LiteralTypeMismatchError -> Typed.TypedTerm Core.LiteralType
literalTypeMismatchErrorActualType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.LiteralTypeMismatchError"),
        Core.projectionFieldName = (Core.Name "actualType")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the expectedType field of hydra.error.core.LiteralTypeMismatchError
literalTypeMismatchErrorExpectedType :: Typed.TypedTerm ErrorCore.LiteralTypeMismatchError -> Typed.TypedTerm Core.LiteralType
literalTypeMismatchErrorExpectedType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.LiteralTypeMismatchError"),
        Core.projectionFieldName = (Core.Name "expectedType")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the actualType field of hydra.error.core.LiteralTypeMismatchError
literalTypeMismatchErrorWithActualType :: Typed.TypedTerm ErrorCore.LiteralTypeMismatchError -> Typed.TypedTerm Core.LiteralType -> Typed.TypedTerm ErrorCore.LiteralTypeMismatchError
literalTypeMismatchErrorWithActualType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.LiteralTypeMismatchError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expectedType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.LiteralTypeMismatchError"),
              Core.projectionFieldName = (Core.Name "expectedType")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "actualType"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the expectedType field of hydra.error.core.LiteralTypeMismatchError
literalTypeMismatchErrorWithExpectedType :: Typed.TypedTerm ErrorCore.LiteralTypeMismatchError -> Typed.TypedTerm Core.LiteralType -> Typed.TypedTerm ErrorCore.LiteralTypeMismatchError
literalTypeMismatchErrorWithExpectedType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.LiteralTypeMismatchError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expectedType"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "actualType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.LiteralTypeMismatchError"),
              Core.projectionFieldName = (Core.Name "actualType")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.error.core.NestedTermAnnotationError
nestedTermAnnotationError :: Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.NestedTermAnnotationError
nestedTermAnnotationError location =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.NestedTermAnnotationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm location)}]}))
-- | DSL accessor for the location field of hydra.error.core.NestedTermAnnotationError
nestedTermAnnotationErrorLocation :: Typed.TypedTerm ErrorCore.NestedTermAnnotationError -> Typed.TypedTerm Paths.SubtermPath
nestedTermAnnotationErrorLocation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.NestedTermAnnotationError"),
        Core.projectionFieldName = (Core.Name "location")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the location field of hydra.error.core.NestedTermAnnotationError
nestedTermAnnotationErrorWithLocation :: Typed.TypedTerm ErrorCore.NestedTermAnnotationError -> Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.NestedTermAnnotationError
nestedTermAnnotationErrorWithLocation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.NestedTermAnnotationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.error.core.NestedTypeAnnotationError
nestedTypeAnnotationError :: Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.NestedTypeAnnotationError
nestedTypeAnnotationError location =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.NestedTypeAnnotationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm location)}]}))
-- | DSL accessor for the location field of hydra.error.core.NestedTypeAnnotationError
nestedTypeAnnotationErrorLocation :: Typed.TypedTerm ErrorCore.NestedTypeAnnotationError -> Typed.TypedTerm Paths.SubtermPath
nestedTypeAnnotationErrorLocation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.NestedTypeAnnotationError"),
        Core.projectionFieldName = (Core.Name "location")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the location field of hydra.error.core.NestedTypeAnnotationError
nestedTypeAnnotationErrorWithLocation :: Typed.TypedTerm ErrorCore.NestedTypeAnnotationError -> Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.NestedTypeAnnotationError
nestedTypeAnnotationErrorWithLocation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.NestedTypeAnnotationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.error.core.NonComparableMapKeyTypeError
nonComparableMapKeyTypeError :: Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm Core.Type -> Typed.TypedTerm ErrorCore.NonComparableMapKeyTypeError
nonComparableMapKeyTypeError location keyType =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.NonComparableMapKeyTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "keyType"),
          Core.fieldTerm = (Typed.unTypedTerm keyType)}]}))
-- | DSL accessor for the keyType field of hydra.error.core.NonComparableMapKeyTypeError
nonComparableMapKeyTypeErrorKeyType :: Typed.TypedTerm ErrorCore.NonComparableMapKeyTypeError -> Typed.TypedTerm Core.Type
nonComparableMapKeyTypeErrorKeyType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.NonComparableMapKeyTypeError"),
        Core.projectionFieldName = (Core.Name "keyType")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the location field of hydra.error.core.NonComparableMapKeyTypeError
nonComparableMapKeyTypeErrorLocation :: Typed.TypedTerm ErrorCore.NonComparableMapKeyTypeError -> Typed.TypedTerm Paths.SubtermPath
nonComparableMapKeyTypeErrorLocation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.NonComparableMapKeyTypeError"),
        Core.projectionFieldName = (Core.Name "location")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the keyType field of hydra.error.core.NonComparableMapKeyTypeError
nonComparableMapKeyTypeErrorWithKeyType :: Typed.TypedTerm ErrorCore.NonComparableMapKeyTypeError -> Typed.TypedTerm Core.Type -> Typed.TypedTerm ErrorCore.NonComparableMapKeyTypeError
nonComparableMapKeyTypeErrorWithKeyType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.NonComparableMapKeyTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.NonComparableMapKeyTypeError"),
              Core.projectionFieldName = (Core.Name "location")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "keyType"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the location field of hydra.error.core.NonComparableMapKeyTypeError
nonComparableMapKeyTypeErrorWithLocation :: Typed.TypedTerm ErrorCore.NonComparableMapKeyTypeError -> Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.NonComparableMapKeyTypeError
nonComparableMapKeyTypeErrorWithLocation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.NonComparableMapKeyTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "keyType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.NonComparableMapKeyTypeError"),
              Core.projectionFieldName = (Core.Name "keyType")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.error.core.NonComparableSetElementTypeError
nonComparableSetElementTypeError :: Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm Core.Type -> Typed.TypedTerm ErrorCore.NonComparableSetElementTypeError
nonComparableSetElementTypeError location elementType =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.NonComparableSetElementTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "elementType"),
          Core.fieldTerm = (Typed.unTypedTerm elementType)}]}))
-- | DSL accessor for the elementType field of hydra.error.core.NonComparableSetElementTypeError
nonComparableSetElementTypeErrorElementType :: Typed.TypedTerm ErrorCore.NonComparableSetElementTypeError -> Typed.TypedTerm Core.Type
nonComparableSetElementTypeErrorElementType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.NonComparableSetElementTypeError"),
        Core.projectionFieldName = (Core.Name "elementType")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the location field of hydra.error.core.NonComparableSetElementTypeError
nonComparableSetElementTypeErrorLocation :: Typed.TypedTerm ErrorCore.NonComparableSetElementTypeError -> Typed.TypedTerm Paths.SubtermPath
nonComparableSetElementTypeErrorLocation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.NonComparableSetElementTypeError"),
        Core.projectionFieldName = (Core.Name "location")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the elementType field of hydra.error.core.NonComparableSetElementTypeError
nonComparableSetElementTypeErrorWithElementType :: Typed.TypedTerm ErrorCore.NonComparableSetElementTypeError -> Typed.TypedTerm Core.Type -> Typed.TypedTerm ErrorCore.NonComparableSetElementTypeError
nonComparableSetElementTypeErrorWithElementType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.NonComparableSetElementTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.NonComparableSetElementTypeError"),
              Core.projectionFieldName = (Core.Name "location")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "elementType"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the location field of hydra.error.core.NonComparableSetElementTypeError
nonComparableSetElementTypeErrorWithLocation :: Typed.TypedTerm ErrorCore.NonComparableSetElementTypeError -> Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.NonComparableSetElementTypeError
nonComparableSetElementTypeErrorWithLocation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.NonComparableSetElementTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "elementType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.NonComparableSetElementTypeError"),
              Core.projectionFieldName = (Core.Name "elementType")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.error.core.RedundantWrapUnwrapError
redundantWrapUnwrapError :: Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.RedundantWrapUnwrapError
redundantWrapUnwrapError location typeName =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.RedundantWrapUnwrapError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Typed.unTypedTerm typeName)}]}))
-- | DSL accessor for the location field of hydra.error.core.RedundantWrapUnwrapError
redundantWrapUnwrapErrorLocation :: Typed.TypedTerm ErrorCore.RedundantWrapUnwrapError -> Typed.TypedTerm Paths.SubtermPath
redundantWrapUnwrapErrorLocation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.RedundantWrapUnwrapError"),
        Core.projectionFieldName = (Core.Name "location")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeName field of hydra.error.core.RedundantWrapUnwrapError
redundantWrapUnwrapErrorTypeName :: Typed.TypedTerm ErrorCore.RedundantWrapUnwrapError -> Typed.TypedTerm Core.Name
redundantWrapUnwrapErrorTypeName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.RedundantWrapUnwrapError"),
        Core.projectionFieldName = (Core.Name "typeName")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the location field of hydra.error.core.RedundantWrapUnwrapError
redundantWrapUnwrapErrorWithLocation :: Typed.TypedTerm ErrorCore.RedundantWrapUnwrapError -> Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.RedundantWrapUnwrapError
redundantWrapUnwrapErrorWithLocation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.RedundantWrapUnwrapError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.RedundantWrapUnwrapError"),
              Core.projectionFieldName = (Core.Name "typeName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the typeName field of hydra.error.core.RedundantWrapUnwrapError
redundantWrapUnwrapErrorWithTypeName :: Typed.TypedTerm ErrorCore.RedundantWrapUnwrapError -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.RedundantWrapUnwrapError
redundantWrapUnwrapErrorWithTypeName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.RedundantWrapUnwrapError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.RedundantWrapUnwrapError"),
              Core.projectionFieldName = (Core.Name "location")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.error.core.SelfApplicationError
selfApplicationError :: Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.SelfApplicationError
selfApplicationError location name =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.SelfApplicationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)}]}))
-- | DSL accessor for the location field of hydra.error.core.SelfApplicationError
selfApplicationErrorLocation :: Typed.TypedTerm ErrorCore.SelfApplicationError -> Typed.TypedTerm Paths.SubtermPath
selfApplicationErrorLocation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.SelfApplicationError"),
        Core.projectionFieldName = (Core.Name "location")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.error.core.SelfApplicationError
selfApplicationErrorName :: Typed.TypedTerm ErrorCore.SelfApplicationError -> Typed.TypedTerm Core.Name
selfApplicationErrorName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.SelfApplicationError"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the location field of hydra.error.core.SelfApplicationError
selfApplicationErrorWithLocation :: Typed.TypedTerm ErrorCore.SelfApplicationError -> Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.SelfApplicationError
selfApplicationErrorWithLocation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.SelfApplicationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.SelfApplicationError"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.error.core.SelfApplicationError
selfApplicationErrorWithName :: Typed.TypedTerm ErrorCore.SelfApplicationError -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.SelfApplicationError
selfApplicationErrorWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.SelfApplicationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.SelfApplicationError"),
              Core.projectionFieldName = (Core.Name "location")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.error.core.SingleVariantUnionError
singleVariantUnionError :: Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.SingleVariantUnionError
singleVariantUnionError location fieldName =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.SingleVariantUnionError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "fieldName"),
          Core.fieldTerm = (Typed.unTypedTerm fieldName)}]}))
-- | DSL accessor for the fieldName field of hydra.error.core.SingleVariantUnionError
singleVariantUnionErrorFieldName :: Typed.TypedTerm ErrorCore.SingleVariantUnionError -> Typed.TypedTerm Core.Name
singleVariantUnionErrorFieldName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.SingleVariantUnionError"),
        Core.projectionFieldName = (Core.Name "fieldName")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the location field of hydra.error.core.SingleVariantUnionError
singleVariantUnionErrorLocation :: Typed.TypedTerm ErrorCore.SingleVariantUnionError -> Typed.TypedTerm Paths.SubtermPath
singleVariantUnionErrorLocation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.SingleVariantUnionError"),
        Core.projectionFieldName = (Core.Name "location")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the fieldName field of hydra.error.core.SingleVariantUnionError
singleVariantUnionErrorWithFieldName :: Typed.TypedTerm ErrorCore.SingleVariantUnionError -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.SingleVariantUnionError
singleVariantUnionErrorWithFieldName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.SingleVariantUnionError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.SingleVariantUnionError"),
              Core.projectionFieldName = (Core.Name "location")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fieldName"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the location field of hydra.error.core.SingleVariantUnionError
singleVariantUnionErrorWithLocation :: Typed.TypedTerm ErrorCore.SingleVariantUnionError -> Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.SingleVariantUnionError
singleVariantUnionErrorWithLocation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.SingleVariantUnionError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fieldName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.SingleVariantUnionError"),
              Core.projectionFieldName = (Core.Name "fieldName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.error.core.TermVariableShadowingError
termVariableShadowingError :: Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.TermVariableShadowingError
termVariableShadowingError location name =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.TermVariableShadowingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)}]}))
-- | DSL accessor for the location field of hydra.error.core.TermVariableShadowingError
termVariableShadowingErrorLocation :: Typed.TypedTerm ErrorCore.TermVariableShadowingError -> Typed.TypedTerm Paths.SubtermPath
termVariableShadowingErrorLocation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.TermVariableShadowingError"),
        Core.projectionFieldName = (Core.Name "location")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.error.core.TermVariableShadowingError
termVariableShadowingErrorName :: Typed.TypedTerm ErrorCore.TermVariableShadowingError -> Typed.TypedTerm Core.Name
termVariableShadowingErrorName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.TermVariableShadowingError"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the location field of hydra.error.core.TermVariableShadowingError
termVariableShadowingErrorWithLocation :: Typed.TypedTerm ErrorCore.TermVariableShadowingError -> Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.TermVariableShadowingError
termVariableShadowingErrorWithLocation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.TermVariableShadowingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.TermVariableShadowingError"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.error.core.TermVariableShadowingError
termVariableShadowingErrorWithName :: Typed.TypedTerm ErrorCore.TermVariableShadowingError -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.TermVariableShadowingError
termVariableShadowingErrorWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.TermVariableShadowingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.TermVariableShadowingError"),
              Core.projectionFieldName = (Core.Name "location")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.error.core.TypeVariableShadowingInForallError
typeVariableShadowingInForallError :: Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.TypeVariableShadowingInForallError
typeVariableShadowingInForallError location name =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.TypeVariableShadowingInForallError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)}]}))
-- | DSL accessor for the location field of hydra.error.core.TypeVariableShadowingInForallError
typeVariableShadowingInForallErrorLocation :: Typed.TypedTerm ErrorCore.TypeVariableShadowingInForallError -> Typed.TypedTerm Paths.SubtermPath
typeVariableShadowingInForallErrorLocation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.TypeVariableShadowingInForallError"),
        Core.projectionFieldName = (Core.Name "location")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.error.core.TypeVariableShadowingInForallError
typeVariableShadowingInForallErrorName :: Typed.TypedTerm ErrorCore.TypeVariableShadowingInForallError -> Typed.TypedTerm Core.Name
typeVariableShadowingInForallErrorName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.TypeVariableShadowingInForallError"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the location field of hydra.error.core.TypeVariableShadowingInForallError
typeVariableShadowingInForallErrorWithLocation :: Typed.TypedTerm ErrorCore.TypeVariableShadowingInForallError -> Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.TypeVariableShadowingInForallError
typeVariableShadowingInForallErrorWithLocation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.TypeVariableShadowingInForallError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.TypeVariableShadowingInForallError"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.error.core.TypeVariableShadowingInForallError
typeVariableShadowingInForallErrorWithName :: Typed.TypedTerm ErrorCore.TypeVariableShadowingInForallError -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.TypeVariableShadowingInForallError
typeVariableShadowingInForallErrorWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.TypeVariableShadowingInForallError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.TypeVariableShadowingInForallError"),
              Core.projectionFieldName = (Core.Name "location")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.error.core.TypeVariableShadowingInTypeLambdaError
typeVariableShadowingInTypeLambdaError :: Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.TypeVariableShadowingInTypeLambdaError
typeVariableShadowingInTypeLambdaError location name =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.TypeVariableShadowingInTypeLambdaError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)}]}))
-- | DSL accessor for the location field of hydra.error.core.TypeVariableShadowingInTypeLambdaError
typeVariableShadowingInTypeLambdaErrorLocation :: Typed.TypedTerm ErrorCore.TypeVariableShadowingInTypeLambdaError -> Typed.TypedTerm Paths.SubtermPath
typeVariableShadowingInTypeLambdaErrorLocation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.TypeVariableShadowingInTypeLambdaError"),
        Core.projectionFieldName = (Core.Name "location")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.error.core.TypeVariableShadowingInTypeLambdaError
typeVariableShadowingInTypeLambdaErrorName :: Typed.TypedTerm ErrorCore.TypeVariableShadowingInTypeLambdaError -> Typed.TypedTerm Core.Name
typeVariableShadowingInTypeLambdaErrorName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.TypeVariableShadowingInTypeLambdaError"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the location field of hydra.error.core.TypeVariableShadowingInTypeLambdaError
typeVariableShadowingInTypeLambdaErrorWithLocation :: Typed.TypedTerm ErrorCore.TypeVariableShadowingInTypeLambdaError -> Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.TypeVariableShadowingInTypeLambdaError
typeVariableShadowingInTypeLambdaErrorWithLocation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.TypeVariableShadowingInTypeLambdaError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.TypeVariableShadowingInTypeLambdaError"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.error.core.TypeVariableShadowingInTypeLambdaError
typeVariableShadowingInTypeLambdaErrorWithName :: Typed.TypedTerm ErrorCore.TypeVariableShadowingInTypeLambdaError -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.TypeVariableShadowingInTypeLambdaError
typeVariableShadowingInTypeLambdaErrorWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.TypeVariableShadowingInTypeLambdaError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.TypeVariableShadowingInTypeLambdaError"),
              Core.projectionFieldName = (Core.Name "location")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.error.core.UndefinedFieldError
undefinedFieldError :: Typed.TypedTerm Core.Name -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.UndefinedFieldError
undefinedFieldError fieldName typeName =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedFieldError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fieldName"),
          Core.fieldTerm = (Typed.unTypedTerm fieldName)},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Typed.unTypedTerm typeName)}]}))
-- | DSL accessor for the fieldName field of hydra.error.core.UndefinedFieldError
undefinedFieldErrorFieldName :: Typed.TypedTerm ErrorCore.UndefinedFieldError -> Typed.TypedTerm Core.Name
undefinedFieldErrorFieldName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedFieldError"),
        Core.projectionFieldName = (Core.Name "fieldName")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeName field of hydra.error.core.UndefinedFieldError
undefinedFieldErrorTypeName :: Typed.TypedTerm ErrorCore.UndefinedFieldError -> Typed.TypedTerm Core.Name
undefinedFieldErrorTypeName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedFieldError"),
        Core.projectionFieldName = (Core.Name "typeName")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the fieldName field of hydra.error.core.UndefinedFieldError
undefinedFieldErrorWithFieldName :: Typed.TypedTerm ErrorCore.UndefinedFieldError -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.UndefinedFieldError
undefinedFieldErrorWithFieldName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedFieldError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fieldName"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedFieldError"),
              Core.projectionFieldName = (Core.Name "typeName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the typeName field of hydra.error.core.UndefinedFieldError
undefinedFieldErrorWithTypeName :: Typed.TypedTerm ErrorCore.UndefinedFieldError -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.UndefinedFieldError
undefinedFieldErrorWithTypeName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedFieldError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fieldName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedFieldError"),
              Core.projectionFieldName = (Core.Name "fieldName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.error.core.UndefinedTermVariableError
undefinedTermVariableError :: Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.UndefinedTermVariableError
undefinedTermVariableError location name =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTermVariableError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)}]}))
-- | DSL accessor for the location field of hydra.error.core.UndefinedTermVariableError
undefinedTermVariableErrorLocation :: Typed.TypedTerm ErrorCore.UndefinedTermVariableError -> Typed.TypedTerm Paths.SubtermPath
undefinedTermVariableErrorLocation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTermVariableError"),
        Core.projectionFieldName = (Core.Name "location")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.error.core.UndefinedTermVariableError
undefinedTermVariableErrorName :: Typed.TypedTerm ErrorCore.UndefinedTermVariableError -> Typed.TypedTerm Core.Name
undefinedTermVariableErrorName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTermVariableError"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the location field of hydra.error.core.UndefinedTermVariableError
undefinedTermVariableErrorWithLocation :: Typed.TypedTerm ErrorCore.UndefinedTermVariableError -> Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.UndefinedTermVariableError
undefinedTermVariableErrorWithLocation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTermVariableError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTermVariableError"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.error.core.UndefinedTermVariableError
undefinedTermVariableErrorWithName :: Typed.TypedTerm ErrorCore.UndefinedTermVariableError -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.UndefinedTermVariableError
undefinedTermVariableErrorWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTermVariableError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTermVariableError"),
              Core.projectionFieldName = (Core.Name "location")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.error.core.UndefinedTypeVariableError
undefinedTypeVariableError :: Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.UndefinedTypeVariableError
undefinedTypeVariableError location name =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)}]}))
-- | DSL accessor for the location field of hydra.error.core.UndefinedTypeVariableError
undefinedTypeVariableErrorLocation :: Typed.TypedTerm ErrorCore.UndefinedTypeVariableError -> Typed.TypedTerm Paths.SubtermPath
undefinedTypeVariableErrorLocation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableError"),
        Core.projectionFieldName = (Core.Name "location")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.error.core.UndefinedTypeVariableError
undefinedTypeVariableErrorName :: Typed.TypedTerm ErrorCore.UndefinedTypeVariableError -> Typed.TypedTerm Core.Name
undefinedTypeVariableErrorName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableError"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the location field of hydra.error.core.UndefinedTypeVariableError
undefinedTypeVariableErrorWithLocation :: Typed.TypedTerm ErrorCore.UndefinedTypeVariableError -> Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.UndefinedTypeVariableError
undefinedTypeVariableErrorWithLocation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableError"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.error.core.UndefinedTypeVariableError
undefinedTypeVariableErrorWithName :: Typed.TypedTerm ErrorCore.UndefinedTypeVariableError -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.UndefinedTypeVariableError
undefinedTypeVariableErrorWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableError"),
              Core.projectionFieldName = (Core.Name "location")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.error.core.UndefinedTypeVariableInBindingTypeError
undefinedTypeVariableInBindingTypeError :: Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.UndefinedTypeVariableInBindingTypeError
undefinedTypeVariableInBindingTypeError location name =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInBindingTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)}]}))
-- | DSL accessor for the location field of hydra.error.core.UndefinedTypeVariableInBindingTypeError
undefinedTypeVariableInBindingTypeErrorLocation :: Typed.TypedTerm ErrorCore.UndefinedTypeVariableInBindingTypeError -> Typed.TypedTerm Paths.SubtermPath
undefinedTypeVariableInBindingTypeErrorLocation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInBindingTypeError"),
        Core.projectionFieldName = (Core.Name "location")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.error.core.UndefinedTypeVariableInBindingTypeError
undefinedTypeVariableInBindingTypeErrorName :: Typed.TypedTerm ErrorCore.UndefinedTypeVariableInBindingTypeError -> Typed.TypedTerm Core.Name
undefinedTypeVariableInBindingTypeErrorName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInBindingTypeError"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the location field of hydra.error.core.UndefinedTypeVariableInBindingTypeError
undefinedTypeVariableInBindingTypeErrorWithLocation :: Typed.TypedTerm ErrorCore.UndefinedTypeVariableInBindingTypeError -> Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.UndefinedTypeVariableInBindingTypeError
undefinedTypeVariableInBindingTypeErrorWithLocation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInBindingTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInBindingTypeError"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.error.core.UndefinedTypeVariableInBindingTypeError
undefinedTypeVariableInBindingTypeErrorWithName :: Typed.TypedTerm ErrorCore.UndefinedTypeVariableInBindingTypeError -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.UndefinedTypeVariableInBindingTypeError
undefinedTypeVariableInBindingTypeErrorWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInBindingTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInBindingTypeError"),
              Core.projectionFieldName = (Core.Name "location")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.error.core.UndefinedTypeVariableInLambdaDomainError
undefinedTypeVariableInLambdaDomainError :: Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.UndefinedTypeVariableInLambdaDomainError
undefinedTypeVariableInLambdaDomainError location name =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInLambdaDomainError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)}]}))
-- | DSL accessor for the location field of hydra.error.core.UndefinedTypeVariableInLambdaDomainError
undefinedTypeVariableInLambdaDomainErrorLocation :: Typed.TypedTerm ErrorCore.UndefinedTypeVariableInLambdaDomainError -> Typed.TypedTerm Paths.SubtermPath
undefinedTypeVariableInLambdaDomainErrorLocation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInLambdaDomainError"),
        Core.projectionFieldName = (Core.Name "location")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.error.core.UndefinedTypeVariableInLambdaDomainError
undefinedTypeVariableInLambdaDomainErrorName :: Typed.TypedTerm ErrorCore.UndefinedTypeVariableInLambdaDomainError -> Typed.TypedTerm Core.Name
undefinedTypeVariableInLambdaDomainErrorName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInLambdaDomainError"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the location field of hydra.error.core.UndefinedTypeVariableInLambdaDomainError
undefinedTypeVariableInLambdaDomainErrorWithLocation :: Typed.TypedTerm ErrorCore.UndefinedTypeVariableInLambdaDomainError -> Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.UndefinedTypeVariableInLambdaDomainError
undefinedTypeVariableInLambdaDomainErrorWithLocation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInLambdaDomainError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInLambdaDomainError"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.error.core.UndefinedTypeVariableInLambdaDomainError
undefinedTypeVariableInLambdaDomainErrorWithName :: Typed.TypedTerm ErrorCore.UndefinedTypeVariableInLambdaDomainError -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.UndefinedTypeVariableInLambdaDomainError
undefinedTypeVariableInLambdaDomainErrorWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInLambdaDomainError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInLambdaDomainError"),
              Core.projectionFieldName = (Core.Name "location")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.error.core.UndefinedTypeVariableInTypeApplicationError
undefinedTypeVariableInTypeApplicationError :: Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.UndefinedTypeVariableInTypeApplicationError
undefinedTypeVariableInTypeApplicationError location name =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInTypeApplicationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)}]}))
-- | DSL accessor for the location field of hydra.error.core.UndefinedTypeVariableInTypeApplicationError
undefinedTypeVariableInTypeApplicationErrorLocation :: Typed.TypedTerm ErrorCore.UndefinedTypeVariableInTypeApplicationError -> Typed.TypedTerm Paths.SubtermPath
undefinedTypeVariableInTypeApplicationErrorLocation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInTypeApplicationError"),
        Core.projectionFieldName = (Core.Name "location")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.error.core.UndefinedTypeVariableInTypeApplicationError
undefinedTypeVariableInTypeApplicationErrorName :: Typed.TypedTerm ErrorCore.UndefinedTypeVariableInTypeApplicationError -> Typed.TypedTerm Core.Name
undefinedTypeVariableInTypeApplicationErrorName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInTypeApplicationError"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the location field of hydra.error.core.UndefinedTypeVariableInTypeApplicationError
undefinedTypeVariableInTypeApplicationErrorWithLocation :: Typed.TypedTerm ErrorCore.UndefinedTypeVariableInTypeApplicationError -> Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.UndefinedTypeVariableInTypeApplicationError
undefinedTypeVariableInTypeApplicationErrorWithLocation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInTypeApplicationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInTypeApplicationError"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.error.core.UndefinedTypeVariableInTypeApplicationError
undefinedTypeVariableInTypeApplicationErrorWithName :: Typed.TypedTerm ErrorCore.UndefinedTypeVariableInTypeApplicationError -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.UndefinedTypeVariableInTypeApplicationError
undefinedTypeVariableInTypeApplicationErrorWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInTypeApplicationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInTypeApplicationError"),
              Core.projectionFieldName = (Core.Name "location")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.error.core.UnexpectedTermVariantError
unexpectedTermVariantError :: Typed.TypedTerm Variants.TermVariant -> Typed.TypedTerm Core.Term -> Typed.TypedTerm ErrorCore.UnexpectedTermVariantError
unexpectedTermVariantError expectedVariant actualTerm =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UnexpectedTermVariantError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expectedVariant"),
          Core.fieldTerm = (Typed.unTypedTerm expectedVariant)},
        Core.Field {
          Core.fieldName = (Core.Name "actualTerm"),
          Core.fieldTerm = (Typed.unTypedTerm actualTerm)}]}))
-- | DSL accessor for the actualTerm field of hydra.error.core.UnexpectedTermVariantError
unexpectedTermVariantErrorActualTerm :: Typed.TypedTerm ErrorCore.UnexpectedTermVariantError -> Typed.TypedTerm Core.Term
unexpectedTermVariantErrorActualTerm x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UnexpectedTermVariantError"),
        Core.projectionFieldName = (Core.Name "actualTerm")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the expectedVariant field of hydra.error.core.UnexpectedTermVariantError
unexpectedTermVariantErrorExpectedVariant :: Typed.TypedTerm ErrorCore.UnexpectedTermVariantError -> Typed.TypedTerm Variants.TermVariant
unexpectedTermVariantErrorExpectedVariant x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UnexpectedTermVariantError"),
        Core.projectionFieldName = (Core.Name "expectedVariant")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the actualTerm field of hydra.error.core.UnexpectedTermVariantError
unexpectedTermVariantErrorWithActualTerm :: Typed.TypedTerm ErrorCore.UnexpectedTermVariantError -> Typed.TypedTerm Core.Term -> Typed.TypedTerm ErrorCore.UnexpectedTermVariantError
unexpectedTermVariantErrorWithActualTerm original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UnexpectedTermVariantError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expectedVariant"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UnexpectedTermVariantError"),
              Core.projectionFieldName = (Core.Name "expectedVariant")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "actualTerm"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the expectedVariant field of hydra.error.core.UnexpectedTermVariantError
unexpectedTermVariantErrorWithExpectedVariant :: Typed.TypedTerm ErrorCore.UnexpectedTermVariantError -> Typed.TypedTerm Variants.TermVariant -> Typed.TypedTerm ErrorCore.UnexpectedTermVariantError
unexpectedTermVariantErrorWithExpectedVariant original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UnexpectedTermVariantError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expectedVariant"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "actualTerm"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UnexpectedTermVariantError"),
              Core.projectionFieldName = (Core.Name "actualTerm")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.error.core.UnexpectedTypeVariantError
unexpectedTypeVariantError :: Typed.TypedTerm Variants.TypeVariant -> Typed.TypedTerm Core.Type -> Typed.TypedTerm ErrorCore.UnexpectedTypeVariantError
unexpectedTypeVariantError expectedVariant actualType =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UnexpectedTypeVariantError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expectedVariant"),
          Core.fieldTerm = (Typed.unTypedTerm expectedVariant)},
        Core.Field {
          Core.fieldName = (Core.Name "actualType"),
          Core.fieldTerm = (Typed.unTypedTerm actualType)}]}))
-- | DSL accessor for the actualType field of hydra.error.core.UnexpectedTypeVariantError
unexpectedTypeVariantErrorActualType :: Typed.TypedTerm ErrorCore.UnexpectedTypeVariantError -> Typed.TypedTerm Core.Type
unexpectedTypeVariantErrorActualType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UnexpectedTypeVariantError"),
        Core.projectionFieldName = (Core.Name "actualType")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the expectedVariant field of hydra.error.core.UnexpectedTypeVariantError
unexpectedTypeVariantErrorExpectedVariant :: Typed.TypedTerm ErrorCore.UnexpectedTypeVariantError -> Typed.TypedTerm Variants.TypeVariant
unexpectedTypeVariantErrorExpectedVariant x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UnexpectedTypeVariantError"),
        Core.projectionFieldName = (Core.Name "expectedVariant")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the actualType field of hydra.error.core.UnexpectedTypeVariantError
unexpectedTypeVariantErrorWithActualType :: Typed.TypedTerm ErrorCore.UnexpectedTypeVariantError -> Typed.TypedTerm Core.Type -> Typed.TypedTerm ErrorCore.UnexpectedTypeVariantError
unexpectedTypeVariantErrorWithActualType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UnexpectedTypeVariantError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expectedVariant"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UnexpectedTypeVariantError"),
              Core.projectionFieldName = (Core.Name "expectedVariant")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "actualType"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the expectedVariant field of hydra.error.core.UnexpectedTypeVariantError
unexpectedTypeVariantErrorWithExpectedVariant :: Typed.TypedTerm ErrorCore.UnexpectedTypeVariantError -> Typed.TypedTerm Variants.TypeVariant -> Typed.TypedTerm ErrorCore.UnexpectedTypeVariantError
unexpectedTypeVariantErrorWithExpectedVariant original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UnexpectedTypeVariantError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expectedVariant"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "actualType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UnexpectedTypeVariantError"),
              Core.projectionFieldName = (Core.Name "actualType")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.error.core.UnknownPrimitiveNameError
unknownPrimitiveNameError :: Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.UnknownPrimitiveNameError
unknownPrimitiveNameError location name =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UnknownPrimitiveNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)}]}))
-- | DSL accessor for the location field of hydra.error.core.UnknownPrimitiveNameError
unknownPrimitiveNameErrorLocation :: Typed.TypedTerm ErrorCore.UnknownPrimitiveNameError -> Typed.TypedTerm Paths.SubtermPath
unknownPrimitiveNameErrorLocation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UnknownPrimitiveNameError"),
        Core.projectionFieldName = (Core.Name "location")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.error.core.UnknownPrimitiveNameError
unknownPrimitiveNameErrorName :: Typed.TypedTerm ErrorCore.UnknownPrimitiveNameError -> Typed.TypedTerm Core.Name
unknownPrimitiveNameErrorName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UnknownPrimitiveNameError"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the location field of hydra.error.core.UnknownPrimitiveNameError
unknownPrimitiveNameErrorWithLocation :: Typed.TypedTerm ErrorCore.UnknownPrimitiveNameError -> Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.UnknownPrimitiveNameError
unknownPrimitiveNameErrorWithLocation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UnknownPrimitiveNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UnknownPrimitiveNameError"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.error.core.UnknownPrimitiveNameError
unknownPrimitiveNameErrorWithName :: Typed.TypedTerm ErrorCore.UnknownPrimitiveNameError -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.UnknownPrimitiveNameError
unknownPrimitiveNameErrorWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UnknownPrimitiveNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UnknownPrimitiveNameError"),
              Core.projectionFieldName = (Core.Name "location")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.error.core.UnnecessaryIdentityApplicationError
unnecessaryIdentityApplicationError :: Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.UnnecessaryIdentityApplicationError
unnecessaryIdentityApplicationError location =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UnnecessaryIdentityApplicationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm location)}]}))
-- | DSL accessor for the location field of hydra.error.core.UnnecessaryIdentityApplicationError
unnecessaryIdentityApplicationErrorLocation :: Typed.TypedTerm ErrorCore.UnnecessaryIdentityApplicationError -> Typed.TypedTerm Paths.SubtermPath
unnecessaryIdentityApplicationErrorLocation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UnnecessaryIdentityApplicationError"),
        Core.projectionFieldName = (Core.Name "location")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the location field of hydra.error.core.UnnecessaryIdentityApplicationError
unnecessaryIdentityApplicationErrorWithLocation :: Typed.TypedTerm ErrorCore.UnnecessaryIdentityApplicationError -> Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.UnnecessaryIdentityApplicationError
unnecessaryIdentityApplicationErrorWithLocation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UnnecessaryIdentityApplicationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.error.core.UntypedTermVariableError
untypedTermVariableError :: Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.UntypedTermVariableError
untypedTermVariableError location name =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UntypedTermVariableError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm location)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)}]}))
-- | DSL accessor for the location field of hydra.error.core.UntypedTermVariableError
untypedTermVariableErrorLocation :: Typed.TypedTerm ErrorCore.UntypedTermVariableError -> Typed.TypedTerm Paths.SubtermPath
untypedTermVariableErrorLocation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UntypedTermVariableError"),
        Core.projectionFieldName = (Core.Name "location")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.error.core.UntypedTermVariableError
untypedTermVariableErrorName :: Typed.TypedTerm ErrorCore.UntypedTermVariableError -> Typed.TypedTerm Core.Name
untypedTermVariableErrorName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.UntypedTermVariableError"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the location field of hydra.error.core.UntypedTermVariableError
untypedTermVariableErrorWithLocation :: Typed.TypedTerm ErrorCore.UntypedTermVariableError -> Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.UntypedTermVariableError
untypedTermVariableErrorWithLocation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UntypedTermVariableError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UntypedTermVariableError"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.error.core.UntypedTermVariableError
untypedTermVariableErrorWithName :: Typed.TypedTerm ErrorCore.UntypedTermVariableError -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorCore.UntypedTermVariableError
untypedTermVariableErrorWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.UntypedTermVariableError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.core.UntypedTermVariableError"),
              Core.projectionFieldName = (Core.Name "location")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.error.core.VoidInNonBottomPositionError
voidInNonBottomPositionError :: Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.VoidInNonBottomPositionError
voidInNonBottomPositionError location =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.VoidInNonBottomPositionError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm location)}]}))
-- | DSL accessor for the location field of hydra.error.core.VoidInNonBottomPositionError
voidInNonBottomPositionErrorLocation :: Typed.TypedTerm ErrorCore.VoidInNonBottomPositionError -> Typed.TypedTerm Paths.SubtermPath
voidInNonBottomPositionErrorLocation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.core.VoidInNonBottomPositionError"),
        Core.projectionFieldName = (Core.Name "location")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the location field of hydra.error.core.VoidInNonBottomPositionError
voidInNonBottomPositionErrorWithLocation :: Typed.TypedTerm ErrorCore.VoidInNonBottomPositionError -> Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm ErrorCore.VoidInNonBottomPositionError
voidInNonBottomPositionErrorWithLocation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.core.VoidInNonBottomPositionError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "location"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
