-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.error.packaging

module Hydra.Dsl.Error.Packaging where
import qualified Hydra.Core as Core
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Dsl.Packaging as DslPackaging
import qualified Hydra.Dsl.Util as DslUtil
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Typed as Typed
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | DSL constructor for hydra.error.packaging.ConflictingModuleNameError
conflictingModuleNameError :: Typed.TypedTerm Packaging.ModuleName -> Typed.TypedTerm Packaging.ModuleName -> Typed.TypedTerm ErrorPackaging.ConflictingModuleNameError
conflictingModuleNameError first second =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.ConflictingModuleNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Typed.unTypedTerm first)},
        Core.Field {
          Core.fieldName = (Core.Name "second"),
          Core.fieldTerm = (Typed.unTypedTerm second)}]}))
-- | DSL accessor for the first field of hydra.error.packaging.ConflictingModuleNameError
conflictingModuleNameErrorFirst :: Typed.TypedTerm ErrorPackaging.ConflictingModuleNameError -> Typed.TypedTerm Packaging.ModuleName
conflictingModuleNameErrorFirst x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingModuleNameError"),
        Core.projectionFieldName = (Core.Name "first")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the second field of hydra.error.packaging.ConflictingModuleNameError
conflictingModuleNameErrorSecond :: Typed.TypedTerm ErrorPackaging.ConflictingModuleNameError -> Typed.TypedTerm Packaging.ModuleName
conflictingModuleNameErrorSecond x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingModuleNameError"),
        Core.projectionFieldName = (Core.Name "second")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the first field of hydra.error.packaging.ConflictingModuleNameError
conflictingModuleNameErrorWithFirst :: Typed.TypedTerm ErrorPackaging.ConflictingModuleNameError -> Typed.TypedTerm Packaging.ModuleName -> Typed.TypedTerm ErrorPackaging.ConflictingModuleNameError
conflictingModuleNameErrorWithFirst original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.ConflictingModuleNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "second"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingModuleNameError"),
              Core.projectionFieldName = (Core.Name "second")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the second field of hydra.error.packaging.ConflictingModuleNameError
conflictingModuleNameErrorWithSecond :: Typed.TypedTerm ErrorPackaging.ConflictingModuleNameError -> Typed.TypedTerm Packaging.ModuleName -> Typed.TypedTerm ErrorPackaging.ConflictingModuleNameError
conflictingModuleNameErrorWithSecond original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.ConflictingModuleNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingModuleNameError"),
              Core.projectionFieldName = (Core.Name "first")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "second"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.error.packaging.ConflictingVariantNameError
conflictingVariantNameError :: Typed.TypedTerm Packaging.ModuleName -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorPackaging.ConflictingVariantNameError
conflictingVariantNameError moduleName typeName variantName conflictingName =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Typed.unTypedTerm moduleName)},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Typed.unTypedTerm typeName)},
        Core.Field {
          Core.fieldName = (Core.Name "variantName"),
          Core.fieldTerm = (Typed.unTypedTerm variantName)},
        Core.Field {
          Core.fieldName = (Core.Name "conflictingName"),
          Core.fieldTerm = (Typed.unTypedTerm conflictingName)}]}))
-- | DSL accessor for the conflictingName field of hydra.error.packaging.ConflictingVariantNameError
conflictingVariantNameErrorConflictingName :: Typed.TypedTerm ErrorPackaging.ConflictingVariantNameError -> Typed.TypedTerm Core.Name
conflictingVariantNameErrorConflictingName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
        Core.projectionFieldName = (Core.Name "conflictingName")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the moduleName field of hydra.error.packaging.ConflictingVariantNameError
conflictingVariantNameErrorModuleName :: Typed.TypedTerm ErrorPackaging.ConflictingVariantNameError -> Typed.TypedTerm Packaging.ModuleName
conflictingVariantNameErrorModuleName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
        Core.projectionFieldName = (Core.Name "moduleName")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeName field of hydra.error.packaging.ConflictingVariantNameError
conflictingVariantNameErrorTypeName :: Typed.TypedTerm ErrorPackaging.ConflictingVariantNameError -> Typed.TypedTerm Core.Name
conflictingVariantNameErrorTypeName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
        Core.projectionFieldName = (Core.Name "typeName")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the variantName field of hydra.error.packaging.ConflictingVariantNameError
conflictingVariantNameErrorVariantName :: Typed.TypedTerm ErrorPackaging.ConflictingVariantNameError -> Typed.TypedTerm Core.Name
conflictingVariantNameErrorVariantName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
        Core.projectionFieldName = (Core.Name "variantName")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the conflictingName field of hydra.error.packaging.ConflictingVariantNameError
conflictingVariantNameErrorWithConflictingName :: Typed.TypedTerm ErrorPackaging.ConflictingVariantNameError -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorPackaging.ConflictingVariantNameError
conflictingVariantNameErrorWithConflictingName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
              Core.projectionFieldName = (Core.Name "moduleName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
              Core.projectionFieldName = (Core.Name "typeName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variantName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
              Core.projectionFieldName = (Core.Name "variantName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "conflictingName"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the moduleName field of hydra.error.packaging.ConflictingVariantNameError
conflictingVariantNameErrorWithModuleName :: Typed.TypedTerm ErrorPackaging.ConflictingVariantNameError -> Typed.TypedTerm Packaging.ModuleName -> Typed.TypedTerm ErrorPackaging.ConflictingVariantNameError
conflictingVariantNameErrorWithModuleName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
              Core.projectionFieldName = (Core.Name "typeName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variantName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
              Core.projectionFieldName = (Core.Name "variantName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "conflictingName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
              Core.projectionFieldName = (Core.Name "conflictingName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the typeName field of hydra.error.packaging.ConflictingVariantNameError
conflictingVariantNameErrorWithTypeName :: Typed.TypedTerm ErrorPackaging.ConflictingVariantNameError -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorPackaging.ConflictingVariantNameError
conflictingVariantNameErrorWithTypeName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
              Core.projectionFieldName = (Core.Name "moduleName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "variantName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
              Core.projectionFieldName = (Core.Name "variantName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "conflictingName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
              Core.projectionFieldName = (Core.Name "conflictingName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the variantName field of hydra.error.packaging.ConflictingVariantNameError
conflictingVariantNameErrorWithVariantName :: Typed.TypedTerm ErrorPackaging.ConflictingVariantNameError -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorPackaging.ConflictingVariantNameError
conflictingVariantNameErrorWithVariantName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
              Core.projectionFieldName = (Core.Name "moduleName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
              Core.projectionFieldName = (Core.Name "typeName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variantName"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "conflictingName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
              Core.projectionFieldName = (Core.Name "conflictingName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.error.packaging.DefinitionNotInModuleNameError
definitionNotInModuleNameError :: Typed.TypedTerm Packaging.ModuleName -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorPackaging.DefinitionNotInModuleNameError
definitionNotInModuleNameError moduleName name =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.DefinitionNotInModuleNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Typed.unTypedTerm moduleName)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)}]}))
-- | DSL accessor for the moduleName field of hydra.error.packaging.DefinitionNotInModuleNameError
definitionNotInModuleNameErrorModuleName :: Typed.TypedTerm ErrorPackaging.DefinitionNotInModuleNameError -> Typed.TypedTerm Packaging.ModuleName
definitionNotInModuleNameErrorModuleName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.DefinitionNotInModuleNameError"),
        Core.projectionFieldName = (Core.Name "moduleName")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.error.packaging.DefinitionNotInModuleNameError
definitionNotInModuleNameErrorName :: Typed.TypedTerm ErrorPackaging.DefinitionNotInModuleNameError -> Typed.TypedTerm Core.Name
definitionNotInModuleNameErrorName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.DefinitionNotInModuleNameError"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the moduleName field of hydra.error.packaging.DefinitionNotInModuleNameError
definitionNotInModuleNameErrorWithModuleName :: Typed.TypedTerm ErrorPackaging.DefinitionNotInModuleNameError -> Typed.TypedTerm Packaging.ModuleName -> Typed.TypedTerm ErrorPackaging.DefinitionNotInModuleNameError
definitionNotInModuleNameErrorWithModuleName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.DefinitionNotInModuleNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.DefinitionNotInModuleNameError"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.error.packaging.DefinitionNotInModuleNameError
definitionNotInModuleNameErrorWithName :: Typed.TypedTerm ErrorPackaging.DefinitionNotInModuleNameError -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorPackaging.DefinitionNotInModuleNameError
definitionNotInModuleNameErrorWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.DefinitionNotInModuleNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.DefinitionNotInModuleNameError"),
              Core.projectionFieldName = (Core.Name "moduleName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.error.packaging.DefinitionsOutOfOrderError
definitionsOutOfOrderError :: Typed.TypedTerm Packaging.ModuleName -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorPackaging.DefinitionsOutOfOrderError
definitionsOutOfOrderError moduleName precedingName followingName =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.DefinitionsOutOfOrderError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Typed.unTypedTerm moduleName)},
        Core.Field {
          Core.fieldName = (Core.Name "precedingName"),
          Core.fieldTerm = (Typed.unTypedTerm precedingName)},
        Core.Field {
          Core.fieldName = (Core.Name "followingName"),
          Core.fieldTerm = (Typed.unTypedTerm followingName)}]}))
-- | DSL accessor for the followingName field of hydra.error.packaging.DefinitionsOutOfOrderError
definitionsOutOfOrderErrorFollowingName :: Typed.TypedTerm ErrorPackaging.DefinitionsOutOfOrderError -> Typed.TypedTerm Core.Name
definitionsOutOfOrderErrorFollowingName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.DefinitionsOutOfOrderError"),
        Core.projectionFieldName = (Core.Name "followingName")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the moduleName field of hydra.error.packaging.DefinitionsOutOfOrderError
definitionsOutOfOrderErrorModuleName :: Typed.TypedTerm ErrorPackaging.DefinitionsOutOfOrderError -> Typed.TypedTerm Packaging.ModuleName
definitionsOutOfOrderErrorModuleName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.DefinitionsOutOfOrderError"),
        Core.projectionFieldName = (Core.Name "moduleName")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the precedingName field of hydra.error.packaging.DefinitionsOutOfOrderError
definitionsOutOfOrderErrorPrecedingName :: Typed.TypedTerm ErrorPackaging.DefinitionsOutOfOrderError -> Typed.TypedTerm Core.Name
definitionsOutOfOrderErrorPrecedingName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.DefinitionsOutOfOrderError"),
        Core.projectionFieldName = (Core.Name "precedingName")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the followingName field of hydra.error.packaging.DefinitionsOutOfOrderError
definitionsOutOfOrderErrorWithFollowingName :: Typed.TypedTerm ErrorPackaging.DefinitionsOutOfOrderError -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorPackaging.DefinitionsOutOfOrderError
definitionsOutOfOrderErrorWithFollowingName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.DefinitionsOutOfOrderError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.DefinitionsOutOfOrderError"),
              Core.projectionFieldName = (Core.Name "moduleName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "precedingName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.DefinitionsOutOfOrderError"),
              Core.projectionFieldName = (Core.Name "precedingName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "followingName"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the moduleName field of hydra.error.packaging.DefinitionsOutOfOrderError
definitionsOutOfOrderErrorWithModuleName :: Typed.TypedTerm ErrorPackaging.DefinitionsOutOfOrderError -> Typed.TypedTerm Packaging.ModuleName -> Typed.TypedTerm ErrorPackaging.DefinitionsOutOfOrderError
definitionsOutOfOrderErrorWithModuleName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.DefinitionsOutOfOrderError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "precedingName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.DefinitionsOutOfOrderError"),
              Core.projectionFieldName = (Core.Name "precedingName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "followingName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.DefinitionsOutOfOrderError"),
              Core.projectionFieldName = (Core.Name "followingName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the precedingName field of hydra.error.packaging.DefinitionsOutOfOrderError
definitionsOutOfOrderErrorWithPrecedingName :: Typed.TypedTerm ErrorPackaging.DefinitionsOutOfOrderError -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorPackaging.DefinitionsOutOfOrderError
definitionsOutOfOrderErrorWithPrecedingName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.DefinitionsOutOfOrderError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.DefinitionsOutOfOrderError"),
              Core.projectionFieldName = (Core.Name "moduleName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "precedingName"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "followingName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.DefinitionsOutOfOrderError"),
              Core.projectionFieldName = (Core.Name "followingName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.error.packaging.DuplicateDefinitionNameError
duplicateDefinitionNameError :: Typed.TypedTerm Packaging.ModuleName -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorPackaging.DuplicateDefinitionNameError
duplicateDefinitionNameError moduleName name =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.DuplicateDefinitionNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Typed.unTypedTerm moduleName)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)}]}))
-- | DSL accessor for the moduleName field of hydra.error.packaging.DuplicateDefinitionNameError
duplicateDefinitionNameErrorModuleName :: Typed.TypedTerm ErrorPackaging.DuplicateDefinitionNameError -> Typed.TypedTerm Packaging.ModuleName
duplicateDefinitionNameErrorModuleName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.DuplicateDefinitionNameError"),
        Core.projectionFieldName = (Core.Name "moduleName")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.error.packaging.DuplicateDefinitionNameError
duplicateDefinitionNameErrorName :: Typed.TypedTerm ErrorPackaging.DuplicateDefinitionNameError -> Typed.TypedTerm Core.Name
duplicateDefinitionNameErrorName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.DuplicateDefinitionNameError"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the moduleName field of hydra.error.packaging.DuplicateDefinitionNameError
duplicateDefinitionNameErrorWithModuleName :: Typed.TypedTerm ErrorPackaging.DuplicateDefinitionNameError -> Typed.TypedTerm Packaging.ModuleName -> Typed.TypedTerm ErrorPackaging.DuplicateDefinitionNameError
duplicateDefinitionNameErrorWithModuleName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.DuplicateDefinitionNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.DuplicateDefinitionNameError"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.error.packaging.DuplicateDefinitionNameError
duplicateDefinitionNameErrorWithName :: Typed.TypedTerm ErrorPackaging.DuplicateDefinitionNameError -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorPackaging.DuplicateDefinitionNameError
duplicateDefinitionNameErrorWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.DuplicateDefinitionNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.DuplicateDefinitionNameError"),
              Core.projectionFieldName = (Core.Name "moduleName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.error.packaging.DuplicateModuleNameError
duplicateModuleNameError :: Typed.TypedTerm Packaging.ModuleName -> Typed.TypedTerm ErrorPackaging.DuplicateModuleNameError
duplicateModuleNameError moduleName =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.DuplicateModuleNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Typed.unTypedTerm moduleName)}]}))
-- | DSL accessor for the moduleName field of hydra.error.packaging.DuplicateModuleNameError
duplicateModuleNameErrorModuleName :: Typed.TypedTerm ErrorPackaging.DuplicateModuleNameError -> Typed.TypedTerm Packaging.ModuleName
duplicateModuleNameErrorModuleName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.DuplicateModuleNameError"),
        Core.projectionFieldName = (Core.Name "moduleName")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the moduleName field of hydra.error.packaging.DuplicateModuleNameError
duplicateModuleNameErrorWithModuleName :: Typed.TypedTerm ErrorPackaging.DuplicateModuleNameError -> Typed.TypedTerm Packaging.ModuleName -> Typed.TypedTerm ErrorPackaging.DuplicateModuleNameError
duplicateModuleNameErrorWithModuleName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.DuplicateModuleNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.error.packaging.InvalidDefinitionNameError
invalidDefinitionNameError :: Typed.TypedTerm Packaging.ModuleName -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Util.CaseConvention -> Typed.TypedTerm ErrorPackaging.InvalidDefinitionNameError
invalidDefinitionNameError moduleName name expectedConvention =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.InvalidDefinitionNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Typed.unTypedTerm moduleName)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "expectedConvention"),
          Core.fieldTerm = (Typed.unTypedTerm expectedConvention)}]}))
-- | DSL accessor for the expectedConvention field of hydra.error.packaging.InvalidDefinitionNameError
invalidDefinitionNameErrorExpectedConvention :: Typed.TypedTerm ErrorPackaging.InvalidDefinitionNameError -> Typed.TypedTerm Util.CaseConvention
invalidDefinitionNameErrorExpectedConvention x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.InvalidDefinitionNameError"),
        Core.projectionFieldName = (Core.Name "expectedConvention")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the moduleName field of hydra.error.packaging.InvalidDefinitionNameError
invalidDefinitionNameErrorModuleName :: Typed.TypedTerm ErrorPackaging.InvalidDefinitionNameError -> Typed.TypedTerm Packaging.ModuleName
invalidDefinitionNameErrorModuleName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.InvalidDefinitionNameError"),
        Core.projectionFieldName = (Core.Name "moduleName")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.error.packaging.InvalidDefinitionNameError
invalidDefinitionNameErrorName :: Typed.TypedTerm ErrorPackaging.InvalidDefinitionNameError -> Typed.TypedTerm Core.Name
invalidDefinitionNameErrorName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.InvalidDefinitionNameError"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expectedConvention field of hydra.error.packaging.InvalidDefinitionNameError
invalidDefinitionNameErrorWithExpectedConvention :: Typed.TypedTerm ErrorPackaging.InvalidDefinitionNameError -> Typed.TypedTerm Util.CaseConvention -> Typed.TypedTerm ErrorPackaging.InvalidDefinitionNameError
invalidDefinitionNameErrorWithExpectedConvention original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.InvalidDefinitionNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.InvalidDefinitionNameError"),
              Core.projectionFieldName = (Core.Name "moduleName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.InvalidDefinitionNameError"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expectedConvention"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the moduleName field of hydra.error.packaging.InvalidDefinitionNameError
invalidDefinitionNameErrorWithModuleName :: Typed.TypedTerm ErrorPackaging.InvalidDefinitionNameError -> Typed.TypedTerm Packaging.ModuleName -> Typed.TypedTerm ErrorPackaging.InvalidDefinitionNameError
invalidDefinitionNameErrorWithModuleName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.InvalidDefinitionNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.InvalidDefinitionNameError"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expectedConvention"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.InvalidDefinitionNameError"),
              Core.projectionFieldName = (Core.Name "expectedConvention")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.error.packaging.InvalidDefinitionNameError
invalidDefinitionNameErrorWithName :: Typed.TypedTerm ErrorPackaging.InvalidDefinitionNameError -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorPackaging.InvalidDefinitionNameError
invalidDefinitionNameErrorWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.InvalidDefinitionNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.InvalidDefinitionNameError"),
              Core.projectionFieldName = (Core.Name "moduleName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expectedConvention"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.InvalidDefinitionNameError"),
              Core.projectionFieldName = (Core.Name "expectedConvention")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the conflictingVariantName variant of hydra.error.packaging.InvalidModuleError
invalidModuleErrorConflictingVariantName :: Typed.TypedTerm ErrorPackaging.ConflictingVariantNameError -> Typed.TypedTerm ErrorPackaging.InvalidModuleError
invalidModuleErrorConflictingVariantName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidModuleError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "conflictingVariantName"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the definitionNotInModuleName variant of hydra.error.packaging.InvalidModuleError
invalidModuleErrorDefinitionNotInModuleName :: Typed.TypedTerm ErrorPackaging.DefinitionNotInModuleNameError -> Typed.TypedTerm ErrorPackaging.InvalidModuleError
invalidModuleErrorDefinitionNotInModuleName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidModuleError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "definitionNotInModuleName"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the definitionsOutOfOrder variant of hydra.error.packaging.InvalidModuleError
invalidModuleErrorDefinitionsOutOfOrder :: Typed.TypedTerm ErrorPackaging.DefinitionsOutOfOrderError -> Typed.TypedTerm ErrorPackaging.InvalidModuleError
invalidModuleErrorDefinitionsOutOfOrder x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidModuleError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "definitionsOutOfOrder"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the duplicateDefinitionName variant of hydra.error.packaging.InvalidModuleError
invalidModuleErrorDuplicateDefinitionName :: Typed.TypedTerm ErrorPackaging.DuplicateDefinitionNameError -> Typed.TypedTerm ErrorPackaging.InvalidModuleError
invalidModuleErrorDuplicateDefinitionName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidModuleError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "duplicateDefinitionName"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the invalidDefinitionName variant of hydra.error.packaging.InvalidModuleError
invalidModuleErrorInvalidDefinitionName :: Typed.TypedTerm ErrorPackaging.InvalidDefinitionNameError -> Typed.TypedTerm ErrorPackaging.InvalidModuleError
invalidModuleErrorInvalidDefinitionName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidModuleError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "invalidDefinitionName"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the invalidModuleNameConvention variant of hydra.error.packaging.InvalidModuleError
invalidModuleErrorInvalidModuleNameConvention :: Typed.TypedTerm ErrorPackaging.InvalidModuleNameConventionError -> Typed.TypedTerm ErrorPackaging.InvalidModuleError
invalidModuleErrorInvalidModuleNameConvention x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidModuleError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "invalidModuleNameConvention"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the missingDocumentation variant of hydra.error.packaging.InvalidModuleError
invalidModuleErrorMissingDocumentation :: Typed.TypedTerm ErrorPackaging.MissingDocumentationError -> Typed.TypedTerm ErrorPackaging.InvalidModuleError
invalidModuleErrorMissingDocumentation x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidModuleError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "missingDocumentation"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.error.packaging.InvalidModuleNameConventionError
invalidModuleNameConventionError :: Typed.TypedTerm Packaging.ModuleName -> Typed.TypedTerm ErrorPackaging.InvalidModuleNameConventionError
invalidModuleNameConventionError moduleName =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.InvalidModuleNameConventionError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Typed.unTypedTerm moduleName)}]}))
-- | DSL accessor for the moduleName field of hydra.error.packaging.InvalidModuleNameConventionError
invalidModuleNameConventionErrorModuleName :: Typed.TypedTerm ErrorPackaging.InvalidModuleNameConventionError -> Typed.TypedTerm Packaging.ModuleName
invalidModuleNameConventionErrorModuleName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.InvalidModuleNameConventionError"),
        Core.projectionFieldName = (Core.Name "moduleName")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the moduleName field of hydra.error.packaging.InvalidModuleNameConventionError
invalidModuleNameConventionErrorWithModuleName :: Typed.TypedTerm ErrorPackaging.InvalidModuleNameConventionError -> Typed.TypedTerm Packaging.ModuleName -> Typed.TypedTerm ErrorPackaging.InvalidModuleNameConventionError
invalidModuleNameConventionErrorWithModuleName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.InvalidModuleNameConventionError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the conflictingModuleName variant of hydra.error.packaging.InvalidPackageError
invalidPackageErrorConflictingModuleName :: Typed.TypedTerm ErrorPackaging.ConflictingModuleNameError -> Typed.TypedTerm ErrorPackaging.InvalidPackageError
invalidPackageErrorConflictingModuleName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidPackageError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "conflictingModuleName"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the duplicateModuleName variant of hydra.error.packaging.InvalidPackageError
invalidPackageErrorDuplicateModuleName :: Typed.TypedTerm ErrorPackaging.DuplicateModuleNameError -> Typed.TypedTerm ErrorPackaging.InvalidPackageError
invalidPackageErrorDuplicateModuleName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidPackageError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "duplicateModuleName"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the invalidModule variant of hydra.error.packaging.InvalidPackageError
invalidPackageErrorInvalidModule :: Typed.TypedTerm ErrorPackaging.InvalidModuleError -> Typed.TypedTerm ErrorPackaging.InvalidPackageError
invalidPackageErrorInvalidModule x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidPackageError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "invalidModule"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the invalidPackageName variant of hydra.error.packaging.InvalidPackageError
invalidPackageErrorInvalidPackageName :: Typed.TypedTerm ErrorPackaging.InvalidPackageNameError -> Typed.TypedTerm ErrorPackaging.InvalidPackageError
invalidPackageErrorInvalidPackageName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidPackageError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "invalidPackageName"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.error.packaging.InvalidPackageNameError
invalidPackageNameError :: Typed.TypedTerm Packaging.PackageName -> Typed.TypedTerm ErrorPackaging.InvalidPackageNameError
invalidPackageNameError packageName =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.InvalidPackageNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "packageName"),
          Core.fieldTerm = (Typed.unTypedTerm packageName)}]}))
-- | DSL accessor for the packageName field of hydra.error.packaging.InvalidPackageNameError
invalidPackageNameErrorPackageName :: Typed.TypedTerm ErrorPackaging.InvalidPackageNameError -> Typed.TypedTerm Packaging.PackageName
invalidPackageNameErrorPackageName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.InvalidPackageNameError"),
        Core.projectionFieldName = (Core.Name "packageName")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the packageName field of hydra.error.packaging.InvalidPackageNameError
invalidPackageNameErrorWithPackageName :: Typed.TypedTerm ErrorPackaging.InvalidPackageNameError -> Typed.TypedTerm Packaging.PackageName -> Typed.TypedTerm ErrorPackaging.InvalidPackageNameError
invalidPackageNameErrorWithPackageName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.InvalidPackageNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "packageName"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.error.packaging.MissingDocumentationError
missingDocumentationError :: Typed.TypedTerm Packaging.ModuleName -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorPackaging.MissingDocumentationError
missingDocumentationError moduleName name =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.MissingDocumentationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Typed.unTypedTerm moduleName)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)}]}))
-- | DSL accessor for the moduleName field of hydra.error.packaging.MissingDocumentationError
missingDocumentationErrorModuleName :: Typed.TypedTerm ErrorPackaging.MissingDocumentationError -> Typed.TypedTerm Packaging.ModuleName
missingDocumentationErrorModuleName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.MissingDocumentationError"),
        Core.projectionFieldName = (Core.Name "moduleName")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.error.packaging.MissingDocumentationError
missingDocumentationErrorName :: Typed.TypedTerm ErrorPackaging.MissingDocumentationError -> Typed.TypedTerm Core.Name
missingDocumentationErrorName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.MissingDocumentationError"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the moduleName field of hydra.error.packaging.MissingDocumentationError
missingDocumentationErrorWithModuleName :: Typed.TypedTerm ErrorPackaging.MissingDocumentationError -> Typed.TypedTerm Packaging.ModuleName -> Typed.TypedTerm ErrorPackaging.MissingDocumentationError
missingDocumentationErrorWithModuleName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.MissingDocumentationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.MissingDocumentationError"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.error.packaging.MissingDocumentationError
missingDocumentationErrorWithName :: Typed.TypedTerm ErrorPackaging.MissingDocumentationError -> Typed.TypedTerm Core.Name -> Typed.TypedTerm ErrorPackaging.MissingDocumentationError
missingDocumentationErrorWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.MissingDocumentationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.MissingDocumentationError"),
              Core.projectionFieldName = (Core.Name "moduleName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
