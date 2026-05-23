-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.error.packaging

module Hydra.Dsl.Error.Packaging where
import qualified Hydra.Core as Core
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | DSL constructor for hydra.error.packaging.ConflictingModuleNameError
conflictingModuleNameError :: Phantoms.TTerm Packaging.ModuleName -> Phantoms.TTerm Packaging.ModuleName -> Phantoms.TTerm ErrorPackaging.ConflictingModuleNameError
conflictingModuleNameError first second =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.ConflictingModuleNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Phantoms.unTTerm first)},
        Core.Field {
          Core.fieldName = (Core.Name "second"),
          Core.fieldTerm = (Phantoms.unTTerm second)}]}))
-- | DSL accessor for the first field of hydra.error.packaging.ConflictingModuleNameError
conflictingModuleNameErrorFirst :: Phantoms.TTerm ErrorPackaging.ConflictingModuleNameError -> Phantoms.TTerm Packaging.ModuleName
conflictingModuleNameErrorFirst x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingModuleNameError"),
        Core.projectionFieldName = (Core.Name "first")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the second field of hydra.error.packaging.ConflictingModuleNameError
conflictingModuleNameErrorSecond :: Phantoms.TTerm ErrorPackaging.ConflictingModuleNameError -> Phantoms.TTerm Packaging.ModuleName
conflictingModuleNameErrorSecond x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingModuleNameError"),
        Core.projectionFieldName = (Core.Name "second")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the first field of hydra.error.packaging.ConflictingModuleNameError
conflictingModuleNameErrorWithFirst :: Phantoms.TTerm ErrorPackaging.ConflictingModuleNameError -> Phantoms.TTerm Packaging.ModuleName -> Phantoms.TTerm ErrorPackaging.ConflictingModuleNameError
conflictingModuleNameErrorWithFirst original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.ConflictingModuleNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "second"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingModuleNameError"),
              Core.projectionFieldName = (Core.Name "second")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the second field of hydra.error.packaging.ConflictingModuleNameError
conflictingModuleNameErrorWithSecond :: Phantoms.TTerm ErrorPackaging.ConflictingModuleNameError -> Phantoms.TTerm Packaging.ModuleName -> Phantoms.TTerm ErrorPackaging.ConflictingModuleNameError
conflictingModuleNameErrorWithSecond original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.ConflictingModuleNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingModuleNameError"),
              Core.projectionFieldName = (Core.Name "first")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "second"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.error.packaging.ConflictingVariantNameError
conflictingVariantNameError :: Phantoms.TTerm Packaging.ModuleName -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorPackaging.ConflictingVariantNameError
conflictingVariantNameError moduleName typeName variantName conflictingName =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Phantoms.unTTerm moduleName)},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Phantoms.unTTerm typeName)},
        Core.Field {
          Core.fieldName = (Core.Name "variantName"),
          Core.fieldTerm = (Phantoms.unTTerm variantName)},
        Core.Field {
          Core.fieldName = (Core.Name "conflictingName"),
          Core.fieldTerm = (Phantoms.unTTerm conflictingName)}]}))
-- | DSL accessor for the conflictingName field of hydra.error.packaging.ConflictingVariantNameError
conflictingVariantNameErrorConflictingName :: Phantoms.TTerm ErrorPackaging.ConflictingVariantNameError -> Phantoms.TTerm Core.Name
conflictingVariantNameErrorConflictingName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
        Core.projectionFieldName = (Core.Name "conflictingName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the moduleName field of hydra.error.packaging.ConflictingVariantNameError
conflictingVariantNameErrorModuleName :: Phantoms.TTerm ErrorPackaging.ConflictingVariantNameError -> Phantoms.TTerm Packaging.ModuleName
conflictingVariantNameErrorModuleName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
        Core.projectionFieldName = (Core.Name "moduleName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typeName field of hydra.error.packaging.ConflictingVariantNameError
conflictingVariantNameErrorTypeName :: Phantoms.TTerm ErrorPackaging.ConflictingVariantNameError -> Phantoms.TTerm Core.Name
conflictingVariantNameErrorTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
        Core.projectionFieldName = (Core.Name "typeName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the variantName field of hydra.error.packaging.ConflictingVariantNameError
conflictingVariantNameErrorVariantName :: Phantoms.TTerm ErrorPackaging.ConflictingVariantNameError -> Phantoms.TTerm Core.Name
conflictingVariantNameErrorVariantName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
        Core.projectionFieldName = (Core.Name "variantName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the conflictingName field of hydra.error.packaging.ConflictingVariantNameError
conflictingVariantNameErrorWithConflictingName :: Phantoms.TTerm ErrorPackaging.ConflictingVariantNameError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorPackaging.ConflictingVariantNameError
conflictingVariantNameErrorWithConflictingName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
              Core.projectionFieldName = (Core.Name "moduleName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
              Core.projectionFieldName = (Core.Name "typeName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variantName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
              Core.projectionFieldName = (Core.Name "variantName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "conflictingName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the moduleName field of hydra.error.packaging.ConflictingVariantNameError
conflictingVariantNameErrorWithModuleName :: Phantoms.TTerm ErrorPackaging.ConflictingVariantNameError -> Phantoms.TTerm Packaging.ModuleName -> Phantoms.TTerm ErrorPackaging.ConflictingVariantNameError
conflictingVariantNameErrorWithModuleName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
              Core.projectionFieldName = (Core.Name "typeName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variantName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
              Core.projectionFieldName = (Core.Name "variantName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "conflictingName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
              Core.projectionFieldName = (Core.Name "conflictingName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the typeName field of hydra.error.packaging.ConflictingVariantNameError
conflictingVariantNameErrorWithTypeName :: Phantoms.TTerm ErrorPackaging.ConflictingVariantNameError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorPackaging.ConflictingVariantNameError
conflictingVariantNameErrorWithTypeName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
              Core.projectionFieldName = (Core.Name "moduleName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "variantName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
              Core.projectionFieldName = (Core.Name "variantName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "conflictingName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
              Core.projectionFieldName = (Core.Name "conflictingName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the variantName field of hydra.error.packaging.ConflictingVariantNameError
conflictingVariantNameErrorWithVariantName :: Phantoms.TTerm ErrorPackaging.ConflictingVariantNameError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorPackaging.ConflictingVariantNameError
conflictingVariantNameErrorWithVariantName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
              Core.projectionFieldName = (Core.Name "moduleName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
              Core.projectionFieldName = (Core.Name "typeName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variantName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "conflictingName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
              Core.projectionFieldName = (Core.Name "conflictingName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.error.packaging.DefinitionNotInModuleNameError
definitionNotInModuleNameError :: Phantoms.TTerm Packaging.ModuleName -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorPackaging.DefinitionNotInModuleNameError
definitionNotInModuleNameError moduleName name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.DefinitionNotInModuleNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Phantoms.unTTerm moduleName)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))
-- | DSL accessor for the moduleName field of hydra.error.packaging.DefinitionNotInModuleNameError
definitionNotInModuleNameErrorModuleName :: Phantoms.TTerm ErrorPackaging.DefinitionNotInModuleNameError -> Phantoms.TTerm Packaging.ModuleName
definitionNotInModuleNameErrorModuleName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.DefinitionNotInModuleNameError"),
        Core.projectionFieldName = (Core.Name "moduleName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.error.packaging.DefinitionNotInModuleNameError
definitionNotInModuleNameErrorName :: Phantoms.TTerm ErrorPackaging.DefinitionNotInModuleNameError -> Phantoms.TTerm Core.Name
definitionNotInModuleNameErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.DefinitionNotInModuleNameError"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the moduleName field of hydra.error.packaging.DefinitionNotInModuleNameError
definitionNotInModuleNameErrorWithModuleName :: Phantoms.TTerm ErrorPackaging.DefinitionNotInModuleNameError -> Phantoms.TTerm Packaging.ModuleName -> Phantoms.TTerm ErrorPackaging.DefinitionNotInModuleNameError
definitionNotInModuleNameErrorWithModuleName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.DefinitionNotInModuleNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.DefinitionNotInModuleNameError"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the name field of hydra.error.packaging.DefinitionNotInModuleNameError
definitionNotInModuleNameErrorWithName :: Phantoms.TTerm ErrorPackaging.DefinitionNotInModuleNameError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorPackaging.DefinitionNotInModuleNameError
definitionNotInModuleNameErrorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.DefinitionNotInModuleNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.DefinitionNotInModuleNameError"),
              Core.projectionFieldName = (Core.Name "moduleName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.error.packaging.DefinitionsOutOfOrderError
definitionsOutOfOrderError :: Phantoms.TTerm Packaging.ModuleName -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorPackaging.DefinitionsOutOfOrderError
definitionsOutOfOrderError moduleName precedingName followingName =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.DefinitionsOutOfOrderError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Phantoms.unTTerm moduleName)},
        Core.Field {
          Core.fieldName = (Core.Name "precedingName"),
          Core.fieldTerm = (Phantoms.unTTerm precedingName)},
        Core.Field {
          Core.fieldName = (Core.Name "followingName"),
          Core.fieldTerm = (Phantoms.unTTerm followingName)}]}))
-- | DSL accessor for the followingName field of hydra.error.packaging.DefinitionsOutOfOrderError
definitionsOutOfOrderErrorFollowingName :: Phantoms.TTerm ErrorPackaging.DefinitionsOutOfOrderError -> Phantoms.TTerm Core.Name
definitionsOutOfOrderErrorFollowingName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.DefinitionsOutOfOrderError"),
        Core.projectionFieldName = (Core.Name "followingName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the moduleName field of hydra.error.packaging.DefinitionsOutOfOrderError
definitionsOutOfOrderErrorModuleName :: Phantoms.TTerm ErrorPackaging.DefinitionsOutOfOrderError -> Phantoms.TTerm Packaging.ModuleName
definitionsOutOfOrderErrorModuleName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.DefinitionsOutOfOrderError"),
        Core.projectionFieldName = (Core.Name "moduleName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the precedingName field of hydra.error.packaging.DefinitionsOutOfOrderError
definitionsOutOfOrderErrorPrecedingName :: Phantoms.TTerm ErrorPackaging.DefinitionsOutOfOrderError -> Phantoms.TTerm Core.Name
definitionsOutOfOrderErrorPrecedingName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.DefinitionsOutOfOrderError"),
        Core.projectionFieldName = (Core.Name "precedingName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the followingName field of hydra.error.packaging.DefinitionsOutOfOrderError
definitionsOutOfOrderErrorWithFollowingName :: Phantoms.TTerm ErrorPackaging.DefinitionsOutOfOrderError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorPackaging.DefinitionsOutOfOrderError
definitionsOutOfOrderErrorWithFollowingName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.DefinitionsOutOfOrderError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.DefinitionsOutOfOrderError"),
              Core.projectionFieldName = (Core.Name "moduleName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "precedingName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.DefinitionsOutOfOrderError"),
              Core.projectionFieldName = (Core.Name "precedingName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "followingName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the moduleName field of hydra.error.packaging.DefinitionsOutOfOrderError
definitionsOutOfOrderErrorWithModuleName :: Phantoms.TTerm ErrorPackaging.DefinitionsOutOfOrderError -> Phantoms.TTerm Packaging.ModuleName -> Phantoms.TTerm ErrorPackaging.DefinitionsOutOfOrderError
definitionsOutOfOrderErrorWithModuleName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.DefinitionsOutOfOrderError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "precedingName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.DefinitionsOutOfOrderError"),
              Core.projectionFieldName = (Core.Name "precedingName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "followingName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.DefinitionsOutOfOrderError"),
              Core.projectionFieldName = (Core.Name "followingName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the precedingName field of hydra.error.packaging.DefinitionsOutOfOrderError
definitionsOutOfOrderErrorWithPrecedingName :: Phantoms.TTerm ErrorPackaging.DefinitionsOutOfOrderError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorPackaging.DefinitionsOutOfOrderError
definitionsOutOfOrderErrorWithPrecedingName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.DefinitionsOutOfOrderError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.DefinitionsOutOfOrderError"),
              Core.projectionFieldName = (Core.Name "moduleName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "precedingName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "followingName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.DefinitionsOutOfOrderError"),
              Core.projectionFieldName = (Core.Name "followingName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.error.packaging.DuplicateDefinitionNameError
duplicateDefinitionNameError :: Phantoms.TTerm Packaging.ModuleName -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorPackaging.DuplicateDefinitionNameError
duplicateDefinitionNameError moduleName name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.DuplicateDefinitionNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Phantoms.unTTerm moduleName)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))
-- | DSL accessor for the moduleName field of hydra.error.packaging.DuplicateDefinitionNameError
duplicateDefinitionNameErrorModuleName :: Phantoms.TTerm ErrorPackaging.DuplicateDefinitionNameError -> Phantoms.TTerm Packaging.ModuleName
duplicateDefinitionNameErrorModuleName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.DuplicateDefinitionNameError"),
        Core.projectionFieldName = (Core.Name "moduleName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.error.packaging.DuplicateDefinitionNameError
duplicateDefinitionNameErrorName :: Phantoms.TTerm ErrorPackaging.DuplicateDefinitionNameError -> Phantoms.TTerm Core.Name
duplicateDefinitionNameErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.DuplicateDefinitionNameError"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the moduleName field of hydra.error.packaging.DuplicateDefinitionNameError
duplicateDefinitionNameErrorWithModuleName :: Phantoms.TTerm ErrorPackaging.DuplicateDefinitionNameError -> Phantoms.TTerm Packaging.ModuleName -> Phantoms.TTerm ErrorPackaging.DuplicateDefinitionNameError
duplicateDefinitionNameErrorWithModuleName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.DuplicateDefinitionNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.DuplicateDefinitionNameError"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the name field of hydra.error.packaging.DuplicateDefinitionNameError
duplicateDefinitionNameErrorWithName :: Phantoms.TTerm ErrorPackaging.DuplicateDefinitionNameError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorPackaging.DuplicateDefinitionNameError
duplicateDefinitionNameErrorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.DuplicateDefinitionNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.DuplicateDefinitionNameError"),
              Core.projectionFieldName = (Core.Name "moduleName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.error.packaging.DuplicateModuleNameError
duplicateModuleNameError :: Phantoms.TTerm Packaging.ModuleName -> Phantoms.TTerm ErrorPackaging.DuplicateModuleNameError
duplicateModuleNameError moduleName =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.DuplicateModuleNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Phantoms.unTTerm moduleName)}]}))
-- | DSL accessor for the moduleName field of hydra.error.packaging.DuplicateModuleNameError
duplicateModuleNameErrorModuleName :: Phantoms.TTerm ErrorPackaging.DuplicateModuleNameError -> Phantoms.TTerm Packaging.ModuleName
duplicateModuleNameErrorModuleName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.DuplicateModuleNameError"),
        Core.projectionFieldName = (Core.Name "moduleName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the moduleName field of hydra.error.packaging.DuplicateModuleNameError
duplicateModuleNameErrorWithModuleName :: Phantoms.TTerm ErrorPackaging.DuplicateModuleNameError -> Phantoms.TTerm Packaging.ModuleName -> Phantoms.TTerm ErrorPackaging.DuplicateModuleNameError
duplicateModuleNameErrorWithModuleName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.DuplicateModuleNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.error.packaging.InvalidDefinitionNameError
invalidDefinitionNameError :: Phantoms.TTerm Packaging.ModuleName -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Util.CaseConvention -> Phantoms.TTerm ErrorPackaging.InvalidDefinitionNameError
invalidDefinitionNameError moduleName name expectedConvention =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.InvalidDefinitionNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Phantoms.unTTerm moduleName)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "expectedConvention"),
          Core.fieldTerm = (Phantoms.unTTerm expectedConvention)}]}))
-- | DSL accessor for the expectedConvention field of hydra.error.packaging.InvalidDefinitionNameError
invalidDefinitionNameErrorExpectedConvention :: Phantoms.TTerm ErrorPackaging.InvalidDefinitionNameError -> Phantoms.TTerm Util.CaseConvention
invalidDefinitionNameErrorExpectedConvention x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.InvalidDefinitionNameError"),
        Core.projectionFieldName = (Core.Name "expectedConvention")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the moduleName field of hydra.error.packaging.InvalidDefinitionNameError
invalidDefinitionNameErrorModuleName :: Phantoms.TTerm ErrorPackaging.InvalidDefinitionNameError -> Phantoms.TTerm Packaging.ModuleName
invalidDefinitionNameErrorModuleName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.InvalidDefinitionNameError"),
        Core.projectionFieldName = (Core.Name "moduleName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.error.packaging.InvalidDefinitionNameError
invalidDefinitionNameErrorName :: Phantoms.TTerm ErrorPackaging.InvalidDefinitionNameError -> Phantoms.TTerm Core.Name
invalidDefinitionNameErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.InvalidDefinitionNameError"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the expectedConvention field of hydra.error.packaging.InvalidDefinitionNameError
invalidDefinitionNameErrorWithExpectedConvention :: Phantoms.TTerm ErrorPackaging.InvalidDefinitionNameError -> Phantoms.TTerm Util.CaseConvention -> Phantoms.TTerm ErrorPackaging.InvalidDefinitionNameError
invalidDefinitionNameErrorWithExpectedConvention original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.InvalidDefinitionNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.InvalidDefinitionNameError"),
              Core.projectionFieldName = (Core.Name "moduleName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.InvalidDefinitionNameError"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expectedConvention"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the moduleName field of hydra.error.packaging.InvalidDefinitionNameError
invalidDefinitionNameErrorWithModuleName :: Phantoms.TTerm ErrorPackaging.InvalidDefinitionNameError -> Phantoms.TTerm Packaging.ModuleName -> Phantoms.TTerm ErrorPackaging.InvalidDefinitionNameError
invalidDefinitionNameErrorWithModuleName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.InvalidDefinitionNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.InvalidDefinitionNameError"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expectedConvention"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.InvalidDefinitionNameError"),
              Core.projectionFieldName = (Core.Name "expectedConvention")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the name field of hydra.error.packaging.InvalidDefinitionNameError
invalidDefinitionNameErrorWithName :: Phantoms.TTerm ErrorPackaging.InvalidDefinitionNameError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorPackaging.InvalidDefinitionNameError
invalidDefinitionNameErrorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.InvalidDefinitionNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.InvalidDefinitionNameError"),
              Core.projectionFieldName = (Core.Name "moduleName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expectedConvention"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.InvalidDefinitionNameError"),
              Core.projectionFieldName = (Core.Name "expectedConvention")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the conflictingVariantName variant of hydra.error.packaging.InvalidModuleError
invalidModuleErrorConflictingVariantName :: Phantoms.TTerm ErrorPackaging.ConflictingVariantNameError -> Phantoms.TTerm ErrorPackaging.InvalidModuleError
invalidModuleErrorConflictingVariantName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidModuleError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "conflictingVariantName"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the definitionNotInModuleName variant of hydra.error.packaging.InvalidModuleError
invalidModuleErrorDefinitionNotInModuleName :: Phantoms.TTerm ErrorPackaging.DefinitionNotInModuleNameError -> Phantoms.TTerm ErrorPackaging.InvalidModuleError
invalidModuleErrorDefinitionNotInModuleName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidModuleError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "definitionNotInModuleName"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the definitionsOutOfOrder variant of hydra.error.packaging.InvalidModuleError
invalidModuleErrorDefinitionsOutOfOrder :: Phantoms.TTerm ErrorPackaging.DefinitionsOutOfOrderError -> Phantoms.TTerm ErrorPackaging.InvalidModuleError
invalidModuleErrorDefinitionsOutOfOrder x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidModuleError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "definitionsOutOfOrder"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the duplicateDefinitionName variant of hydra.error.packaging.InvalidModuleError
invalidModuleErrorDuplicateDefinitionName :: Phantoms.TTerm ErrorPackaging.DuplicateDefinitionNameError -> Phantoms.TTerm ErrorPackaging.InvalidModuleError
invalidModuleErrorDuplicateDefinitionName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidModuleError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "duplicateDefinitionName"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the invalidDefinitionName variant of hydra.error.packaging.InvalidModuleError
invalidModuleErrorInvalidDefinitionName :: Phantoms.TTerm ErrorPackaging.InvalidDefinitionNameError -> Phantoms.TTerm ErrorPackaging.InvalidModuleError
invalidModuleErrorInvalidDefinitionName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidModuleError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "invalidDefinitionName"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the invalidModuleNameConvention variant of hydra.error.packaging.InvalidModuleError
invalidModuleErrorInvalidModuleNameConvention :: Phantoms.TTerm ErrorPackaging.InvalidModuleNameConventionError -> Phantoms.TTerm ErrorPackaging.InvalidModuleError
invalidModuleErrorInvalidModuleNameConvention x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidModuleError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "invalidModuleNameConvention"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the missingDocumentation variant of hydra.error.packaging.InvalidModuleError
invalidModuleErrorMissingDocumentation :: Phantoms.TTerm ErrorPackaging.MissingDocumentationError -> Phantoms.TTerm ErrorPackaging.InvalidModuleError
invalidModuleErrorMissingDocumentation x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidModuleError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "missingDocumentation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.error.packaging.InvalidModuleNameConventionError
invalidModuleNameConventionError :: Phantoms.TTerm Packaging.ModuleName -> Phantoms.TTerm ErrorPackaging.InvalidModuleNameConventionError
invalidModuleNameConventionError moduleName =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.InvalidModuleNameConventionError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Phantoms.unTTerm moduleName)}]}))
-- | DSL accessor for the moduleName field of hydra.error.packaging.InvalidModuleNameConventionError
invalidModuleNameConventionErrorModuleName :: Phantoms.TTerm ErrorPackaging.InvalidModuleNameConventionError -> Phantoms.TTerm Packaging.ModuleName
invalidModuleNameConventionErrorModuleName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.InvalidModuleNameConventionError"),
        Core.projectionFieldName = (Core.Name "moduleName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the moduleName field of hydra.error.packaging.InvalidModuleNameConventionError
invalidModuleNameConventionErrorWithModuleName :: Phantoms.TTerm ErrorPackaging.InvalidModuleNameConventionError -> Phantoms.TTerm Packaging.ModuleName -> Phantoms.TTerm ErrorPackaging.InvalidModuleNameConventionError
invalidModuleNameConventionErrorWithModuleName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.InvalidModuleNameConventionError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the conflictingModuleName variant of hydra.error.packaging.InvalidPackageError
invalidPackageErrorConflictingModuleName :: Phantoms.TTerm ErrorPackaging.ConflictingModuleNameError -> Phantoms.TTerm ErrorPackaging.InvalidPackageError
invalidPackageErrorConflictingModuleName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidPackageError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "conflictingModuleName"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the duplicateModuleName variant of hydra.error.packaging.InvalidPackageError
invalidPackageErrorDuplicateModuleName :: Phantoms.TTerm ErrorPackaging.DuplicateModuleNameError -> Phantoms.TTerm ErrorPackaging.InvalidPackageError
invalidPackageErrorDuplicateModuleName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidPackageError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "duplicateModuleName"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the invalidModule variant of hydra.error.packaging.InvalidPackageError
invalidPackageErrorInvalidModule :: Phantoms.TTerm ErrorPackaging.InvalidModuleError -> Phantoms.TTerm ErrorPackaging.InvalidPackageError
invalidPackageErrorInvalidModule x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidPackageError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "invalidModule"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the invalidPackageName variant of hydra.error.packaging.InvalidPackageError
invalidPackageErrorInvalidPackageName :: Phantoms.TTerm ErrorPackaging.InvalidPackageNameError -> Phantoms.TTerm ErrorPackaging.InvalidPackageError
invalidPackageErrorInvalidPackageName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidPackageError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "invalidPackageName"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.error.packaging.InvalidPackageNameError
invalidPackageNameError :: Phantoms.TTerm Packaging.PackageName -> Phantoms.TTerm ErrorPackaging.InvalidPackageNameError
invalidPackageNameError packageName =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.InvalidPackageNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "packageName"),
          Core.fieldTerm = (Phantoms.unTTerm packageName)}]}))
-- | DSL accessor for the packageName field of hydra.error.packaging.InvalidPackageNameError
invalidPackageNameErrorPackageName :: Phantoms.TTerm ErrorPackaging.InvalidPackageNameError -> Phantoms.TTerm Packaging.PackageName
invalidPackageNameErrorPackageName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.InvalidPackageNameError"),
        Core.projectionFieldName = (Core.Name "packageName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the packageName field of hydra.error.packaging.InvalidPackageNameError
invalidPackageNameErrorWithPackageName :: Phantoms.TTerm ErrorPackaging.InvalidPackageNameError -> Phantoms.TTerm Packaging.PackageName -> Phantoms.TTerm ErrorPackaging.InvalidPackageNameError
invalidPackageNameErrorWithPackageName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.InvalidPackageNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "packageName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.error.packaging.MissingDocumentationError
missingDocumentationError :: Phantoms.TTerm Packaging.ModuleName -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorPackaging.MissingDocumentationError
missingDocumentationError moduleName name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.MissingDocumentationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Phantoms.unTTerm moduleName)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))
-- | DSL accessor for the moduleName field of hydra.error.packaging.MissingDocumentationError
missingDocumentationErrorModuleName :: Phantoms.TTerm ErrorPackaging.MissingDocumentationError -> Phantoms.TTerm Packaging.ModuleName
missingDocumentationErrorModuleName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.MissingDocumentationError"),
        Core.projectionFieldName = (Core.Name "moduleName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.error.packaging.MissingDocumentationError
missingDocumentationErrorName :: Phantoms.TTerm ErrorPackaging.MissingDocumentationError -> Phantoms.TTerm Core.Name
missingDocumentationErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.MissingDocumentationError"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the moduleName field of hydra.error.packaging.MissingDocumentationError
missingDocumentationErrorWithModuleName :: Phantoms.TTerm ErrorPackaging.MissingDocumentationError -> Phantoms.TTerm Packaging.ModuleName -> Phantoms.TTerm ErrorPackaging.MissingDocumentationError
missingDocumentationErrorWithModuleName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.MissingDocumentationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.MissingDocumentationError"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the name field of hydra.error.packaging.MissingDocumentationError
missingDocumentationErrorWithName :: Phantoms.TTerm ErrorPackaging.MissingDocumentationError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorPackaging.MissingDocumentationError
missingDocumentationErrorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.MissingDocumentationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.MissingDocumentationError"),
              Core.projectionFieldName = (Core.Name "moduleName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
