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
-- | DSL constructor for hydra.error.packaging.ConflictingModuleNamespaceError
conflictingModuleNamespaceError :: Phantoms.TTerm Packaging.Namespace -> Phantoms.TTerm Packaging.Namespace -> Phantoms.TTerm ErrorPackaging.ConflictingModuleNamespaceError
conflictingModuleNamespaceError first second =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.ConflictingModuleNamespaceError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Phantoms.unTTerm first)},
        Core.Field {
          Core.fieldName = (Core.Name "second"),
          Core.fieldTerm = (Phantoms.unTTerm second)}]}))
-- | DSL accessor for the first field of hydra.error.packaging.ConflictingModuleNamespaceError
conflictingModuleNamespaceErrorFirst :: Phantoms.TTerm ErrorPackaging.ConflictingModuleNamespaceError -> Phantoms.TTerm Packaging.Namespace
conflictingModuleNamespaceErrorFirst x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingModuleNamespaceError"),
        Core.projectionFieldName = (Core.Name "first")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the second field of hydra.error.packaging.ConflictingModuleNamespaceError
conflictingModuleNamespaceErrorSecond :: Phantoms.TTerm ErrorPackaging.ConflictingModuleNamespaceError -> Phantoms.TTerm Packaging.Namespace
conflictingModuleNamespaceErrorSecond x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingModuleNamespaceError"),
        Core.projectionFieldName = (Core.Name "second")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the first field of hydra.error.packaging.ConflictingModuleNamespaceError
conflictingModuleNamespaceErrorWithFirst :: Phantoms.TTerm ErrorPackaging.ConflictingModuleNamespaceError -> Phantoms.TTerm Packaging.Namespace -> Phantoms.TTerm ErrorPackaging.ConflictingModuleNamespaceError
conflictingModuleNamespaceErrorWithFirst original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.ConflictingModuleNamespaceError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "second"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingModuleNamespaceError"),
              Core.projectionFieldName = (Core.Name "second")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the second field of hydra.error.packaging.ConflictingModuleNamespaceError
conflictingModuleNamespaceErrorWithSecond :: Phantoms.TTerm ErrorPackaging.ConflictingModuleNamespaceError -> Phantoms.TTerm Packaging.Namespace -> Phantoms.TTerm ErrorPackaging.ConflictingModuleNamespaceError
conflictingModuleNamespaceErrorWithSecond original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.ConflictingModuleNamespaceError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingModuleNamespaceError"),
              Core.projectionFieldName = (Core.Name "first")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "second"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.error.packaging.ConflictingVariantNameError
conflictingVariantNameError :: Phantoms.TTerm Packaging.Namespace -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorPackaging.ConflictingVariantNameError
conflictingVariantNameError namespace typeName variantName conflictingName =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm namespace)},
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
-- | DSL accessor for the namespace field of hydra.error.packaging.ConflictingVariantNameError
conflictingVariantNameErrorNamespace :: Phantoms.TTerm ErrorPackaging.ConflictingVariantNameError -> Phantoms.TTerm Packaging.Namespace
conflictingVariantNameErrorNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
        Core.projectionFieldName = (Core.Name "namespace")})),
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
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
              Core.projectionFieldName = (Core.Name "namespace")})),
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
-- | DSL updater for the namespace field of hydra.error.packaging.ConflictingVariantNameError
conflictingVariantNameErrorWithNamespace :: Phantoms.TTerm ErrorPackaging.ConflictingVariantNameError -> Phantoms.TTerm Packaging.Namespace -> Phantoms.TTerm ErrorPackaging.ConflictingVariantNameError
conflictingVariantNameErrorWithNamespace original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
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
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
              Core.projectionFieldName = (Core.Name "namespace")})),
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
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
              Core.projectionFieldName = (Core.Name "namespace")})),
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
-- | DSL constructor for hydra.error.packaging.DefinitionNotInModuleNamespaceError
definitionNotInModuleNamespaceError :: Phantoms.TTerm Packaging.Namespace -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorPackaging.DefinitionNotInModuleNamespaceError
definitionNotInModuleNamespaceError namespace name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.DefinitionNotInModuleNamespaceError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm namespace)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))
-- | DSL accessor for the name field of hydra.error.packaging.DefinitionNotInModuleNamespaceError
definitionNotInModuleNamespaceErrorName :: Phantoms.TTerm ErrorPackaging.DefinitionNotInModuleNamespaceError -> Phantoms.TTerm Core.Name
definitionNotInModuleNamespaceErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.DefinitionNotInModuleNamespaceError"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the namespace field of hydra.error.packaging.DefinitionNotInModuleNamespaceError
definitionNotInModuleNamespaceErrorNamespace :: Phantoms.TTerm ErrorPackaging.DefinitionNotInModuleNamespaceError -> Phantoms.TTerm Packaging.Namespace
definitionNotInModuleNamespaceErrorNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.DefinitionNotInModuleNamespaceError"),
        Core.projectionFieldName = (Core.Name "namespace")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.error.packaging.DefinitionNotInModuleNamespaceError
definitionNotInModuleNamespaceErrorWithName :: Phantoms.TTerm ErrorPackaging.DefinitionNotInModuleNamespaceError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorPackaging.DefinitionNotInModuleNamespaceError
definitionNotInModuleNamespaceErrorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.DefinitionNotInModuleNamespaceError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.DefinitionNotInModuleNamespaceError"),
              Core.projectionFieldName = (Core.Name "namespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the namespace field of hydra.error.packaging.DefinitionNotInModuleNamespaceError
definitionNotInModuleNamespaceErrorWithNamespace :: Phantoms.TTerm ErrorPackaging.DefinitionNotInModuleNamespaceError -> Phantoms.TTerm Packaging.Namespace -> Phantoms.TTerm ErrorPackaging.DefinitionNotInModuleNamespaceError
definitionNotInModuleNamespaceErrorWithNamespace original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.DefinitionNotInModuleNamespaceError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.DefinitionNotInModuleNamespaceError"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.error.packaging.DefinitionsOutOfOrderError
definitionsOutOfOrderError :: Phantoms.TTerm Packaging.Namespace -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorPackaging.DefinitionsOutOfOrderError
definitionsOutOfOrderError namespace precedingName followingName =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.DefinitionsOutOfOrderError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm namespace)},
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
-- | DSL accessor for the namespace field of hydra.error.packaging.DefinitionsOutOfOrderError
definitionsOutOfOrderErrorNamespace :: Phantoms.TTerm ErrorPackaging.DefinitionsOutOfOrderError -> Phantoms.TTerm Packaging.Namespace
definitionsOutOfOrderErrorNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.DefinitionsOutOfOrderError"),
        Core.projectionFieldName = (Core.Name "namespace")})),
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
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.DefinitionsOutOfOrderError"),
              Core.projectionFieldName = (Core.Name "namespace")})),
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
-- | DSL updater for the namespace field of hydra.error.packaging.DefinitionsOutOfOrderError
definitionsOutOfOrderErrorWithNamespace :: Phantoms.TTerm ErrorPackaging.DefinitionsOutOfOrderError -> Phantoms.TTerm Packaging.Namespace -> Phantoms.TTerm ErrorPackaging.DefinitionsOutOfOrderError
definitionsOutOfOrderErrorWithNamespace original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.DefinitionsOutOfOrderError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
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
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.DefinitionsOutOfOrderError"),
              Core.projectionFieldName = (Core.Name "namespace")})),
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
duplicateDefinitionNameError :: Phantoms.TTerm Packaging.Namespace -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorPackaging.DuplicateDefinitionNameError
duplicateDefinitionNameError namespace name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.DuplicateDefinitionNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm namespace)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))
-- | DSL accessor for the name field of hydra.error.packaging.DuplicateDefinitionNameError
duplicateDefinitionNameErrorName :: Phantoms.TTerm ErrorPackaging.DuplicateDefinitionNameError -> Phantoms.TTerm Core.Name
duplicateDefinitionNameErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.DuplicateDefinitionNameError"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the namespace field of hydra.error.packaging.DuplicateDefinitionNameError
duplicateDefinitionNameErrorNamespace :: Phantoms.TTerm ErrorPackaging.DuplicateDefinitionNameError -> Phantoms.TTerm Packaging.Namespace
duplicateDefinitionNameErrorNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.DuplicateDefinitionNameError"),
        Core.projectionFieldName = (Core.Name "namespace")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.error.packaging.DuplicateDefinitionNameError
duplicateDefinitionNameErrorWithName :: Phantoms.TTerm ErrorPackaging.DuplicateDefinitionNameError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorPackaging.DuplicateDefinitionNameError
duplicateDefinitionNameErrorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.DuplicateDefinitionNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.DuplicateDefinitionNameError"),
              Core.projectionFieldName = (Core.Name "namespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the namespace field of hydra.error.packaging.DuplicateDefinitionNameError
duplicateDefinitionNameErrorWithNamespace :: Phantoms.TTerm ErrorPackaging.DuplicateDefinitionNameError -> Phantoms.TTerm Packaging.Namespace -> Phantoms.TTerm ErrorPackaging.DuplicateDefinitionNameError
duplicateDefinitionNameErrorWithNamespace original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.DuplicateDefinitionNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.DuplicateDefinitionNameError"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.error.packaging.DuplicateModuleNamespaceError
duplicateModuleNamespaceError :: Phantoms.TTerm Packaging.Namespace -> Phantoms.TTerm ErrorPackaging.DuplicateModuleNamespaceError
duplicateModuleNamespaceError namespace =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.DuplicateModuleNamespaceError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm namespace)}]}))
-- | DSL accessor for the namespace field of hydra.error.packaging.DuplicateModuleNamespaceError
duplicateModuleNamespaceErrorNamespace :: Phantoms.TTerm ErrorPackaging.DuplicateModuleNamespaceError -> Phantoms.TTerm Packaging.Namespace
duplicateModuleNamespaceErrorNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.DuplicateModuleNamespaceError"),
        Core.projectionFieldName = (Core.Name "namespace")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the namespace field of hydra.error.packaging.DuplicateModuleNamespaceError
duplicateModuleNamespaceErrorWithNamespace :: Phantoms.TTerm ErrorPackaging.DuplicateModuleNamespaceError -> Phantoms.TTerm Packaging.Namespace -> Phantoms.TTerm ErrorPackaging.DuplicateModuleNamespaceError
duplicateModuleNamespaceErrorWithNamespace original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.DuplicateModuleNamespaceError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.error.packaging.InvalidDefinitionNameError
invalidDefinitionNameError :: Phantoms.TTerm Packaging.Namespace -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Util.CaseConvention -> Phantoms.TTerm ErrorPackaging.InvalidDefinitionNameError
invalidDefinitionNameError namespace name expectedConvention =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.InvalidDefinitionNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm namespace)},
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
-- | DSL accessor for the name field of hydra.error.packaging.InvalidDefinitionNameError
invalidDefinitionNameErrorName :: Phantoms.TTerm ErrorPackaging.InvalidDefinitionNameError -> Phantoms.TTerm Core.Name
invalidDefinitionNameErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.InvalidDefinitionNameError"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the namespace field of hydra.error.packaging.InvalidDefinitionNameError
invalidDefinitionNameErrorNamespace :: Phantoms.TTerm ErrorPackaging.InvalidDefinitionNameError -> Phantoms.TTerm Packaging.Namespace
invalidDefinitionNameErrorNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.InvalidDefinitionNameError"),
        Core.projectionFieldName = (Core.Name "namespace")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the expectedConvention field of hydra.error.packaging.InvalidDefinitionNameError
invalidDefinitionNameErrorWithExpectedConvention :: Phantoms.TTerm ErrorPackaging.InvalidDefinitionNameError -> Phantoms.TTerm Util.CaseConvention -> Phantoms.TTerm ErrorPackaging.InvalidDefinitionNameError
invalidDefinitionNameErrorWithExpectedConvention original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.InvalidDefinitionNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.InvalidDefinitionNameError"),
              Core.projectionFieldName = (Core.Name "namespace")})),
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
-- | DSL updater for the name field of hydra.error.packaging.InvalidDefinitionNameError
invalidDefinitionNameErrorWithName :: Phantoms.TTerm ErrorPackaging.InvalidDefinitionNameError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorPackaging.InvalidDefinitionNameError
invalidDefinitionNameErrorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.InvalidDefinitionNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.InvalidDefinitionNameError"),
              Core.projectionFieldName = (Core.Name "namespace")})),
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
-- | DSL updater for the namespace field of hydra.error.packaging.InvalidDefinitionNameError
invalidDefinitionNameErrorWithNamespace :: Phantoms.TTerm ErrorPackaging.InvalidDefinitionNameError -> Phantoms.TTerm Packaging.Namespace -> Phantoms.TTerm ErrorPackaging.InvalidDefinitionNameError
invalidDefinitionNameErrorWithNamespace original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.InvalidDefinitionNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
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
-- | DSL injection for the conflictingVariantName variant of hydra.error.packaging.InvalidModuleError
invalidModuleErrorConflictingVariantName :: Phantoms.TTerm ErrorPackaging.ConflictingVariantNameError -> Phantoms.TTerm ErrorPackaging.InvalidModuleError
invalidModuleErrorConflictingVariantName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidModuleError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "conflictingVariantName"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the definitionNotInModuleNamespace variant of hydra.error.packaging.InvalidModuleError
invalidModuleErrorDefinitionNotInModuleNamespace :: Phantoms.TTerm ErrorPackaging.DefinitionNotInModuleNamespaceError -> Phantoms.TTerm ErrorPackaging.InvalidModuleError
invalidModuleErrorDefinitionNotInModuleNamespace x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidModuleError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "definitionNotInModuleNamespace"),
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
-- | DSL injection for the invalidNamespaceConvention variant of hydra.error.packaging.InvalidModuleError
invalidModuleErrorInvalidNamespaceConvention :: Phantoms.TTerm ErrorPackaging.InvalidNamespaceConventionError -> Phantoms.TTerm ErrorPackaging.InvalidModuleError
invalidModuleErrorInvalidNamespaceConvention x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidModuleError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "invalidNamespaceConvention"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the missingDocumentation variant of hydra.error.packaging.InvalidModuleError
invalidModuleErrorMissingDocumentation :: Phantoms.TTerm ErrorPackaging.MissingDocumentationError -> Phantoms.TTerm ErrorPackaging.InvalidModuleError
invalidModuleErrorMissingDocumentation x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidModuleError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "missingDocumentation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.error.packaging.InvalidNamespaceConventionError
invalidNamespaceConventionError :: Phantoms.TTerm Packaging.Namespace -> Phantoms.TTerm ErrorPackaging.InvalidNamespaceConventionError
invalidNamespaceConventionError namespace =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.InvalidNamespaceConventionError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm namespace)}]}))
-- | DSL accessor for the namespace field of hydra.error.packaging.InvalidNamespaceConventionError
invalidNamespaceConventionErrorNamespace :: Phantoms.TTerm ErrorPackaging.InvalidNamespaceConventionError -> Phantoms.TTerm Packaging.Namespace
invalidNamespaceConventionErrorNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.InvalidNamespaceConventionError"),
        Core.projectionFieldName = (Core.Name "namespace")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the namespace field of hydra.error.packaging.InvalidNamespaceConventionError
invalidNamespaceConventionErrorWithNamespace :: Phantoms.TTerm ErrorPackaging.InvalidNamespaceConventionError -> Phantoms.TTerm Packaging.Namespace -> Phantoms.TTerm ErrorPackaging.InvalidNamespaceConventionError
invalidNamespaceConventionErrorWithNamespace original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.InvalidNamespaceConventionError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the conflictingModuleNamespace variant of hydra.error.packaging.InvalidPackageError
invalidPackageErrorConflictingModuleNamespace :: Phantoms.TTerm ErrorPackaging.ConflictingModuleNamespaceError -> Phantoms.TTerm ErrorPackaging.InvalidPackageError
invalidPackageErrorConflictingModuleNamespace x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidPackageError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "conflictingModuleNamespace"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the duplicateModuleNamespace variant of hydra.error.packaging.InvalidPackageError
invalidPackageErrorDuplicateModuleNamespace :: Phantoms.TTerm ErrorPackaging.DuplicateModuleNamespaceError -> Phantoms.TTerm ErrorPackaging.InvalidPackageError
invalidPackageErrorDuplicateModuleNamespace x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidPackageError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "duplicateModuleNamespace"),
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
missingDocumentationError :: Phantoms.TTerm Packaging.Namespace -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorPackaging.MissingDocumentationError
missingDocumentationError namespace name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.MissingDocumentationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm namespace)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))
-- | DSL accessor for the name field of hydra.error.packaging.MissingDocumentationError
missingDocumentationErrorName :: Phantoms.TTerm ErrorPackaging.MissingDocumentationError -> Phantoms.TTerm Core.Name
missingDocumentationErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.MissingDocumentationError"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the namespace field of hydra.error.packaging.MissingDocumentationError
missingDocumentationErrorNamespace :: Phantoms.TTerm ErrorPackaging.MissingDocumentationError -> Phantoms.TTerm Packaging.Namespace
missingDocumentationErrorNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.MissingDocumentationError"),
        Core.projectionFieldName = (Core.Name "namespace")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.error.packaging.MissingDocumentationError
missingDocumentationErrorWithName :: Phantoms.TTerm ErrorPackaging.MissingDocumentationError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm ErrorPackaging.MissingDocumentationError
missingDocumentationErrorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.MissingDocumentationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.MissingDocumentationError"),
              Core.projectionFieldName = (Core.Name "namespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the namespace field of hydra.error.packaging.MissingDocumentationError
missingDocumentationErrorWithNamespace :: Phantoms.TTerm ErrorPackaging.MissingDocumentationError -> Phantoms.TTerm Packaging.Namespace -> Phantoms.TTerm ErrorPackaging.MissingDocumentationError
missingDocumentationErrorWithNamespace original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.MissingDocumentationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.MissingDocumentationError"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
