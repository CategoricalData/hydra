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
conflictingModuleNamespaceErrorFirst :: Phantoms.TTerm ErrorPackaging.ConflictingModuleNamespaceError -> Phantoms.TTerm Packaging.Namespace
conflictingModuleNamespaceErrorFirst x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingModuleNamespaceError"),
        Core.projectionField = (Core.Name "first")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
conflictingModuleNamespaceErrorSecond :: Phantoms.TTerm ErrorPackaging.ConflictingModuleNamespaceError -> Phantoms.TTerm Packaging.Namespace
conflictingModuleNamespaceErrorSecond x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingModuleNamespaceError"),
        Core.projectionField = (Core.Name "second")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
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
              Core.projectionField = (Core.Name "second")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
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
              Core.projectionField = (Core.Name "first")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "second"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
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
conflictingVariantNameErrorConflictingName :: Phantoms.TTerm ErrorPackaging.ConflictingVariantNameError -> Phantoms.TTerm Core.Name
conflictingVariantNameErrorConflictingName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
        Core.projectionField = (Core.Name "conflictingName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
conflictingVariantNameErrorNamespace :: Phantoms.TTerm ErrorPackaging.ConflictingVariantNameError -> Phantoms.TTerm Packaging.Namespace
conflictingVariantNameErrorNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
        Core.projectionField = (Core.Name "namespace")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
conflictingVariantNameErrorTypeName :: Phantoms.TTerm ErrorPackaging.ConflictingVariantNameError -> Phantoms.TTerm Core.Name
conflictingVariantNameErrorTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
        Core.projectionField = (Core.Name "typeName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
conflictingVariantNameErrorVariantName :: Phantoms.TTerm ErrorPackaging.ConflictingVariantNameError -> Phantoms.TTerm Core.Name
conflictingVariantNameErrorVariantName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
        Core.projectionField = (Core.Name "variantName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
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
              Core.projectionField = (Core.Name "namespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
              Core.projectionField = (Core.Name "typeName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variantName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
              Core.projectionField = (Core.Name "variantName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "conflictingName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
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
              Core.projectionField = (Core.Name "typeName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variantName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
              Core.projectionField = (Core.Name "variantName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "conflictingName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
              Core.projectionField = (Core.Name "conflictingName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
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
              Core.projectionField = (Core.Name "namespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "variantName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
              Core.projectionField = (Core.Name "variantName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "conflictingName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
              Core.projectionField = (Core.Name "conflictingName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
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
              Core.projectionField = (Core.Name "namespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
              Core.projectionField = (Core.Name "typeName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variantName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "conflictingName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
              Core.projectionField = (Core.Name "conflictingName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
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
definitionNotInModuleNamespaceErrorName :: Phantoms.TTerm ErrorPackaging.DefinitionNotInModuleNamespaceError -> Phantoms.TTerm Core.Name
definitionNotInModuleNamespaceErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.DefinitionNotInModuleNamespaceError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
definitionNotInModuleNamespaceErrorNamespace :: Phantoms.TTerm ErrorPackaging.DefinitionNotInModuleNamespaceError -> Phantoms.TTerm Packaging.Namespace
definitionNotInModuleNamespaceErrorNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.DefinitionNotInModuleNamespaceError"),
        Core.projectionField = (Core.Name "namespace")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
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
              Core.projectionField = (Core.Name "namespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
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
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
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
definitionsOutOfOrderErrorFollowingName :: Phantoms.TTerm ErrorPackaging.DefinitionsOutOfOrderError -> Phantoms.TTerm Core.Name
definitionsOutOfOrderErrorFollowingName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.DefinitionsOutOfOrderError"),
        Core.projectionField = (Core.Name "followingName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
definitionsOutOfOrderErrorNamespace :: Phantoms.TTerm ErrorPackaging.DefinitionsOutOfOrderError -> Phantoms.TTerm Packaging.Namespace
definitionsOutOfOrderErrorNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.DefinitionsOutOfOrderError"),
        Core.projectionField = (Core.Name "namespace")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
definitionsOutOfOrderErrorPrecedingName :: Phantoms.TTerm ErrorPackaging.DefinitionsOutOfOrderError -> Phantoms.TTerm Core.Name
definitionsOutOfOrderErrorPrecedingName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.DefinitionsOutOfOrderError"),
        Core.projectionField = (Core.Name "precedingName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
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
              Core.projectionField = (Core.Name "namespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "precedingName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.DefinitionsOutOfOrderError"),
              Core.projectionField = (Core.Name "precedingName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "followingName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
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
              Core.projectionField = (Core.Name "precedingName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "followingName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.DefinitionsOutOfOrderError"),
              Core.projectionField = (Core.Name "followingName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
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
              Core.projectionField = (Core.Name "namespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "precedingName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "followingName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.DefinitionsOutOfOrderError"),
              Core.projectionField = (Core.Name "followingName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
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
duplicateDefinitionNameErrorName :: Phantoms.TTerm ErrorPackaging.DuplicateDefinitionNameError -> Phantoms.TTerm Core.Name
duplicateDefinitionNameErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.DuplicateDefinitionNameError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
duplicateDefinitionNameErrorNamespace :: Phantoms.TTerm ErrorPackaging.DuplicateDefinitionNameError -> Phantoms.TTerm Packaging.Namespace
duplicateDefinitionNameErrorNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.DuplicateDefinitionNameError"),
        Core.projectionField = (Core.Name "namespace")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
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
              Core.projectionField = (Core.Name "namespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
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
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
duplicateModuleNamespaceError :: Phantoms.TTerm Packaging.Namespace -> Phantoms.TTerm ErrorPackaging.DuplicateModuleNamespaceError
duplicateModuleNamespaceError namespace =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.DuplicateModuleNamespaceError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm namespace)}]}))
duplicateModuleNamespaceErrorNamespace :: Phantoms.TTerm ErrorPackaging.DuplicateModuleNamespaceError -> Phantoms.TTerm Packaging.Namespace
duplicateModuleNamespaceErrorNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.DuplicateModuleNamespaceError"),
        Core.projectionField = (Core.Name "namespace")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
duplicateModuleNamespaceErrorWithNamespace :: Phantoms.TTerm ErrorPackaging.DuplicateModuleNamespaceError -> Phantoms.TTerm Packaging.Namespace -> Phantoms.TTerm ErrorPackaging.DuplicateModuleNamespaceError
duplicateModuleNamespaceErrorWithNamespace original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.DuplicateModuleNamespaceError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
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
invalidDefinitionNameErrorExpectedConvention :: Phantoms.TTerm ErrorPackaging.InvalidDefinitionNameError -> Phantoms.TTerm Util.CaseConvention
invalidDefinitionNameErrorExpectedConvention x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.InvalidDefinitionNameError"),
        Core.projectionField = (Core.Name "expectedConvention")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
invalidDefinitionNameErrorName :: Phantoms.TTerm ErrorPackaging.InvalidDefinitionNameError -> Phantoms.TTerm Core.Name
invalidDefinitionNameErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.InvalidDefinitionNameError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
invalidDefinitionNameErrorNamespace :: Phantoms.TTerm ErrorPackaging.InvalidDefinitionNameError -> Phantoms.TTerm Packaging.Namespace
invalidDefinitionNameErrorNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.InvalidDefinitionNameError"),
        Core.projectionField = (Core.Name "namespace")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
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
              Core.projectionField = (Core.Name "namespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.InvalidDefinitionNameError"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expectedConvention"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
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
              Core.projectionField = (Core.Name "namespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expectedConvention"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.InvalidDefinitionNameError"),
              Core.projectionField = (Core.Name "expectedConvention")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
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
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expectedConvention"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.packaging.InvalidDefinitionNameError"),
              Core.projectionField = (Core.Name "expectedConvention")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
invalidModuleErrorConflictingVariantName :: Phantoms.TTerm ErrorPackaging.ConflictingVariantNameError -> Phantoms.TTerm ErrorPackaging.InvalidModuleError
invalidModuleErrorConflictingVariantName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidModuleError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "conflictingVariantName"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
invalidModuleErrorDefinitionNotInModuleNamespace :: Phantoms.TTerm ErrorPackaging.DefinitionNotInModuleNamespaceError -> Phantoms.TTerm ErrorPackaging.InvalidModuleError
invalidModuleErrorDefinitionNotInModuleNamespace x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidModuleError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "definitionNotInModuleNamespace"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
invalidModuleErrorDefinitionsOutOfOrder :: Phantoms.TTerm ErrorPackaging.DefinitionsOutOfOrderError -> Phantoms.TTerm ErrorPackaging.InvalidModuleError
invalidModuleErrorDefinitionsOutOfOrder x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidModuleError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "definitionsOutOfOrder"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
invalidModuleErrorDuplicateDefinitionName :: Phantoms.TTerm ErrorPackaging.DuplicateDefinitionNameError -> Phantoms.TTerm ErrorPackaging.InvalidModuleError
invalidModuleErrorDuplicateDefinitionName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidModuleError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "duplicateDefinitionName"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
invalidModuleErrorInvalidDefinitionName :: Phantoms.TTerm ErrorPackaging.InvalidDefinitionNameError -> Phantoms.TTerm ErrorPackaging.InvalidModuleError
invalidModuleErrorInvalidDefinitionName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidModuleError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "invalidDefinitionName"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
invalidModuleErrorInvalidNamespaceConvention :: Phantoms.TTerm ErrorPackaging.InvalidNamespaceConventionError -> Phantoms.TTerm ErrorPackaging.InvalidModuleError
invalidModuleErrorInvalidNamespaceConvention x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidModuleError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "invalidNamespaceConvention"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
invalidModuleErrorMissingDocumentation :: Phantoms.TTerm ErrorPackaging.MissingDocumentationError -> Phantoms.TTerm ErrorPackaging.InvalidModuleError
invalidModuleErrorMissingDocumentation x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidModuleError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "missingDocumentation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
invalidNamespaceConventionError :: Phantoms.TTerm Packaging.Namespace -> Phantoms.TTerm ErrorPackaging.InvalidNamespaceConventionError
invalidNamespaceConventionError namespace =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.InvalidNamespaceConventionError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm namespace)}]}))
invalidNamespaceConventionErrorNamespace :: Phantoms.TTerm ErrorPackaging.InvalidNamespaceConventionError -> Phantoms.TTerm Packaging.Namespace
invalidNamespaceConventionErrorNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.InvalidNamespaceConventionError"),
        Core.projectionField = (Core.Name "namespace")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
invalidNamespaceConventionErrorWithNamespace :: Phantoms.TTerm ErrorPackaging.InvalidNamespaceConventionError -> Phantoms.TTerm Packaging.Namespace -> Phantoms.TTerm ErrorPackaging.InvalidNamespaceConventionError
invalidNamespaceConventionErrorWithNamespace original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.InvalidNamespaceConventionError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
invalidPackageErrorConflictingModuleNamespace :: Phantoms.TTerm ErrorPackaging.ConflictingModuleNamespaceError -> Phantoms.TTerm ErrorPackaging.InvalidPackageError
invalidPackageErrorConflictingModuleNamespace x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidPackageError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "conflictingModuleNamespace"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
invalidPackageErrorDuplicateModuleNamespace :: Phantoms.TTerm ErrorPackaging.DuplicateModuleNamespaceError -> Phantoms.TTerm ErrorPackaging.InvalidPackageError
invalidPackageErrorDuplicateModuleNamespace x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidPackageError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "duplicateModuleNamespace"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
invalidPackageErrorInvalidModule :: Phantoms.TTerm ErrorPackaging.InvalidModuleError -> Phantoms.TTerm ErrorPackaging.InvalidPackageError
invalidPackageErrorInvalidModule x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidPackageError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "invalidModule"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
invalidPackageErrorInvalidPackageName :: Phantoms.TTerm ErrorPackaging.InvalidPackageNameError -> Phantoms.TTerm ErrorPackaging.InvalidPackageError
invalidPackageErrorInvalidPackageName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidPackageError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "invalidPackageName"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
invalidPackageNameError :: Phantoms.TTerm Packaging.PackageName -> Phantoms.TTerm ErrorPackaging.InvalidPackageNameError
invalidPackageNameError packageName =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.InvalidPackageNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "packageName"),
          Core.fieldTerm = (Phantoms.unTTerm packageName)}]}))
invalidPackageNameErrorPackageName :: Phantoms.TTerm ErrorPackaging.InvalidPackageNameError -> Phantoms.TTerm Packaging.PackageName
invalidPackageNameErrorPackageName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.InvalidPackageNameError"),
        Core.projectionField = (Core.Name "packageName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
invalidPackageNameErrorWithPackageName :: Phantoms.TTerm ErrorPackaging.InvalidPackageNameError -> Phantoms.TTerm Packaging.PackageName -> Phantoms.TTerm ErrorPackaging.InvalidPackageNameError
invalidPackageNameErrorWithPackageName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.InvalidPackageNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "packageName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
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
missingDocumentationErrorName :: Phantoms.TTerm ErrorPackaging.MissingDocumentationError -> Phantoms.TTerm Core.Name
missingDocumentationErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.MissingDocumentationError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
missingDocumentationErrorNamespace :: Phantoms.TTerm ErrorPackaging.MissingDocumentationError -> Phantoms.TTerm Packaging.Namespace
missingDocumentationErrorNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.MissingDocumentationError"),
        Core.projectionField = (Core.Name "namespace")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
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
              Core.projectionField = (Core.Name "namespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
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
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
