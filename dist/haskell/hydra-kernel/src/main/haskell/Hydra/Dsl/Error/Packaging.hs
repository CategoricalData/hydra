-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.error.packaging

module Hydra.Dsl.Error.Packaging where

import qualified Hydra.Core as Core
import qualified Hydra.Error.Packaging as Packaging
import qualified Hydra.Packaging as Packaging_
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

conflictingModuleNamespaceError :: Phantoms.TTerm Packaging_.Namespace -> Phantoms.TTerm Packaging_.Namespace -> Phantoms.TTerm Packaging.ConflictingModuleNamespaceError
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

conflictingModuleNamespaceErrorFirst :: Phantoms.TTerm Packaging.ConflictingModuleNamespaceError -> Phantoms.TTerm Packaging_.Namespace
conflictingModuleNamespaceErrorFirst x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingModuleNamespaceError"),
        Core.projectionField = (Core.Name "first")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

conflictingModuleNamespaceErrorSecond :: Phantoms.TTerm Packaging.ConflictingModuleNamespaceError -> Phantoms.TTerm Packaging_.Namespace
conflictingModuleNamespaceErrorSecond x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingModuleNamespaceError"),
        Core.projectionField = (Core.Name "second")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

conflictingModuleNamespaceErrorWithFirst :: Phantoms.TTerm Packaging.ConflictingModuleNamespaceError -> Phantoms.TTerm Packaging_.Namespace -> Phantoms.TTerm Packaging.ConflictingModuleNamespaceError
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

conflictingModuleNamespaceErrorWithSecond :: Phantoms.TTerm Packaging.ConflictingModuleNamespaceError -> Phantoms.TTerm Packaging_.Namespace -> Phantoms.TTerm Packaging.ConflictingModuleNamespaceError
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

conflictingVariantNameError :: Phantoms.TTerm Packaging_.Namespace -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Packaging.ConflictingVariantNameError
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

conflictingVariantNameErrorConflictingName :: Phantoms.TTerm Packaging.ConflictingVariantNameError -> Phantoms.TTerm Core.Name
conflictingVariantNameErrorConflictingName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
        Core.projectionField = (Core.Name "conflictingName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

conflictingVariantNameErrorNamespace :: Phantoms.TTerm Packaging.ConflictingVariantNameError -> Phantoms.TTerm Packaging_.Namespace
conflictingVariantNameErrorNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
        Core.projectionField = (Core.Name "namespace")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

conflictingVariantNameErrorTypeName :: Phantoms.TTerm Packaging.ConflictingVariantNameError -> Phantoms.TTerm Core.Name
conflictingVariantNameErrorTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
        Core.projectionField = (Core.Name "typeName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

conflictingVariantNameErrorVariantName :: Phantoms.TTerm Packaging.ConflictingVariantNameError -> Phantoms.TTerm Core.Name
conflictingVariantNameErrorVariantName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
        Core.projectionField = (Core.Name "variantName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

conflictingVariantNameErrorWithConflictingName :: Phantoms.TTerm Packaging.ConflictingVariantNameError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Packaging.ConflictingVariantNameError
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

conflictingVariantNameErrorWithNamespace :: Phantoms.TTerm Packaging.ConflictingVariantNameError -> Phantoms.TTerm Packaging_.Namespace -> Phantoms.TTerm Packaging.ConflictingVariantNameError
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

conflictingVariantNameErrorWithTypeName :: Phantoms.TTerm Packaging.ConflictingVariantNameError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Packaging.ConflictingVariantNameError
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

conflictingVariantNameErrorWithVariantName :: Phantoms.TTerm Packaging.ConflictingVariantNameError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Packaging.ConflictingVariantNameError
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

definitionNotInModuleNamespaceError :: Phantoms.TTerm Packaging_.Namespace -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Packaging.DefinitionNotInModuleNamespaceError
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

definitionNotInModuleNamespaceErrorName :: Phantoms.TTerm Packaging.DefinitionNotInModuleNamespaceError -> Phantoms.TTerm Core.Name
definitionNotInModuleNamespaceErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.DefinitionNotInModuleNamespaceError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

definitionNotInModuleNamespaceErrorNamespace :: Phantoms.TTerm Packaging.DefinitionNotInModuleNamespaceError -> Phantoms.TTerm Packaging_.Namespace
definitionNotInModuleNamespaceErrorNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.DefinitionNotInModuleNamespaceError"),
        Core.projectionField = (Core.Name "namespace")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

definitionNotInModuleNamespaceErrorWithName :: Phantoms.TTerm Packaging.DefinitionNotInModuleNamespaceError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Packaging.DefinitionNotInModuleNamespaceError
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

definitionNotInModuleNamespaceErrorWithNamespace :: Phantoms.TTerm Packaging.DefinitionNotInModuleNamespaceError -> Phantoms.TTerm Packaging_.Namespace -> Phantoms.TTerm Packaging.DefinitionNotInModuleNamespaceError
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

duplicateDefinitionNameError :: Phantoms.TTerm Packaging_.Namespace -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Packaging.DuplicateDefinitionNameError
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

duplicateDefinitionNameErrorName :: Phantoms.TTerm Packaging.DuplicateDefinitionNameError -> Phantoms.TTerm Core.Name
duplicateDefinitionNameErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.DuplicateDefinitionNameError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

duplicateDefinitionNameErrorNamespace :: Phantoms.TTerm Packaging.DuplicateDefinitionNameError -> Phantoms.TTerm Packaging_.Namespace
duplicateDefinitionNameErrorNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.DuplicateDefinitionNameError"),
        Core.projectionField = (Core.Name "namespace")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

duplicateDefinitionNameErrorWithName :: Phantoms.TTerm Packaging.DuplicateDefinitionNameError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Packaging.DuplicateDefinitionNameError
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

duplicateDefinitionNameErrorWithNamespace :: Phantoms.TTerm Packaging.DuplicateDefinitionNameError -> Phantoms.TTerm Packaging_.Namespace -> Phantoms.TTerm Packaging.DuplicateDefinitionNameError
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

duplicateModuleNamespaceError :: Phantoms.TTerm Packaging_.Namespace -> Phantoms.TTerm Packaging.DuplicateModuleNamespaceError
duplicateModuleNamespaceError namespace =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.DuplicateModuleNamespaceError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm namespace)}]}))

duplicateModuleNamespaceErrorNamespace :: Phantoms.TTerm Packaging.DuplicateModuleNamespaceError -> Phantoms.TTerm Packaging_.Namespace
duplicateModuleNamespaceErrorNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.packaging.DuplicateModuleNamespaceError"),
        Core.projectionField = (Core.Name "namespace")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

duplicateModuleNamespaceErrorWithNamespace :: Phantoms.TTerm Packaging.DuplicateModuleNamespaceError -> Phantoms.TTerm Packaging_.Namespace -> Phantoms.TTerm Packaging.DuplicateModuleNamespaceError
duplicateModuleNamespaceErrorWithNamespace original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.DuplicateModuleNamespaceError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

invalidModuleErrorConflictingVariantName :: Phantoms.TTerm Packaging.ConflictingVariantNameError -> Phantoms.TTerm Packaging.InvalidModuleError
invalidModuleErrorConflictingVariantName x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidModuleError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "conflictingVariantName"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidModuleErrorDefinitionNotInModuleNamespace :: Phantoms.TTerm Packaging.DefinitionNotInModuleNamespaceError -> Phantoms.TTerm Packaging.InvalidModuleError
invalidModuleErrorDefinitionNotInModuleNamespace x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidModuleError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "definitionNotInModuleNamespace"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidModuleErrorDuplicateDefinitionName :: Phantoms.TTerm Packaging.DuplicateDefinitionNameError -> Phantoms.TTerm Packaging.InvalidModuleError
invalidModuleErrorDuplicateDefinitionName x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidModuleError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "duplicateDefinitionName"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidPackageErrorConflictingModuleNamespace :: Phantoms.TTerm Packaging.ConflictingModuleNamespaceError -> Phantoms.TTerm Packaging.InvalidPackageError
invalidPackageErrorConflictingModuleNamespace x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidPackageError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "conflictingModuleNamespace"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidPackageErrorDuplicateModuleNamespace :: Phantoms.TTerm Packaging.DuplicateModuleNamespaceError -> Phantoms.TTerm Packaging.InvalidPackageError
invalidPackageErrorDuplicateModuleNamespace x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidPackageError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "duplicateModuleNamespace"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

invalidPackageErrorInvalidModule :: Phantoms.TTerm Packaging.InvalidModuleError -> Phantoms.TTerm Packaging.InvalidPackageError
invalidPackageErrorInvalidModule x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidPackageError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "invalidModule"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
