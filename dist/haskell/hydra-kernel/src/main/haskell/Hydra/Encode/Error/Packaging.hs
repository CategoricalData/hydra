-- Note: this is an automatically generated file. Do not edit.

-- | Term encoders for hydra.error.packaging

module Hydra.Encode.Error.Packaging where

import qualified Hydra.Core as Core
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Encode.Packaging as EncodePackaging
import qualified Hydra.Encode.Util as Util
import qualified Hydra.Error.Packaging as ErrorPackaging
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

-- | Encoder for hydra.error.packaging.ConflictingModuleNameError
conflictingModuleNameError :: ErrorPackaging.ConflictingModuleNameError -> Core.Term
conflictingModuleNameError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.ConflictingModuleNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (EncodePackaging.moduleName (ErrorPackaging.conflictingModuleNameErrorFirst x))},
        Core.Field {
          Core.fieldName = (Core.Name "second"),
          Core.fieldTerm = (EncodePackaging.moduleName (ErrorPackaging.conflictingModuleNameErrorSecond x))}]})

-- | Encoder for hydra.error.packaging.ConflictingVariantNameError
conflictingVariantNameError :: ErrorPackaging.ConflictingVariantNameError -> Core.Term
conflictingVariantNameError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (EncodePackaging.moduleName (ErrorPackaging.conflictingVariantNameErrorModuleName x))},
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (EncodeCore.name (ErrorPackaging.conflictingVariantNameErrorTypeName x))},
        Core.Field {
          Core.fieldName = (Core.Name "variantName"),
          Core.fieldTerm = (EncodeCore.name (ErrorPackaging.conflictingVariantNameErrorVariantName x))},
        Core.Field {
          Core.fieldName = (Core.Name "conflictingName"),
          Core.fieldTerm = (EncodeCore.name (ErrorPackaging.conflictingVariantNameErrorConflictingName x))}]})

-- | Encoder for hydra.error.packaging.DefinitionNotInModuleNameError
definitionNotInModuleNameError :: ErrorPackaging.DefinitionNotInModuleNameError -> Core.Term
definitionNotInModuleNameError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.DefinitionNotInModuleNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (EncodePackaging.moduleName (ErrorPackaging.definitionNotInModuleNameErrorModuleName x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (EncodeCore.name (ErrorPackaging.definitionNotInModuleNameErrorName x))}]})

-- | Encoder for hydra.error.packaging.DefinitionsOutOfOrderError
definitionsOutOfOrderError :: ErrorPackaging.DefinitionsOutOfOrderError -> Core.Term
definitionsOutOfOrderError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.DefinitionsOutOfOrderError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (EncodePackaging.moduleName (ErrorPackaging.definitionsOutOfOrderErrorModuleName x))},
        Core.Field {
          Core.fieldName = (Core.Name "precedingName"),
          Core.fieldTerm = (EncodeCore.name (ErrorPackaging.definitionsOutOfOrderErrorPrecedingName x))},
        Core.Field {
          Core.fieldName = (Core.Name "followingName"),
          Core.fieldTerm = (EncodeCore.name (ErrorPackaging.definitionsOutOfOrderErrorFollowingName x))}]})

-- | Encoder for hydra.error.packaging.DuplicateDefinitionNameError
duplicateDefinitionNameError :: ErrorPackaging.DuplicateDefinitionNameError -> Core.Term
duplicateDefinitionNameError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.DuplicateDefinitionNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (EncodePackaging.moduleName (ErrorPackaging.duplicateDefinitionNameErrorModuleName x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (EncodeCore.name (ErrorPackaging.duplicateDefinitionNameErrorName x))}]})

-- | Encoder for hydra.error.packaging.DuplicateModuleNameError
duplicateModuleNameError :: ErrorPackaging.DuplicateModuleNameError -> Core.Term
duplicateModuleNameError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.DuplicateModuleNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (EncodePackaging.moduleName (ErrorPackaging.duplicateModuleNameErrorModuleName x))}]})

-- | Encoder for hydra.error.packaging.InvalidDefinitionNameError
invalidDefinitionNameError :: ErrorPackaging.InvalidDefinitionNameError -> Core.Term
invalidDefinitionNameError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.InvalidDefinitionNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (EncodePackaging.moduleName (ErrorPackaging.invalidDefinitionNameErrorModuleName x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (EncodeCore.name (ErrorPackaging.invalidDefinitionNameErrorName x))},
        Core.Field {
          Core.fieldName = (Core.Name "expectedConvention"),
          Core.fieldTerm = (Util.caseConvention (ErrorPackaging.invalidDefinitionNameErrorExpectedConvention x))}]})

-- | Encoder for hydra.error.packaging.InvalidModuleError
invalidModuleError :: ErrorPackaging.InvalidModuleError -> Core.Term
invalidModuleError x =
    case x of
      ErrorPackaging.InvalidModuleErrorConflictingVariantName v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidModuleError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "conflictingVariantName"),
          Core.fieldTerm = (conflictingVariantNameError v0)}})
      ErrorPackaging.InvalidModuleErrorDefinitionNotInModuleName v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidModuleError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "definitionNotInModuleName"),
          Core.fieldTerm = (definitionNotInModuleNameError v0)}})
      ErrorPackaging.InvalidModuleErrorDefinitionsOutOfOrder v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidModuleError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "definitionsOutOfOrder"),
          Core.fieldTerm = (definitionsOutOfOrderError v0)}})
      ErrorPackaging.InvalidModuleErrorDuplicateDefinitionName v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidModuleError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "duplicateDefinitionName"),
          Core.fieldTerm = (duplicateDefinitionNameError v0)}})
      ErrorPackaging.InvalidModuleErrorInvalidDefinitionName v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidModuleError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "invalidDefinitionName"),
          Core.fieldTerm = (invalidDefinitionNameError v0)}})
      ErrorPackaging.InvalidModuleErrorInvalidModuleNameConvention v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidModuleError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "invalidModuleNameConvention"),
          Core.fieldTerm = (invalidModuleNameConventionError v0)}})
      ErrorPackaging.InvalidModuleErrorMissingDocumentation v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidModuleError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "missingDocumentation"),
          Core.fieldTerm = (missingDocumentationError v0)}})

-- | Encoder for hydra.error.packaging.InvalidModuleNameConventionError
invalidModuleNameConventionError :: ErrorPackaging.InvalidModuleNameConventionError -> Core.Term
invalidModuleNameConventionError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.InvalidModuleNameConventionError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (EncodePackaging.moduleName (ErrorPackaging.invalidModuleNameConventionErrorModuleName x))}]})

-- | Encoder for hydra.error.packaging.InvalidPackageError
invalidPackageError :: ErrorPackaging.InvalidPackageError -> Core.Term
invalidPackageError x =
    case x of
      ErrorPackaging.InvalidPackageErrorConflictingModuleName v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidPackageError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "conflictingModuleName"),
          Core.fieldTerm = (conflictingModuleNameError v0)}})
      ErrorPackaging.InvalidPackageErrorDuplicateModuleName v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidPackageError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "duplicateModuleName"),
          Core.fieldTerm = (duplicateModuleNameError v0)}})
      ErrorPackaging.InvalidPackageErrorInvalidModule v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidPackageError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "invalidModule"),
          Core.fieldTerm = (invalidModuleError v0)}})
      ErrorPackaging.InvalidPackageErrorInvalidPackageName v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidPackageError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "invalidPackageName"),
          Core.fieldTerm = (invalidPackageNameError v0)}})
      ErrorPackaging.InvalidPackageErrorUndeclaredDependency v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidPackageError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "undeclaredDependency"),
          Core.fieldTerm = (undeclaredDependencyError v0)}})

-- | Encoder for hydra.error.packaging.InvalidPackageNameError
invalidPackageNameError :: ErrorPackaging.InvalidPackageNameError -> Core.Term
invalidPackageNameError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.InvalidPackageNameError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "packageName"),
          Core.fieldTerm = (EncodePackaging.packageName (ErrorPackaging.invalidPackageNameErrorPackageName x))}]})

-- | Encoder for hydra.error.packaging.MissingDocumentationError
missingDocumentationError :: ErrorPackaging.MissingDocumentationError -> Core.Term
missingDocumentationError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.MissingDocumentationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (EncodePackaging.moduleName (ErrorPackaging.missingDocumentationErrorModuleName x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (EncodeCore.name (ErrorPackaging.missingDocumentationErrorName x))}]})

-- | Encoder for hydra.error.packaging.UndeclaredDependencyError
undeclaredDependencyError :: ErrorPackaging.UndeclaredDependencyError -> Core.Term
undeclaredDependencyError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.packaging.UndeclaredDependencyError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (EncodePackaging.moduleName (ErrorPackaging.undeclaredDependencyErrorModuleName x))},
        Core.Field {
          Core.fieldName = (Core.Name "referencedName"),
          Core.fieldTerm = (EncodeCore.name (ErrorPackaging.undeclaredDependencyErrorReferencedName x))},
        Core.Field {
          Core.fieldName = (Core.Name "owningModuleName"),
          Core.fieldTerm = (EncodePackaging.moduleName (ErrorPackaging.undeclaredDependencyErrorOwningModuleName x))}]})
