-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.error.packaging

module Hydra.Decode.Error.Packaging where

import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Decode.Packaging as DecodePackaging
import qualified Hydra.Decode.Util as DecodeUtil
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Overlay.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Overlay.Haskell.Lib.Maps as Maps
import qualified Hydra.Overlay.Haskell.Lib.Optionals as Optionals
import qualified Hydra.Overlay.Haskell.Lib.Strings as Strings
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

-- | Decoder for hydra.error.packaging.ConflictingModuleNameError
conflictingModuleNameError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorPackaging.ConflictingModuleNameError
conflictingModuleNameError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "first" DecodePackaging.moduleName fieldMap cx) (\field_first -> Eithers.bind (ExtractCore.requireField "second" DecodePackaging.moduleName fieldMap cx) (\field_second -> Right (ErrorPackaging.ConflictingModuleNameError {
          ErrorPackaging.conflictingModuleNameErrorFirst = field_first,
          ErrorPackaging.conflictingModuleNameErrorSecond = field_second}))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.error.packaging.ConflictingModuleNameError")) (ExtractCore.stripWithDecodingError cx raw)

-- | Decoder for hydra.error.packaging.ConflictingVariantNameError
conflictingVariantNameError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorPackaging.ConflictingVariantNameError
conflictingVariantNameError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "moduleName" DecodePackaging.moduleName fieldMap cx) (\field_moduleName -> Eithers.bind (ExtractCore.requireField "typeName" DecodeCore.name fieldMap cx) (\field_typeName -> Eithers.bind (ExtractCore.requireField "variantName" DecodeCore.name fieldMap cx) (\field_variantName -> Eithers.bind (ExtractCore.requireField "conflictingName" DecodeCore.name fieldMap cx) (\field_conflictingName -> Right (ErrorPackaging.ConflictingVariantNameError {
          ErrorPackaging.conflictingVariantNameErrorModuleName = field_moduleName,
          ErrorPackaging.conflictingVariantNameErrorTypeName = field_typeName,
          ErrorPackaging.conflictingVariantNameErrorVariantName = field_variantName,
          ErrorPackaging.conflictingVariantNameErrorConflictingName = field_conflictingName}))))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.error.packaging.ConflictingVariantNameError")) (ExtractCore.stripWithDecodingError cx raw)

-- | Decoder for hydra.error.packaging.DefinitionNotInModuleNameError
definitionNotInModuleNameError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorPackaging.DefinitionNotInModuleNameError
definitionNotInModuleNameError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "moduleName" DecodePackaging.moduleName fieldMap cx) (\field_moduleName -> Eithers.bind (ExtractCore.requireField "name" DecodeCore.name fieldMap cx) (\field_name -> Right (ErrorPackaging.DefinitionNotInModuleNameError {
          ErrorPackaging.definitionNotInModuleNameErrorModuleName = field_moduleName,
          ErrorPackaging.definitionNotInModuleNameErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.error.packaging.DefinitionNotInModuleNameError")) (ExtractCore.stripWithDecodingError cx raw)

-- | Decoder for hydra.error.packaging.DefinitionsOutOfOrderError
definitionsOutOfOrderError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorPackaging.DefinitionsOutOfOrderError
definitionsOutOfOrderError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "moduleName" DecodePackaging.moduleName fieldMap cx) (\field_moduleName -> Eithers.bind (ExtractCore.requireField "precedingName" DecodeCore.name fieldMap cx) (\field_precedingName -> Eithers.bind (ExtractCore.requireField "followingName" DecodeCore.name fieldMap cx) (\field_followingName -> Right (ErrorPackaging.DefinitionsOutOfOrderError {
          ErrorPackaging.definitionsOutOfOrderErrorModuleName = field_moduleName,
          ErrorPackaging.definitionsOutOfOrderErrorPrecedingName = field_precedingName,
          ErrorPackaging.definitionsOutOfOrderErrorFollowingName = field_followingName})))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.error.packaging.DefinitionsOutOfOrderError")) (ExtractCore.stripWithDecodingError cx raw)

-- | Decoder for hydra.error.packaging.DuplicateDefinitionNameError
duplicateDefinitionNameError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorPackaging.DuplicateDefinitionNameError
duplicateDefinitionNameError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "moduleName" DecodePackaging.moduleName fieldMap cx) (\field_moduleName -> Eithers.bind (ExtractCore.requireField "name" DecodeCore.name fieldMap cx) (\field_name -> Right (ErrorPackaging.DuplicateDefinitionNameError {
          ErrorPackaging.duplicateDefinitionNameErrorModuleName = field_moduleName,
          ErrorPackaging.duplicateDefinitionNameErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.error.packaging.DuplicateDefinitionNameError")) (ExtractCore.stripWithDecodingError cx raw)

-- | Decoder for hydra.error.packaging.DuplicateModuleNameError
duplicateModuleNameError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorPackaging.DuplicateModuleNameError
duplicateModuleNameError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "moduleName" DecodePackaging.moduleName fieldMap cx) (\field_moduleName -> Right (ErrorPackaging.DuplicateModuleNameError {
          ErrorPackaging.duplicateModuleNameErrorModuleName = field_moduleName})))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.error.packaging.DuplicateModuleNameError")) (ExtractCore.stripWithDecodingError cx raw)

-- | Decoder for hydra.error.packaging.InvalidDefinitionNameError
invalidDefinitionNameError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorPackaging.InvalidDefinitionNameError
invalidDefinitionNameError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "moduleName" DecodePackaging.moduleName fieldMap cx) (\field_moduleName -> Eithers.bind (ExtractCore.requireField "name" DecodeCore.name fieldMap cx) (\field_name -> Eithers.bind (ExtractCore.requireField "expectedConvention" DecodeUtil.caseConvention fieldMap cx) (\field_expectedConvention -> Right (ErrorPackaging.InvalidDefinitionNameError {
          ErrorPackaging.invalidDefinitionNameErrorModuleName = field_moduleName,
          ErrorPackaging.invalidDefinitionNameErrorName = field_name,
          ErrorPackaging.invalidDefinitionNameErrorExpectedConvention = field_expectedConvention})))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.error.packaging.InvalidDefinitionNameError")) (ExtractCore.stripWithDecodingError cx raw)

-- | Decoder for hydra.error.packaging.InvalidModuleError
invalidModuleError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorPackaging.InvalidModuleError
invalidModuleError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (
                        Core.Name "conflictingVariantName",
                        (\input -> Eithers.map (\t -> ErrorPackaging.InvalidModuleErrorConflictingVariantName t) (conflictingVariantNameError cx input))),
                      (
                        Core.Name "definitionNotInModuleName",
                        (\input -> Eithers.map (\t -> ErrorPackaging.InvalidModuleErrorDefinitionNotInModuleName t) (definitionNotInModuleNameError cx input))),
                      (
                        Core.Name "definitionsOutOfOrder",
                        (\input -> Eithers.map (\t -> ErrorPackaging.InvalidModuleErrorDefinitionsOutOfOrder t) (definitionsOutOfOrderError cx input))),
                      (
                        Core.Name "duplicateDefinitionName",
                        (\input -> Eithers.map (\t -> ErrorPackaging.InvalidModuleErrorDuplicateDefinitionName t) (duplicateDefinitionNameError cx input))),
                      (
                        Core.Name "invalidDefinitionName",
                        (\input -> Eithers.map (\t -> ErrorPackaging.InvalidModuleErrorInvalidDefinitionName t) (invalidDefinitionNameError cx input))),
                      (
                        Core.Name "invalidModuleNameConvention",
                        (\input -> Eithers.map (\t -> ErrorPackaging.InvalidModuleErrorInvalidModuleNameConvention t) (invalidModuleNameConventionError cx input))),
                      (
                        Core.Name "missingDocumentation",
                        (\input -> Eithers.map (\t -> ErrorPackaging.InvalidModuleErrorMissingDocumentation t) (missingDocumentationError cx input)))]
        in (Optionals.cases (Maps.lookup fname variantMap) (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)

-- | Decoder for hydra.error.packaging.InvalidModuleNameConventionError
invalidModuleNameConventionError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorPackaging.InvalidModuleNameConventionError
invalidModuleNameConventionError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "moduleName" DecodePackaging.moduleName fieldMap cx) (\field_moduleName -> Right (ErrorPackaging.InvalidModuleNameConventionError {
          ErrorPackaging.invalidModuleNameConventionErrorModuleName = field_moduleName})))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.error.packaging.InvalidModuleNameConventionError")) (ExtractCore.stripWithDecodingError cx raw)

-- | Decoder for hydra.error.packaging.InvalidPackageError
invalidPackageError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorPackaging.InvalidPackageError
invalidPackageError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (
                        Core.Name "conflictingModuleName",
                        (\input -> Eithers.map (\t -> ErrorPackaging.InvalidPackageErrorConflictingModuleName t) (conflictingModuleNameError cx input))),
                      (
                        Core.Name "duplicateModuleName",
                        (\input -> Eithers.map (\t -> ErrorPackaging.InvalidPackageErrorDuplicateModuleName t) (duplicateModuleNameError cx input))),
                      (
                        Core.Name "invalidModule",
                        (\input -> Eithers.map (\t -> ErrorPackaging.InvalidPackageErrorInvalidModule t) (invalidModuleError cx input))),
                      (
                        Core.Name "invalidPackageName",
                        (\input -> Eithers.map (\t -> ErrorPackaging.InvalidPackageErrorInvalidPackageName t) (invalidPackageNameError cx input))),
                      (
                        Core.Name "undeclaredDependency",
                        (\input -> Eithers.map (\t -> ErrorPackaging.InvalidPackageErrorUndeclaredDependency t) (undeclaredDependencyError cx input)))]
        in (Optionals.cases (Maps.lookup fname variantMap) (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)

-- | Decoder for hydra.error.packaging.InvalidPackageNameError
invalidPackageNameError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorPackaging.InvalidPackageNameError
invalidPackageNameError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "packageName" DecodePackaging.packageName fieldMap cx) (\field_packageName -> Right (ErrorPackaging.InvalidPackageNameError {
          ErrorPackaging.invalidPackageNameErrorPackageName = field_packageName})))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.error.packaging.InvalidPackageNameError")) (ExtractCore.stripWithDecodingError cx raw)

-- | Decoder for hydra.error.packaging.MissingDocumentationError
missingDocumentationError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorPackaging.MissingDocumentationError
missingDocumentationError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "moduleName" DecodePackaging.moduleName fieldMap cx) (\field_moduleName -> Eithers.bind (ExtractCore.requireField "name" DecodeCore.name fieldMap cx) (\field_name -> Right (ErrorPackaging.MissingDocumentationError {
          ErrorPackaging.missingDocumentationErrorModuleName = field_moduleName,
          ErrorPackaging.missingDocumentationErrorName = field_name}))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.error.packaging.MissingDocumentationError")) (ExtractCore.stripWithDecodingError cx raw)

-- | Decoder for hydra.error.packaging.UndeclaredDependencyError
undeclaredDependencyError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ErrorPackaging.UndeclaredDependencyError
undeclaredDependencyError cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "moduleName" DecodePackaging.moduleName fieldMap cx) (\field_moduleName -> Eithers.bind (ExtractCore.requireField "referencedName" DecodeCore.name fieldMap cx) (\field_referencedName -> Eithers.bind (ExtractCore.requireField "owningModuleName" DecodePackaging.moduleName fieldMap cx) (\field_owningModuleName -> Right (ErrorPackaging.UndeclaredDependencyError {
          ErrorPackaging.undeclaredDependencyErrorModuleName = field_moduleName,
          ErrorPackaging.undeclaredDependencyErrorReferencedName = field_referencedName,
          ErrorPackaging.undeclaredDependencyErrorOwningModuleName = field_owningModuleName})))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.error.packaging.UndeclaredDependencyError")) (ExtractCore.stripWithDecodingError cx raw)
