-- Note: this is an automatically generated file. Do not edit.
-- | Term decoders for hydra.packaging

module Hydra.Decode.Packaging where
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Decode.Typing as Typing
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Haskell.Lib.Maps as Maps
import qualified Hydra.Haskell.Lib.Maybes as Maybes
import qualified Hydra.Haskell.Lib.Strings as Strings
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Decoder for hydra.packaging.Definition
definition :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Packaging.Definition
definition cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "term", (\input -> Eithers.map (\t -> Packaging.DefinitionTerm t) (termDefinition cx input))),
                      (Core.Name "type", (\input -> Eithers.map (\t -> Packaging.DefinitionType t) (typeDefinition cx input))),
                      (Core.Name "primitive", (\input -> Eithers.map (\t -> Packaging.DefinitionPrimitive t) (primitiveDefinition cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.packaging.DefinitionReference
definitionReference :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Packaging.DefinitionReference
definitionReference cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "type", (\input -> Eithers.map (\t -> Packaging.DefinitionReferenceType t) (DecodeCore.name cx input))),
                      (Core.Name "term", (\input -> Eithers.map (\t -> Packaging.DefinitionReferenceTerm t) (DecodeCore.name cx input))),
                      (
                        Core.Name "primitive",
                        (\input -> Eithers.map (\t -> Packaging.DefinitionReferencePrimitive t) (DecodeCore.name cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.packaging.EntityLifecycle
entityLifecycle :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Packaging.EntityLifecycle
entityLifecycle cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "availableSince" (ExtractCore.decodeMaybe version) fieldMap cx) (\field_availableSince -> Eithers.bind (ExtractCore.requireField "deprecatedSince" (ExtractCore.decodeMaybe version) fieldMap cx) (\field_deprecatedSince -> Right (Packaging.EntityLifecycle {
          Packaging.entityLifecycleAvailableSince = field_availableSince,
          Packaging.entityLifecycleDeprecatedSince = field_deprecatedSince}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.packaging.EntityMetadata
entityMetadata :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Packaging.EntityMetadata
entityMetadata cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "description" (ExtractCore.decodeMaybe (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2))) fieldMap cx) (\field_description -> Eithers.bind (ExtractCore.requireField "comments" (ExtractCore.decodeList (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2))) fieldMap cx) (\field_comments -> Eithers.bind (ExtractCore.requireField "seeAlso" (ExtractCore.decodeList entityReference) fieldMap cx) (\field_seeAlso -> Eithers.bind (ExtractCore.requireField "lifecycle" (ExtractCore.decodeMaybe entityLifecycle) fieldMap cx) (\field_lifecycle -> Right (Packaging.EntityMetadata {
          Packaging.entityMetadataDescription = field_description,
          Packaging.entityMetadataComments = field_comments,
          Packaging.entityMetadataSeeAlso = field_seeAlso,
          Packaging.entityMetadataLifecycle = field_lifecycle}))))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.packaging.EntityReference
entityReference :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Packaging.EntityReference
entityReference cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "package", (\input -> Eithers.map (\t -> Packaging.EntityReferencePackage t) (packageName cx input))),
                      (Core.Name "module", (\input -> Eithers.map (\t -> Packaging.EntityReferenceModule t) (moduleName cx input))),
                      (
                        Core.Name "definition",
                        (\input -> Eithers.map (\t -> Packaging.EntityReferenceDefinition t) (definitionReference cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.packaging.FileExtension
fileExtension :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Packaging.FileExtension
fileExtension cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Packaging.FileExtension b) ((\raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected string literal")
        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx raw2)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.packaging.Module
module_ :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Packaging.Module
module_ cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "name" moduleName fieldMap cx) (\field_name -> Eithers.bind (ExtractCore.requireField "metadata" (ExtractCore.decodeMaybe entityMetadata) fieldMap cx) (\field_metadata -> Eithers.bind (ExtractCore.requireField "dependencies" (ExtractCore.decodeList moduleDependency) fieldMap cx) (\field_dependencies -> Eithers.bind (ExtractCore.requireField "definitions" (ExtractCore.decodeList definition) fieldMap cx) (\field_definitions -> Right (Packaging.Module {
          Packaging.moduleName = field_name,
          Packaging.moduleMetadata = field_metadata,
          Packaging.moduleDependencies = field_dependencies,
          Packaging.moduleDefinitions = field_definitions}))))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.packaging.ModuleDependency
moduleDependency :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Packaging.ModuleDependency
moduleDependency cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "module" moduleName fieldMap cx) (\field_module -> Eithers.bind (ExtractCore.requireField "package" (ExtractCore.decodeMaybe packageName) fieldMap cx) (\field_package -> Right (Packaging.ModuleDependency {
          Packaging.moduleDependencyModule = field_module,
          Packaging.moduleDependencyPackage = field_package}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.packaging.ModuleName
moduleName :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Packaging.ModuleName
moduleName cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Packaging.ModuleName b) ((\raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected string literal")
        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx raw2)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.packaging.Package
package :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Packaging.Package
package cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "name" packageName fieldMap cx) (\field_name -> Eithers.bind (ExtractCore.requireField "metadata" (ExtractCore.decodeMaybe entityMetadata) fieldMap cx) (\field_metadata -> Eithers.bind (ExtractCore.requireField "dependencies" (ExtractCore.decodeList packageDependency) fieldMap cx) (\field_dependencies -> Eithers.bind (ExtractCore.requireField "modules" (ExtractCore.decodeList module_) fieldMap cx) (\field_modules -> Right (Packaging.Package {
          Packaging.packageName = field_name,
          Packaging.packageMetadata = field_metadata,
          Packaging.packageDependencies = field_dependencies,
          Packaging.packageModules = field_modules}))))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.packaging.PackageDependency
packageDependency :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Packaging.PackageDependency
packageDependency cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "name" packageName fieldMap cx) (\field_name -> Eithers.bind (ExtractCore.requireField "version" packageVersionSpecifier fieldMap cx) (\field_version -> Right (Packaging.PackageDependency {
          Packaging.packageDependencyName = field_name,
          Packaging.packageDependencyVersion = field_version}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.packaging.PackageName
packageName :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Packaging.PackageName
packageName cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Packaging.PackageName b) ((\raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected string literal")
        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx raw2)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.packaging.PackageVersionSpecifier
packageVersionSpecifier :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Packaging.PackageVersionSpecifier
packageVersionSpecifier cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "any", (\input -> Eithers.map (\t -> Packaging.PackageVersionSpecifierAny) (ExtractCore.decodeUnit cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.packaging.PrimitiveDefinition
primitiveDefinition :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Packaging.PrimitiveDefinition
primitiveDefinition cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "name" DecodeCore.name fieldMap cx) (\field_name -> Eithers.bind (ExtractCore.requireField "signature" Typing.termSignature fieldMap cx) (\field_signature -> Eithers.bind (ExtractCore.requireField "metadata" (ExtractCore.decodeMaybe entityMetadata) fieldMap cx) (\field_metadata -> Eithers.bind (ExtractCore.requireField "isPure" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralBoolean v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected boolean literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_isPure -> Eithers.bind (ExtractCore.requireField "isTotal" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralBoolean v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected boolean literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_isTotal -> Eithers.bind (ExtractCore.requireField "defaultImplementation" (ExtractCore.decodeMaybe DecodeCore.term) fieldMap cx) (\field_defaultImplementation -> Right (Packaging.PrimitiveDefinition {
          Packaging.primitiveDefinitionName = field_name,
          Packaging.primitiveDefinitionSignature = field_signature,
          Packaging.primitiveDefinitionMetadata = field_metadata,
          Packaging.primitiveDefinitionIsPure = field_isPure,
          Packaging.primitiveDefinitionIsTotal = field_isTotal,
          Packaging.primitiveDefinitionDefaultImplementation = field_defaultImplementation}))))))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.packaging.QualifiedName
qualifiedName :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Packaging.QualifiedName
qualifiedName cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "moduleName" (ExtractCore.decodeMaybe moduleName) fieldMap cx) (\field_moduleName -> Eithers.bind (ExtractCore.requireField "local" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_local -> Right (Packaging.QualifiedName {
          Packaging.qualifiedNameModuleName = field_moduleName,
          Packaging.qualifiedNameLocal = field_local}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.packaging.TermDefinition
termDefinition :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Packaging.TermDefinition
termDefinition cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "name" DecodeCore.name fieldMap cx) (\field_name -> Eithers.bind (ExtractCore.requireField "metadata" (ExtractCore.decodeMaybe entityMetadata) fieldMap cx) (\field_metadata -> Eithers.bind (ExtractCore.requireField "term" DecodeCore.term fieldMap cx) (\field_term -> Eithers.bind (ExtractCore.requireField "signature" (ExtractCore.decodeMaybe Typing.termSignature) fieldMap cx) (\field_signature -> Right (Packaging.TermDefinition {
          Packaging.termDefinitionName = field_name,
          Packaging.termDefinitionMetadata = field_metadata,
          Packaging.termDefinitionTerm = field_term,
          Packaging.termDefinitionSignature = field_signature}))))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.packaging.TypeDefinition
typeDefinition :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Packaging.TypeDefinition
typeDefinition cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "name" DecodeCore.name fieldMap cx) (\field_name -> Eithers.bind (ExtractCore.requireField "metadata" (ExtractCore.decodeMaybe entityMetadata) fieldMap cx) (\field_metadata -> Eithers.bind (ExtractCore.requireField "typeScheme" DecodeCore.typeScheme fieldMap cx) (\field_typeScheme -> Right (Packaging.TypeDefinition {
          Packaging.typeDefinitionName = field_name,
          Packaging.typeDefinitionMetadata = field_metadata,
          Packaging.typeDefinitionTypeScheme = field_typeScheme})))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.packaging.Version
version :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Packaging.Version
version cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Packaging.Version b) ((\raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected string literal")
        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx raw2)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)
