-- Note: this is an automatically generated file. Do not edit.
-- | Term decoders for hydra.packaging

module Hydra.Decode.Packaging where
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Packaging as Packaging
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
                      (Core.Name "type", (\input -> Eithers.map (\t -> Packaging.DefinitionType t) (typeDefinition cx input)))]
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
        in (Eithers.bind (ExtractCore.requireField "description" (ExtractCore.decodeMaybe (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2))) fieldMap cx) (\field_description -> Eithers.bind (ExtractCore.requireField "namespace" namespace fieldMap cx) (\field_namespace -> Eithers.bind (ExtractCore.requireField "dependencies" (ExtractCore.decodeList namespace) fieldMap cx) (\field_dependencies -> Eithers.bind (ExtractCore.requireField "definitions" (ExtractCore.decodeList definition) fieldMap cx) (\field_definitions -> Right (Packaging.Module {
          Packaging.moduleDescription = field_description,
          Packaging.moduleNamespace = field_namespace,
          Packaging.moduleDependencies = field_dependencies,
          Packaging.moduleDefinitions = field_definitions}))))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.packaging.Namespace
namespace :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Packaging.Namespace
namespace cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Packaging.Namespace b) ((\raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
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
        in (Eithers.bind (ExtractCore.requireField "name" packageName fieldMap cx) (\field_name -> Eithers.bind (ExtractCore.requireField "modules" (ExtractCore.decodeList module_) fieldMap cx) (\field_modules -> Eithers.bind (ExtractCore.requireField "dependencies" (ExtractCore.decodeList packageDependency) fieldMap cx) (\field_dependencies -> Eithers.bind (ExtractCore.requireField "description" (ExtractCore.decodeMaybe (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2))) fieldMap cx) (\field_description -> Right (Packaging.Package {
          Packaging.packageName = field_name,
          Packaging.packageModules = field_modules,
          Packaging.packageDependencies = field_dependencies,
          Packaging.packageDescription = field_description}))))))
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
-- | Decoder for hydra.packaging.QualifiedName
qualifiedName :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Packaging.QualifiedName
qualifiedName cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "namespace" (ExtractCore.decodeMaybe namespace) fieldMap cx) (\field_namespace -> Eithers.bind (ExtractCore.requireField "local" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_local -> Right (Packaging.QualifiedName {
          Packaging.qualifiedNameNamespace = field_namespace,
          Packaging.qualifiedNameLocal = field_local}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.packaging.TermDefinition
termDefinition :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Packaging.TermDefinition
termDefinition cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "name" DecodeCore.name fieldMap cx) (\field_name -> Eithers.bind (ExtractCore.requireField "term" DecodeCore.term fieldMap cx) (\field_term -> Eithers.bind (ExtractCore.requireField "typeScheme" (ExtractCore.decodeMaybe DecodeCore.typeScheme) fieldMap cx) (\field_typeScheme -> Right (Packaging.TermDefinition {
          Packaging.termDefinitionName = field_name,
          Packaging.termDefinitionTerm = field_term,
          Packaging.termDefinitionTypeScheme = field_typeScheme})))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.packaging.TypeDefinition
typeDefinition :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Packaging.TypeDefinition
typeDefinition cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "name" DecodeCore.name fieldMap cx) (\field_name -> Eithers.bind (ExtractCore.requireField "typeScheme" DecodeCore.typeScheme fieldMap cx) (\field_typeScheme -> Right (Packaging.TypeDefinition {
          Packaging.typeDefinitionName = field_name,
          Packaging.typeDefinitionTypeScheme = field_typeScheme}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
