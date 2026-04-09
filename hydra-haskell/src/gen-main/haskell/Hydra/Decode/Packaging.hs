-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.packaging

module Hydra.Decode.Packaging where

import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as Core__
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Packaging as Packaging
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

definition :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Packaging.Definition
definition cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermUnion v0 ->
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
      _ -> Left (Errors.DecodingError "expected union")) (Core__.stripWithDecodingError cx raw)

fileExtension :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Packaging.FileExtension
fileExtension cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Packaging.FileExtension b) ((\raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected string literal")
        _ -> Left (Errors.DecodingError "expected literal")) (Core__.stripWithDecodingError cx raw2)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (Core__.stripWithDecodingError cx raw)

module_ :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Packaging.Module
module_ cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Core__.toFieldMap v0
        in (Eithers.bind (Core__.requireField "namespace" namespace fieldMap cx) (\field_namespace -> Eithers.bind (Core__.requireField "definitions" (Core__.decodeList definition) fieldMap cx) (\field_definitions -> Eithers.bind (Core__.requireField "termDependencies" (Core__.decodeList namespace) fieldMap cx) (\field_termDependencies -> Eithers.bind (Core__.requireField "typeDependencies" (Core__.decodeList namespace) fieldMap cx) (\field_typeDependencies -> Eithers.bind (Core__.requireField "description" (Core__.decodeMaybe (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (Core__.stripWithDecodingError cx2 raw2))) fieldMap cx) (\field_description -> Right (Packaging.Module {
          Packaging.moduleNamespace = field_namespace,
          Packaging.moduleDefinitions = field_definitions,
          Packaging.moduleTermDependencies = field_termDependencies,
          Packaging.moduleTypeDependencies = field_typeDependencies,
          Packaging.moduleDescription = field_description})))))))
      _ -> Left (Errors.DecodingError "expected record")) (Core__.stripWithDecodingError cx raw)

namespace :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Packaging.Namespace
namespace cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Packaging.Namespace b) ((\raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected string literal")
        _ -> Left (Errors.DecodingError "expected literal")) (Core__.stripWithDecodingError cx raw2)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (Core__.stripWithDecodingError cx raw)

namespaces :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Packaging.Namespaces t0)
namespaces n cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Core__.toFieldMap v0
        in (Eithers.bind (Core__.requireField "focus" (Core__.decodePair namespace n) fieldMap cx) (\field_focus -> Eithers.bind (Core__.requireField "mapping" (Core__.decodeMap namespace n) fieldMap cx) (\field_mapping -> Right (Packaging.Namespaces {
          Packaging.namespacesFocus = field_focus,
          Packaging.namespacesMapping = field_mapping}))))
      _ -> Left (Errors.DecodingError "expected record")) (Core__.stripWithDecodingError cx raw)

package :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Packaging.Package
package cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Core__.toFieldMap v0
        in (Eithers.bind (Core__.requireField "name" packageName fieldMap cx) (\field_name -> Eithers.bind (Core__.requireField "modules" (Core__.decodeList module_) fieldMap cx) (\field_modules -> Eithers.bind (Core__.requireField "dependencies" (Core__.decodeList packageName) fieldMap cx) (\field_dependencies -> Eithers.bind (Core__.requireField "description" (Core__.decodeMaybe (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (Core__.stripWithDecodingError cx2 raw2))) fieldMap cx) (\field_description -> Right (Packaging.Package {
          Packaging.packageName = field_name,
          Packaging.packageModules = field_modules,
          Packaging.packageDependencies = field_dependencies,
          Packaging.packageDescription = field_description}))))))
      _ -> Left (Errors.DecodingError "expected record")) (Core__.stripWithDecodingError cx raw)

packageName :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Packaging.PackageName
packageName cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Packaging.PackageName b) ((\raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected string literal")
        _ -> Left (Errors.DecodingError "expected literal")) (Core__.stripWithDecodingError cx raw2)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (Core__.stripWithDecodingError cx raw)

qualifiedName :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Packaging.QualifiedName
qualifiedName cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Core__.toFieldMap v0
        in (Eithers.bind (Core__.requireField "namespace" (Core__.decodeMaybe namespace) fieldMap cx) (\field_namespace -> Eithers.bind (Core__.requireField "local" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (Core__.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_local -> Right (Packaging.QualifiedName {
          Packaging.qualifiedNameNamespace = field_namespace,
          Packaging.qualifiedNameLocal = field_local}))))
      _ -> Left (Errors.DecodingError "expected record")) (Core__.stripWithDecodingError cx raw)

termDefinition :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Packaging.TermDefinition
termDefinition cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Core__.toFieldMap v0
        in (Eithers.bind (Core__.requireField "name" Core_.name fieldMap cx) (\field_name -> Eithers.bind (Core__.requireField "term" Core_.term fieldMap cx) (\field_term -> Eithers.bind (Core__.requireField "type" (Core__.decodeMaybe Core_.typeScheme) fieldMap cx) (\field_type -> Right (Packaging.TermDefinition {
          Packaging.termDefinitionName = field_name,
          Packaging.termDefinitionTerm = field_term,
          Packaging.termDefinitionType = field_type})))))
      _ -> Left (Errors.DecodingError "expected record")) (Core__.stripWithDecodingError cx raw)

typeDefinition :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Packaging.TypeDefinition
typeDefinition cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Core__.toFieldMap v0
        in (Eithers.bind (Core__.requireField "name" Core_.name fieldMap cx) (\field_name -> Eithers.bind (Core__.requireField "type" Core_.typeScheme fieldMap cx) (\field_type -> Right (Packaging.TypeDefinition {
          Packaging.typeDefinitionName = field_name,
          Packaging.typeDefinitionType = field_type}))))
      _ -> Left (Errors.DecodingError "expected record")) (Core__.stripWithDecodingError cx raw)
