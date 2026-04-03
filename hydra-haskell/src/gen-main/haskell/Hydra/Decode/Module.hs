-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.module

module Hydra.Decode.Module where

import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as Core__
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Module as Module
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

definition :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Module.Definition
definition cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermUnion v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "term", (\input -> Eithers.map (\t -> Module.DefinitionTerm t) (termDefinition cx input))),
                      (Core.Name "type", (\input -> Eithers.map (\t -> Module.DefinitionType t) (typeDefinition cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (Lexical.stripAndDereferenceTermEither cx raw)

fileExtension :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Module.FileExtension
fileExtension cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Module.FileExtension b) ((\raw2 -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped2 -> case stripped2 of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected string literal")
        _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx raw2)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (Lexical.stripAndDereferenceTermEither cx raw)

module_ :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Module.Module
module_ cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Core__.toFieldMap v0
        in (Eithers.bind (Core__.requireField "namespace" namespace fieldMap cx) (\field_namespace -> Eithers.bind (Core__.requireField "definitions" (Core__.decodeList definition) fieldMap cx) (\field_definitions -> Eithers.bind (Core__.requireField "termDependencies" (Core__.decodeList namespace) fieldMap cx) (\field_termDependencies -> Eithers.bind (Core__.requireField "typeDependencies" (Core__.decodeList namespace) fieldMap cx) (\field_typeDependencies -> Eithers.bind (Core__.requireField "description" (Core__.decodeMaybe (\cx2 -> \raw2 -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx2 raw2))) fieldMap cx) (\field_description -> Right (Module.Module {
          Module.moduleNamespace = field_namespace,
          Module.moduleDefinitions = field_definitions,
          Module.moduleTermDependencies = field_termDependencies,
          Module.moduleTypeDependencies = field_typeDependencies,
          Module.moduleDescription = field_description})))))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

namespace :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Module.Namespace
namespace cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Module.Namespace b) ((\raw2 -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped2 -> case stripped2 of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected string literal")
        _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx raw2)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (Lexical.stripAndDereferenceTermEither cx raw)

namespaces :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Module.Namespaces t0)
namespaces n cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Core__.toFieldMap v0
        in (Eithers.bind (Core__.requireField "focus" (Core__.decodePair namespace n) fieldMap cx) (\field_focus -> Eithers.bind (Core__.requireField "mapping" (Core__.decodeMap namespace n) fieldMap cx) (\field_mapping -> Right (Module.Namespaces {
          Module.namespacesFocus = field_focus,
          Module.namespacesMapping = field_mapping}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

qualifiedName :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Module.QualifiedName
qualifiedName cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Core__.toFieldMap v0
        in (Eithers.bind (Core__.requireField "namespace" (Core__.decodeMaybe namespace) fieldMap cx) (\field_namespace -> Eithers.bind (Core__.requireField "local" (\cx2 -> \raw2 -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx2 raw2)) fieldMap cx) (\field_local -> Right (Module.QualifiedName {
          Module.qualifiedNameNamespace = field_namespace,
          Module.qualifiedNameLocal = field_local}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

termDefinition :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Module.TermDefinition
termDefinition cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Core__.toFieldMap v0
        in (Eithers.bind (Core__.requireField "name" Core_.name fieldMap cx) (\field_name -> Eithers.bind (Core__.requireField "term" Core_.term fieldMap cx) (\field_term -> Eithers.bind (Core__.requireField "type" (Core__.decodeMaybe Core_.typeScheme) fieldMap cx) (\field_type -> Right (Module.TermDefinition {
          Module.termDefinitionName = field_name,
          Module.termDefinitionTerm = field_term,
          Module.termDefinitionType = field_type})))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

typeDefinition :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Module.TypeDefinition
typeDefinition cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Core__.toFieldMap v0
        in (Eithers.bind (Core__.requireField "name" Core_.name fieldMap cx) (\field_name -> Eithers.bind (Core__.requireField "type" Core_.type_ fieldMap cx) (\field_type -> Right (Module.TypeDefinition {
          Module.typeDefinitionName = field_name,
          Module.typeDefinitionType = field_type}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)
