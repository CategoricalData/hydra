-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.module

module Hydra.Decode.Module where

import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Extract.Helpers as Helpers
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Module as Module
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

definition :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Module.Definition)
definition cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "term", (\input -> Eithers.map (\t -> Module.DefinitionTerm t) (termDefinition cx input))),
                (Core.Name "type", (\input -> Eithers.map (\t -> Module.DefinitionType t) (typeDefinition cx input)))])
    in (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "no such field ",
      (Core.unName fname),
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util.DecodingError "expected union of type hydra.module.Definition"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

fileExtension :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Module.FileExtension)
fileExtension cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermWrap v1 -> (Eithers.map (\b -> Module.FileExtension b) ((\raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
    Core.TermLiteral v2 -> ((\x -> case x of
      Core.LiteralString v3 -> (Right v3)
      _ -> (Left (Util.DecodingError "expected string literal"))) v2)
    _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) (Core.wrappedTermBody v1)))
  _ -> (Left (Util.DecodingError "expected wrapped type hydra.module.FileExtension"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

module_ :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Module.Module)
module_ cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "namespace" namespace fieldMap cx) (\field_namespace -> Eithers.bind (Helpers.requireField "elements" (Helpers.decodeList Core_.binding) fieldMap cx) (\field_elements -> Eithers.bind (Helpers.requireField "termDependencies" (Helpers.decodeList namespace) fieldMap cx) (\field_termDependencies -> Eithers.bind (Helpers.requireField "typeDependencies" (Helpers.decodeList namespace) fieldMap cx) (\field_typeDependencies -> Eithers.bind (Helpers.requireField "description" (Helpers.decodeMaybe (\cx -> \raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))) fieldMap cx) (\field_description -> Right (Module.Module {
      Module.moduleNamespace = field_namespace,
      Module.moduleElements = field_elements,
      Module.moduleTermDependencies = field_termDependencies,
      Module.moduleTypeDependencies = field_typeDependencies,
      Module.moduleDescription = field_description})))))))
  _ -> (Left (Util.DecodingError "expected record of type hydra.module.Module"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

namespace :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Module.Namespace)
namespace cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermWrap v1 -> (Eithers.map (\b -> Module.Namespace b) ((\raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
    Core.TermLiteral v2 -> ((\x -> case x of
      Core.LiteralString v3 -> (Right v3)
      _ -> (Left (Util.DecodingError "expected string literal"))) v2)
    _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) (Core.wrappedTermBody v1)))
  _ -> (Left (Util.DecodingError "expected wrapped type hydra.module.Namespace"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

namespaces :: ((Graph.Graph -> Core.Term -> Either Util.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Util.DecodingError (Module.Namespaces t0))
namespaces n cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "focus" (Helpers.decodePair namespace n) fieldMap cx) (\field_focus -> Eithers.bind (Helpers.requireField "mapping" (Helpers.decodeMap namespace n) fieldMap cx) (\field_mapping -> Right (Module.Namespaces {
      Module.namespacesFocus = field_focus,
      Module.namespacesMapping = field_mapping}))))
  _ -> (Left (Util.DecodingError "expected record of type hydra.module.Namespaces"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

qualifiedName :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Module.QualifiedName)
qualifiedName cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "namespace" (Helpers.decodeMaybe namespace) fieldMap cx) (\field_namespace -> Eithers.bind (Helpers.requireField "local" (\cx -> \raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_local -> Right (Module.QualifiedName {
      Module.qualifiedNameNamespace = field_namespace,
      Module.qualifiedNameLocal = field_local}))))
  _ -> (Left (Util.DecodingError "expected record of type hydra.module.QualifiedName"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

termDefinition :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Module.TermDefinition)
termDefinition cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "name" Core_.name fieldMap cx) (\field_name -> Eithers.bind (Helpers.requireField "term" Core_.term fieldMap cx) (\field_term -> Eithers.bind (Helpers.requireField "type" Core_.typeScheme fieldMap cx) (\field_type -> Right (Module.TermDefinition {
      Module.termDefinitionName = field_name,
      Module.termDefinitionTerm = field_term,
      Module.termDefinitionType = field_type})))))
  _ -> (Left (Util.DecodingError "expected record of type hydra.module.TermDefinition"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

typeDefinition :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Module.TypeDefinition)
typeDefinition cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "name" Core_.name fieldMap cx) (\field_name -> Eithers.bind (Helpers.requireField "type" Core_.type_ fieldMap cx) (\field_type -> Right (Module.TypeDefinition {
      Module.typeDefinitionName = field_name,
      Module.typeDefinitionType = field_type}))))
  _ -> (Left (Util.DecodingError "expected record of type hydra.module.TypeDefinition"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))
