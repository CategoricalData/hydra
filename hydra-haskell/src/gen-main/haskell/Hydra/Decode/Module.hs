-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.module

module Hydra.Decode.Module where

import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Module as Module
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
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
      Core.unName fname,
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
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_namespace -> Eithers.either (\err -> Left err) (\field_elements -> Eithers.either (\err -> Left err) (\field_termDependencies -> Eithers.either (\err -> Left err) (\field_typeDependencies -> Eithers.either (\err -> Left err) (\field_description -> Right (Module.Module {
      Module.moduleNamespace = field_namespace,
      Module.moduleElements = field_elements,
      Module.moduleTermDependencies = field_termDependencies,
      Module.moduleTypeDependencies = field_typeDependencies,
      Module.moduleDescription = field_description})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "description",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermMaybe v2 -> (Eithers.mapMaybe (\raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
        Core.TermLiteral v3 -> ((\x -> case x of
          Core.LiteralString v4 -> (Right v4)
          _ -> (Left (Util.DecodingError "expected string literal"))) v3)
        _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) v2)
      _ -> (Left (Util.DecodingError "expected optional value"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "description") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "typeDependencies",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermList v2 -> (Eithers.mapList (namespace cx) v2)
      _ -> (Left (Util.DecodingError "expected list"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "typeDependencies") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "termDependencies",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermList v2 -> (Eithers.mapList (namespace cx) v2)
      _ -> (Left (Util.DecodingError "expected list"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "termDependencies") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "elements",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermList v2 -> (Eithers.mapList (Core_.binding cx) v2)
      _ -> (Left (Util.DecodingError "expected list"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "elements") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "namespace",
      " in record"]))) (\fieldTerm -> namespace cx fieldTerm) (Maps.lookup (Core.Name "namespace") fieldMap)))
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
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_focus -> Eithers.either (\err -> Left err) (\field_mapping -> Right (Module.Namespaces {
      Module.namespacesFocus = field_focus,
      Module.namespacesMapping = field_mapping})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "mapping",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermMap v2 ->  
        let pairs = (Maps.toList v2) 
            decodePair = (\kv ->  
                    let rawKey = (Pairs.first kv) 
                        rawVal = (Pairs.second kv)
                    in (Eithers.either (\err -> Left err) (\k2 -> Eithers.either (\err2 -> Left err2) (\v2 -> Right (k2, v2)) (n cx rawVal)) (namespace cx rawKey)))
        in (Eithers.either (\err -> Left err) (\decodedPairs -> Right (Maps.fromList decodedPairs)) (Eithers.mapList decodePair pairs))
      _ -> (Left (Util.DecodingError "expected map"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "mapping") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "focus",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermPair v2 ->  
        let rawFirst = (Pairs.first v2) 
            rawSecond = (Pairs.second v2)
        in (Eithers.either (\err -> Left err) (\decodedFirst -> Eithers.either (\err2 -> Left err2) (\decodedSecond -> Right (decodedFirst, decodedSecond)) (n cx rawSecond)) (namespace cx rawFirst))
      _ -> (Left (Util.DecodingError "expected pair"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "focus") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.module.Namespaces"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

qualifiedName :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Module.QualifiedName)
qualifiedName cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_namespace -> Eithers.either (\err -> Left err) (\field_local -> Right (Module.QualifiedName {
      Module.qualifiedNameNamespace = field_namespace,
      Module.qualifiedNameLocal = field_local})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "local",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "local") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "namespace",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermMaybe v2 -> (Eithers.mapMaybe (namespace cx) v2)
      _ -> (Left (Util.DecodingError "expected optional value"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "namespace") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.module.QualifiedName"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

termDefinition :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Module.TermDefinition)
termDefinition cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_name -> Eithers.either (\err -> Left err) (\field_term -> Eithers.either (\err -> Left err) (\field_type -> Right (Module.TermDefinition {
      Module.termDefinitionName = field_name,
      Module.termDefinitionTerm = field_term,
      Module.termDefinitionType = field_type})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "type",
      " in record"]))) (\fieldTerm -> Core_.typeScheme cx fieldTerm) (Maps.lookup (Core.Name "type") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "term",
      " in record"]))) (\fieldTerm -> Core_.term cx fieldTerm) (Maps.lookup (Core.Name "term") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "name",
      " in record"]))) (\fieldTerm -> Core_.name cx fieldTerm) (Maps.lookup (Core.Name "name") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.module.TermDefinition"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

typeDefinition :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Module.TypeDefinition)
typeDefinition cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_name -> Eithers.either (\err -> Left err) (\field_type -> Right (Module.TypeDefinition {
      Module.typeDefinitionName = field_name,
      Module.typeDefinitionType = field_type})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "type",
      " in record"]))) (\fieldTerm -> Core_.type_ cx fieldTerm) (Maps.lookup (Core.Name "type") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "name",
      " in record"]))) (\fieldTerm -> Core_.name cx fieldTerm) (Maps.lookup (Core.Name "name") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.module.TypeDefinition"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))
