-- Note: this is an automatically generated file. Do not edit.

-- | Term encoders for hydra.module

module Hydra.Encode.Module where

import qualified Hydra.Core as Core
import qualified Hydra.Encode.Core as Core_
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Module as Module
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

definition :: (Module.Definition -> Core.Term)
definition x = case x of
  Module.DefinitionTerm v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.module.Definition"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "term"),
      Core.fieldTerm = (termDefinition v1)}}))
  Module.DefinitionType v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.module.Definition"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (typeDefinition v1)}}))

fileExtension :: (Module.FileExtension -> Core.Term)
fileExtension x = (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.module.FileExtension"),
  Core.wrappedTermBody = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Module.unFileExtension x))}))

module_ :: (Module.Module -> Core.Term)
module_ x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.module.Module"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "namespace"),
      Core.fieldTerm = (namespace (Module.moduleNamespace x))},
    Core.Field {
      Core.fieldName = (Core.Name "elements"),
      Core.fieldTerm = ((\xs -> Core.TermList (Lists.map Core_.binding xs)) (Module.moduleElements x))},
    Core.Field {
      Core.fieldName = (Core.Name "termDependencies"),
      Core.fieldTerm = ((\xs -> Core.TermList (Lists.map namespace xs)) (Module.moduleTermDependencies x))},
    Core.Field {
      Core.fieldName = (Core.Name "typeDependencies"),
      Core.fieldTerm = ((\xs -> Core.TermList (Lists.map namespace xs)) (Module.moduleTypeDependencies x))},
    Core.Field {
      Core.fieldName = (Core.Name "description"),
      Core.fieldTerm = ((\opt -> Core.TermMaybe (Maybes.map (\x -> Core.TermLiteral (Core.LiteralString x)) opt)) (Module.moduleDescription x))}]}))

namespace :: (Module.Namespace -> Core.Term)
namespace x = (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.module.Namespace"),
  Core.wrappedTermBody = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Module.unNamespace x))}))

namespaces :: ((t0 -> Core.Term) -> Module.Namespaces t0 -> Core.Term)
namespaces n x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.module.Namespaces"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "focus"),
      Core.fieldTerm = ((\p -> Core.TermPair (Pairs.bimap namespace n p)) (Module.namespacesFocus x))},
    Core.Field {
      Core.fieldName = (Core.Name "mapping"),
      Core.fieldTerm = ((\m -> Core.TermMap (Maps.bimap namespace n m)) (Module.namespacesMapping x))}]}))

qualifiedName :: (Module.QualifiedName -> Core.Term)
qualifiedName x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.module.QualifiedName"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "namespace"),
      Core.fieldTerm = ((\opt -> Core.TermMaybe (Maybes.map namespace opt)) (Module.qualifiedNameNamespace x))},
    Core.Field {
      Core.fieldName = (Core.Name "local"),
      Core.fieldTerm = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Module.qualifiedNameLocal x))}]}))

termDefinition :: (Module.TermDefinition -> Core.Term)
termDefinition x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.module.TermDefinition"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core_.name (Module.termDefinitionName x))},
    Core.Field {
      Core.fieldName = (Core.Name "term"),
      Core.fieldTerm = (Core_.term (Module.termDefinitionTerm x))},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core_.typeScheme (Module.termDefinitionType x))}]}))

typeDefinition :: (Module.TypeDefinition -> Core.Term)
typeDefinition x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.module.TypeDefinition"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core_.name (Module.typeDefinitionName x))},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core_.type_ (Module.typeDefinitionType x))}]}))
