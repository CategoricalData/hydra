-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.module

module Hydra.Dsl.Module where

import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Hydra.Module as Module
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

definitionTerm :: (Module.TermDefinition -> Module.Definition)
definitionTerm x = (Module.DefinitionTerm x)

definitionType :: (Module.TypeDefinition -> Module.Definition)
definitionType x = (Module.DefinitionType x)

fileExtension :: (String -> Module.FileExtension)
fileExtension x = (Module.FileExtension x)

unFileExtension :: (Module.FileExtension -> String)
unFileExtension = Module.unFileExtension

library :: (Module.Namespace -> String -> [Graph.Primitive] -> Module.Library)
library namespace prefix primitives = Module.Library {
  Module.libraryNamespace = namespace,
  Module.libraryPrefix = prefix,
  Module.libraryPrimitives = primitives}

libraryNamespace :: (Module.Library -> Module.Namespace)
libraryNamespace = Module.libraryNamespace

libraryPrefix :: (Module.Library -> String)
libraryPrefix = Module.libraryPrefix

libraryPrimitives :: (Module.Library -> [Graph.Primitive])
libraryPrimitives = Module.libraryPrimitives

libraryWithNamespace :: (Module.Library -> Module.Namespace -> Module.Library)
libraryWithNamespace original newVal = Module.Library {
  Module.libraryNamespace = newVal,
  Module.libraryPrefix = (Module.libraryPrefix original),
  Module.libraryPrimitives = (Module.libraryPrimitives original)}

libraryWithPrefix :: (Module.Library -> String -> Module.Library)
libraryWithPrefix original newVal = Module.Library {
  Module.libraryNamespace = (Module.libraryNamespace original),
  Module.libraryPrefix = newVal,
  Module.libraryPrimitives = (Module.libraryPrimitives original)}

libraryWithPrimitives :: (Module.Library -> [Graph.Primitive] -> Module.Library)
libraryWithPrimitives original newVal = Module.Library {
  Module.libraryNamespace = (Module.libraryNamespace original),
  Module.libraryPrefix = (Module.libraryPrefix original),
  Module.libraryPrimitives = newVal}

module_ :: (Module.Namespace -> [Core.Binding] -> [Module.Namespace] -> [Module.Namespace] -> Maybe String -> Module.Module)
module_ namespace elements termDependencies typeDependencies description = Module.Module {
  Module.moduleNamespace = namespace,
  Module.moduleElements = elements,
  Module.moduleTermDependencies = termDependencies,
  Module.moduleTypeDependencies = typeDependencies,
  Module.moduleDescription = description}

moduleNamespace :: (Module.Module -> Module.Namespace)
moduleNamespace = Module.moduleNamespace

moduleElements :: (Module.Module -> [Core.Binding])
moduleElements = Module.moduleElements

moduleTermDependencies :: (Module.Module -> [Module.Namespace])
moduleTermDependencies = Module.moduleTermDependencies

moduleTypeDependencies :: (Module.Module -> [Module.Namespace])
moduleTypeDependencies = Module.moduleTypeDependencies

moduleDescription :: (Module.Module -> Maybe String)
moduleDescription = Module.moduleDescription

moduleWithNamespace :: (Module.Module -> Module.Namespace -> Module.Module)
moduleWithNamespace original newVal = Module.Module {
  Module.moduleNamespace = newVal,
  Module.moduleElements = (Module.moduleElements original),
  Module.moduleTermDependencies = (Module.moduleTermDependencies original),
  Module.moduleTypeDependencies = (Module.moduleTypeDependencies original),
  Module.moduleDescription = (Module.moduleDescription original)}

moduleWithElements :: (Module.Module -> [Core.Binding] -> Module.Module)
moduleWithElements original newVal = Module.Module {
  Module.moduleNamespace = (Module.moduleNamespace original),
  Module.moduleElements = newVal,
  Module.moduleTermDependencies = (Module.moduleTermDependencies original),
  Module.moduleTypeDependencies = (Module.moduleTypeDependencies original),
  Module.moduleDescription = (Module.moduleDescription original)}

moduleWithTermDependencies :: (Module.Module -> [Module.Namespace] -> Module.Module)
moduleWithTermDependencies original newVal = Module.Module {
  Module.moduleNamespace = (Module.moduleNamespace original),
  Module.moduleElements = (Module.moduleElements original),
  Module.moduleTermDependencies = newVal,
  Module.moduleTypeDependencies = (Module.moduleTypeDependencies original),
  Module.moduleDescription = (Module.moduleDescription original)}

moduleWithTypeDependencies :: (Module.Module -> [Module.Namespace] -> Module.Module)
moduleWithTypeDependencies original newVal = Module.Module {
  Module.moduleNamespace = (Module.moduleNamespace original),
  Module.moduleElements = (Module.moduleElements original),
  Module.moduleTermDependencies = (Module.moduleTermDependencies original),
  Module.moduleTypeDependencies = newVal,
  Module.moduleDescription = (Module.moduleDescription original)}

moduleWithDescription :: (Module.Module -> Maybe String -> Module.Module)
moduleWithDescription original newVal = Module.Module {
  Module.moduleNamespace = (Module.moduleNamespace original),
  Module.moduleElements = (Module.moduleElements original),
  Module.moduleTermDependencies = (Module.moduleTermDependencies original),
  Module.moduleTypeDependencies = (Module.moduleTypeDependencies original),
  Module.moduleDescription = newVal}

namespace :: (String -> Module.Namespace)
namespace x = (Module.Namespace x)

unNamespace :: (Module.Namespace -> String)
unNamespace = Module.unNamespace

namespaces :: ((Module.Namespace, t0) -> M.Map Module.Namespace t0 -> Module.Namespaces t0)
namespaces focus mapping = Module.Namespaces {
  Module.namespacesFocus = focus,
  Module.namespacesMapping = mapping}

namespacesFocus :: (Module.Namespaces t0 -> (Module.Namespace, t0))
namespacesFocus = Module.namespacesFocus

namespacesMapping :: (Module.Namespaces t0 -> M.Map Module.Namespace t0)
namespacesMapping = Module.namespacesMapping

namespacesWithFocus :: (Module.Namespaces t0 -> (Module.Namespace, t0) -> Module.Namespaces t0)
namespacesWithFocus original newVal = Module.Namespaces {
  Module.namespacesFocus = newVal,
  Module.namespacesMapping = (Module.namespacesMapping original)}

namespacesWithMapping :: (Module.Namespaces t0 -> M.Map Module.Namespace t0 -> Module.Namespaces t0)
namespacesWithMapping original newVal = Module.Namespaces {
  Module.namespacesFocus = (Module.namespacesFocus original),
  Module.namespacesMapping = newVal}

qualifiedName :: (Maybe Module.Namespace -> String -> Module.QualifiedName)
qualifiedName namespace local = Module.QualifiedName {
  Module.qualifiedNameNamespace = namespace,
  Module.qualifiedNameLocal = local}

qualifiedNameNamespace :: (Module.QualifiedName -> Maybe Module.Namespace)
qualifiedNameNamespace = Module.qualifiedNameNamespace

qualifiedNameLocal :: (Module.QualifiedName -> String)
qualifiedNameLocal = Module.qualifiedNameLocal

qualifiedNameWithNamespace :: (Module.QualifiedName -> Maybe Module.Namespace -> Module.QualifiedName)
qualifiedNameWithNamespace original newVal = Module.QualifiedName {
  Module.qualifiedNameNamespace = newVal,
  Module.qualifiedNameLocal = (Module.qualifiedNameLocal original)}

qualifiedNameWithLocal :: (Module.QualifiedName -> String -> Module.QualifiedName)
qualifiedNameWithLocal original newVal = Module.QualifiedName {
  Module.qualifiedNameNamespace = (Module.qualifiedNameNamespace original),
  Module.qualifiedNameLocal = newVal}

termDefinition :: (Core.Name -> Core.Term -> Core.TypeScheme -> Module.TermDefinition)
termDefinition name term type_ = Module.TermDefinition {
  Module.termDefinitionName = name,
  Module.termDefinitionTerm = term,
  Module.termDefinitionType = type_}

termDefinitionName :: (Module.TermDefinition -> Core.Name)
termDefinitionName = Module.termDefinitionName

termDefinitionTerm :: (Module.TermDefinition -> Core.Term)
termDefinitionTerm = Module.termDefinitionTerm

termDefinitionType :: (Module.TermDefinition -> Core.TypeScheme)
termDefinitionType = Module.termDefinitionType

termDefinitionWithName :: (Module.TermDefinition -> Core.Name -> Module.TermDefinition)
termDefinitionWithName original newVal = Module.TermDefinition {
  Module.termDefinitionName = newVal,
  Module.termDefinitionTerm = (Module.termDefinitionTerm original),
  Module.termDefinitionType = (Module.termDefinitionType original)}

termDefinitionWithTerm :: (Module.TermDefinition -> Core.Term -> Module.TermDefinition)
termDefinitionWithTerm original newVal = Module.TermDefinition {
  Module.termDefinitionName = (Module.termDefinitionName original),
  Module.termDefinitionTerm = newVal,
  Module.termDefinitionType = (Module.termDefinitionType original)}

termDefinitionWithType :: (Module.TermDefinition -> Core.TypeScheme -> Module.TermDefinition)
termDefinitionWithType original newVal = Module.TermDefinition {
  Module.termDefinitionName = (Module.termDefinitionName original),
  Module.termDefinitionTerm = (Module.termDefinitionTerm original),
  Module.termDefinitionType = newVal}

typeDefinition :: (Core.Name -> Core.Type -> Module.TypeDefinition)
typeDefinition name type_ = Module.TypeDefinition {
  Module.typeDefinitionName = name,
  Module.typeDefinitionType = type_}

typeDefinitionName :: (Module.TypeDefinition -> Core.Name)
typeDefinitionName = Module.typeDefinitionName

typeDefinitionType :: (Module.TypeDefinition -> Core.Type)
typeDefinitionType = Module.typeDefinitionType

typeDefinitionWithName :: (Module.TypeDefinition -> Core.Name -> Module.TypeDefinition)
typeDefinitionWithName original newVal = Module.TypeDefinition {
  Module.typeDefinitionName = newVal,
  Module.typeDefinitionType = (Module.typeDefinitionType original)}

typeDefinitionWithType :: (Module.TypeDefinition -> Core.Type -> Module.TypeDefinition)
typeDefinitionWithType original newVal = Module.TypeDefinition {
  Module.typeDefinitionName = (Module.typeDefinitionName original),
  Module.typeDefinitionType = newVal}
