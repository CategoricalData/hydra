-- | Meta-DSL for constructing module-related terms (definitions, namespaces)

module Hydra.Dsl.Meta.Module where

import Hydra.Kernel
import Hydra.Dsl.Meta.Phantoms

import qualified Data.Map as M


definitionTerm :: TTerm TermDefinition -> TTerm Definition
definitionTerm = inject _Definition _Definition_term

definitionType :: TTerm TypeDefinition -> TTerm Definition
definitionType = inject _Definition _Definition_type

module_ :: TTerm Namespace -> TTerm [Binding] -> TTerm [Namespace] -> TTerm [Namespace] -> TTerm (Maybe String) -> TTerm Module
module_ ns elems termDeps typeDeps desc = record _Module [
  _Module_namespace>>: ns,
  _Module_elements>>: elems,
  _Module_termDependencies>>: termDeps,
  _Module_typeDependencies>>: typeDeps,
  _Module_description>>: desc]

moduleNamespace :: TTerm Module -> TTerm Namespace
moduleNamespace m = project _Module _Module_namespace @@ m

moduleElements :: TTerm Module -> TTerm [Binding]
moduleElements m = project _Module _Module_elements @@ m

moduleTermDependencies :: TTerm Module -> TTerm [Namespace]
moduleTermDependencies m = project _Module _Module_termDependencies @@ m

moduleTypeDependencies :: TTerm Module -> TTerm [Namespace]
moduleTypeDependencies m = project _Module _Module_typeDependencies @@ m

moduleDescription :: TTerm Module -> TTerm (Maybe String)
moduleDescription m = project _Module _Module_description @@ m

namespace :: TTerm String -> TTerm Namespace
namespace ns = wrap _Namespace ns

namespaces :: TTerm (Namespace, n) -> TTerm (M.Map Namespace n) -> TTerm (Namespaces n)
namespaces focus mapping = record _Namespaces [
  _Namespaces_focus>>: focus,
  _Namespaces_mapping>>: mapping]

namespacesFocus :: TTerm (Namespaces n) -> TTerm (Namespace, n)
namespacesFocus ns = project _Namespaces _Namespaces_focus @@ ns

namespacesMapping :: TTerm (Namespaces n) -> TTerm (M.Map Namespace n)
namespacesMapping ns = project _Namespaces _Namespaces_mapping @@ ns

namespacesWithFocus :: TTerm (Namespaces n) -> TTerm (Namespace, n) -> TTerm (Namespaces n)
namespacesWithFocus ns focus = namespaces focus (Hydra.Dsl.Meta.Module.namespacesMapping ns)

namespacesWithMapping :: TTerm (Namespaces n) -> TTerm (M.Map Namespace n) -> TTerm (Namespaces n)
namespacesWithMapping ns mapping = namespaces (Hydra.Dsl.Meta.Module.namespacesFocus ns) mapping

qualifiedName :: TTerm (Maybe Namespace) -> TTerm String -> TTerm QualifiedName
qualifiedName ns local = record _QualifiedName [
  _QualifiedName_namespace>>: ns,
  _QualifiedName_local>>: local]

qualifiedNameLocal :: TTerm QualifiedName -> TTerm String
qualifiedNameLocal qn = project _QualifiedName _QualifiedName_local @@ qn

qualifiedNameNamespace :: TTerm QualifiedName -> TTerm (Maybe Namespace)
qualifiedNameNamespace qn = project _QualifiedName _QualifiedName_namespace @@ qn

termDefinition :: TTerm Name -> TTerm Term -> TTerm TypeScheme -> TTerm TermDefinition
termDefinition name term typeScheme = record _TermDefinition [
  _TermDefinition_name>>: name,
  _TermDefinition_term>>: term,
  _TermDefinition_type>>: typeScheme]

termDefinitionName :: TTerm TermDefinition -> TTerm Name
termDefinitionName td = project _TermDefinition _TermDefinition_name @@ td

termDefinitionTerm :: TTerm TermDefinition -> TTerm Term
termDefinitionTerm td = project _TermDefinition _TermDefinition_term @@ td

termDefinitionType :: TTerm TermDefinition -> TTerm TypeScheme
termDefinitionType td = project _TermDefinition _TermDefinition_type @@ td

typeDefinition :: TTerm Name -> TTerm Type -> TTerm TypeDefinition
typeDefinition name typ = record _TypeDefinition [
  _TypeDefinition_name>>: name,
  _TypeDefinition_type>>: typ]

typeDefinitionName :: TTerm TypeDefinition -> TTerm Name
typeDefinitionName td = project _TypeDefinition _TypeDefinition_name @@ td

typeDefinitionType :: TTerm TypeDefinition -> TTerm Type
typeDefinitionType td = project _TypeDefinition _TypeDefinition_type @@ td

unFileExtension :: TTerm FileExtension -> TTerm String
unFileExtension fe = unwrap _FileExtension @@ fe

unNamespace :: TTerm Namespace -> TTerm String
unNamespace ns = unwrap _Namespace @@ ns
