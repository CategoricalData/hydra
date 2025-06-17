module Hydra.Dsl.Module where

import Hydra.Kernel
import Hydra.Dsl.Phantoms


module_ :: TTerm Namespace -> TTerm [Element] -> TTerm [Module] -> TTerm [Module] -> TTerm (Maybe String) -> TTerm Module
module_ ns elems termDeps typeDeps desc = record _Module [
  _Module_namespace>>: ns,
  _Module_elements>>: elems,
  _Module_termDependencies>>: termDeps,
  _Module_typeDependencies>>: typeDeps,
  _Module_description>>: desc]

namespace :: TTerm String -> TTerm Namespace
namespace ns = wrap _Namespace ns

qualifiedName :: TTerm (Maybe Namespace) -> TTerm String -> TTerm QualifiedName
qualifiedName ns local = record _QualifiedName [
  _QualifiedName_namespace>>: ns,
  _QualifiedName_local>>: local]

qualifiedNameLocal :: TTerm QualifiedName -> TTerm String
qualifiedNameLocal qn = project _QualifiedName _QualifiedName_local @@ qn

qualifiedNameNamespace :: TTerm QualifiedName -> TTerm (Maybe Namespace)
qualifiedNameNamespace qn = project _QualifiedName _QualifiedName_namespace @@ qn

termDefinitionName :: TTerm TermDefinition -> TTerm Name
termDefinitionName td = project _TermDefinition _TermDefinition_name @@ td

termDefinitionTerm :: TTerm TermDefinition -> TTerm Term
termDefinitionTerm td = project _TermDefinition _TermDefinition_term @@ td

termDefinitionType :: TTerm TermDefinition -> TTerm Type
termDefinitionType td = project _TermDefinition _TermDefinition_type @@ td

typeDefinitionName :: TTerm TypeDefinition -> TTerm Name
typeDefinitionName td = project _TypeDefinition _TypeDefinition_name @@ td

typeDefinitionType :: TTerm TypeDefinition -> TTerm Type
typeDefinitionType td = project _TypeDefinition _TypeDefinition_type @@ td

unFileExtension :: TTerm FileExtension -> TTerm String
unFileExtension fe = unwrap _FileExtension @@ fe

unNamespace :: TTerm Namespace -> TTerm String
unNamespace ns = unwrap _Namespace @@ ns
