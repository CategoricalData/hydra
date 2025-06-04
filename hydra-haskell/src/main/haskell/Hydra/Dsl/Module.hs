module Hydra.Dsl.Module where

import Hydra.Kernel
import Hydra.Dsl.Phantoms


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

unFileExtension :: TTerm FileExtension -> TTerm String
unFileExtension fe = unwrap _FileExtension @@ fe