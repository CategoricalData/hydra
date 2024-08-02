module Hydra.Dsl.Module where

import Hydra.Kernel
import Hydra.Dsl.Base as Base


qualifiedName :: TTerm (Maybe Namespace) -> TTerm String -> TTerm QualifiedName
qualifiedName ns local = record _QualifiedName [
  _QualifiedName_namespace>>: ns,
  _QualifiedName_local>>: local]

qualifiedNameLocal :: TTerm (QualifiedName -> String)
qualifiedNameLocal = project _QualifiedName _QualifiedName_local

qualifiedNameNamespace :: TTerm (QualifiedName -> Maybe Namespace)
qualifiedNameNamespace = project _QualifiedName _QualifiedName_namespace
