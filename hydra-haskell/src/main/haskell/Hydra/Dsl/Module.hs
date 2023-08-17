module Hydra.Dsl.Module where

import Hydra.Kernel
import Hydra.Dsl.Base as Base


qualifiedName :: Datum (Maybe Namespace) -> Datum String -> Datum QualifiedName
qualifiedName ns local = record _QualifiedName [
  _QualifiedName_namespace>>: ns,
  _QualifiedName_local>>: local]

qualifiedNameLocal :: Datum (QualifiedName -> String)
qualifiedNameLocal = project _QualifiedName _QualifiedName_local

qualifiedNameNamespace :: Datum (QualifiedName -> Maybe Namespace)
qualifiedNameNamespace = project _QualifiedName _QualifiedName_namespace
