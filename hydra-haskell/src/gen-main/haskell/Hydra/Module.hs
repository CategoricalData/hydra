-- | A model for Hydra namespaces and modules (collections of elements in the same namespace)

module Hydra.Module where

import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

newtype FileExtension = 
  FileExtension {
    unFileExtension :: String}
  deriving (Eq, Ord, Read, Show)

_FileExtension = (Core.Name "hydra/module.FileExtension")

_FileExtension_type_ = (Core.TypeLiteral Core.LiteralTypeString)

-- | A logical collection of elements in the same namespace, having dependencies on zero or more other modules
data Module = 
  Module {
    -- | A common prefix for all element names in the module
    moduleNamespace :: Namespace,
    -- | The elements defined in this module
    moduleElements :: [Graph.Element],
    -- | Any modules which the term expressions of this module directly depend upon
    moduleTermDependencies :: [Module],
    -- | Any modules which the type expressions of this module directly depend upon
    moduleTypeDependencies :: [Module],
    -- | An optional human-readable description of the module
    moduleDescription :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_Module = (Core.Name "hydra/module.Module")

_Module_namespace = (Core.Name "namespace")

_Module_elements = (Core.Name "elements")

_Module_termDependencies = (Core.Name "termDependencies")

_Module_typeDependencies = (Core.Name "typeDependencies")

_Module_description = (Core.Name "description")

_Module_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/module.Module"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "namespace"),
      Core.fieldTypeType = _Namespace_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "elements"),
      Core.fieldTypeType = (Core.TypeList Graph._Element_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "termDependencies"),
      Core.fieldTypeType = (Core.TypeList _Module_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "typeDependencies"),
      Core.fieldTypeType = (Core.TypeList _Module_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "description"),
      Core.fieldTypeType = (Core.TypeOptional (Core.TypeLiteral Core.LiteralTypeString))}]}))

-- | A prefix for element names
newtype Namespace = 
  Namespace {
    unNamespace :: String}
  deriving (Eq, Ord, Read, Show)

_Namespace = (Core.Name "hydra/module.Namespace")

_Namespace_type_ = (Core.TypeLiteral Core.LiteralTypeString)

-- | A qualified name consisting of an optional namespace together with a mandatory local name
data QualifiedName = 
  QualifiedName {
    qualifiedNameNamespace :: (Maybe Namespace),
    qualifiedNameLocal :: String}
  deriving (Eq, Ord, Read, Show)

_QualifiedName = (Core.Name "hydra/module.QualifiedName")

_QualifiedName_namespace = (Core.Name "namespace")

_QualifiedName_local = (Core.Name "local")

_QualifiedName_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/module.QualifiedName"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "namespace"),
      Core.fieldTypeType = (Core.TypeOptional _Namespace_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "local"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)}]}))