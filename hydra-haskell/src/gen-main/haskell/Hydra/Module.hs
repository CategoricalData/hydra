-- | A model for Hydra namespaces and modules (collections of elements in the same namespace)

module Hydra.Module where

import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import Data.List
import Data.Map
import Data.Set

newtype FileExtension = 
  FileExtension {
    unFileExtension :: String}
  deriving (Eq, Ord, Read, Show)

_FileExtension = (Core.Name "hydra/module.FileExtension")

-- | A logical collection of elements in the same namespace, having dependencies on zero or more other modules
data Module a = 
  Module {
    -- | A common prefix for all element names in the module
    moduleNamespace :: Namespace,
    -- | The elements defined in this module
    moduleElements :: [Graph.Element a],
    -- | Any additional modules this one has a direct dependency upon
    moduleDependencies :: [Module a],
    -- | An optional human-readable description of the module
    moduleDescription :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_Module = (Core.Name "hydra/module.Module")

_Module_namespace = (Core.FieldName "namespace")

_Module_elements = (Core.FieldName "elements")

_Module_dependencies = (Core.FieldName "dependencies")

_Module_description = (Core.FieldName "description")

-- | A prefix for element names
newtype Namespace = 
  Namespace {
    -- | A prefix for element names
    unNamespace :: String}
  deriving (Eq, Ord, Read, Show)

_Namespace = (Core.Name "hydra/module.Namespace")