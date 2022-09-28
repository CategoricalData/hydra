module Hydra.Graph where

import qualified Hydra.Core as Core
import Data.Map
import Data.Set

-- A logical collection of elements; a graph subset with dependencies on zero or more other subsets
data Module m 
  = Module {
    -- A common prefix for all element names in the module
    moduleNamespace :: Namespace,
    -- The elements defined in this module
    moduleElements :: [Core.Element m],
    -- Any additional modules this one has a direct dependency upon
    moduleDependencies :: [Module m]}
  deriving (Eq, Ord, Read, Show)

_Module = (Core.Name "hydra/graph.Module")

_Module_namespace = (Core.FieldName "namespace")

_Module_elements = (Core.FieldName "elements")

_Module_dependencies = (Core.FieldName "dependencies")

-- A prefix for element names
newtype Namespace 
  = Namespace {
    -- A prefix for element names
    unNamespace :: String}
  deriving (Eq, Ord, Read, Show)

_Namespace = (Core.Name "hydra/graph.Namespace")