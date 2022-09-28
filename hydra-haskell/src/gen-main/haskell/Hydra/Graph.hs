-- | Abstractions for graphs, elements, and modules

module Hydra.Graph where

import qualified Hydra.Core as Core
import Data.Map
import Data.Set

-- | A graph element, having a name, data term (value), and schema term (type)
data Element m 
  = Element {
    elementName :: Core.Name,
    elementSchema :: (Core.Term m),
    elementData :: (Core.Term m)}
  deriving (Eq, Ord, Read, Show)

_Element = (Core.Name "hydra/graph.Element")

_Element_name = (Core.FieldName "name")

_Element_schema = (Core.FieldName "schema")

_Element_data = (Core.FieldName "data")

-- | A graph, or set of named terms, together with its schema graph
data Graph m 
  = Graph {
    -- | All of the elements in the graph
    graphElements :: (Map Core.Name (Element m)),
    -- | The schema graph to this graph. If omitted, the graph is its own schema graph.
    graphSchema :: (Maybe (Graph m))}
  deriving (Eq, Ord, Read, Show)

_Graph = (Core.Name "hydra/graph.Graph")

_Graph_elements = (Core.FieldName "elements")

_Graph_schema = (Core.FieldName "schema")

-- | A logical collection of elements; a graph subset with dependencies on zero or more other subsets
data Module m 
  = Module {
    -- | A common prefix for all element names in the module
    moduleNamespace :: Namespace,
    -- | The elements defined in this module
    moduleElements :: [Element m],
    -- | Any additional modules this one has a direct dependency upon
    moduleDependencies :: [Module m],
    -- | An optional human-readable description of the module
    moduleDescription :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_Module = (Core.Name "hydra/graph.Module")

_Module_namespace = (Core.FieldName "namespace")

_Module_elements = (Core.FieldName "elements")

_Module_dependencies = (Core.FieldName "dependencies")

_Module_description = (Core.FieldName "description")

-- | A prefix for element names
newtype Namespace 
  = Namespace {
    -- | A prefix for element names
    unNamespace :: String}
  deriving (Eq, Ord, Read, Show)

_Namespace = (Core.Name "hydra/graph.Namespace")