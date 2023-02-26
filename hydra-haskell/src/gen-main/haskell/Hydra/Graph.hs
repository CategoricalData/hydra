-- | The extension to graphs of Hydra's core type system (hydra/core)

module Hydra.Graph where

import qualified Hydra.Core as Core
import Data.List
import Data.Map
import Data.Set

-- | A graph element, having a name, data term (value), and schema term (type)
data Element m = 
  Element {
    elementName :: Core.Name,
    elementSchema :: (Core.Term m),
    elementData :: (Core.Term m)}
  deriving (Eq, Ord, Read, Show)

_Element = (Core.Name "hydra/graph.Element")

_Element_name = (Core.FieldName "name")

_Element_schema = (Core.FieldName "schema")

_Element_data = (Core.FieldName "data")

-- | A graph, or set of named terms, together with its schema graph
data Graph m = 
  Graph {
    -- | All of the elements in the graph
    graphElements :: (Map Core.Name (Element m)),
    -- | The schema graph to this graph. If omitted, the graph is its own schema graph.
    graphSchema :: (Maybe (Graph m))}
  deriving (Eq, Ord, Read, Show)

_Graph = (Core.Name "hydra/graph.Graph")

_Graph_elements = (Core.FieldName "elements")

_Graph_schema = (Core.FieldName "schema")