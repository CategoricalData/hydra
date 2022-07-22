module Hydra.Graph where

import qualified Hydra.Core as Core
import Data.Map
import Data.Set

-- A graph element, having a name, data term (value), and schema term (type)
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

-- A graph, or set of legal terms combined with a set of elements over those terms, as well as another graph, called the schema graph
data Graph m 
  = Graph {
    graphName :: GraphName,
    graphElements :: [Element m],
    graphTermExprs :: (Core.Term m -> Bool),
    -- A reference to this graph's schema graph within the provided graph set
    graphSchemaGraph :: GraphName}

_Graph = (Core.Name "hydra/graph.Graph")

_Graph_name = (Core.FieldName "name")

_Graph_elements = (Core.FieldName "elements")

_Graph_termExprs = (Core.FieldName "termExprs")

_Graph_schemaGraph = (Core.FieldName "schemaGraph")

-- A unique identifier for a graph within a graph set
newtype GraphName 
  = GraphName {
    -- A unique identifier for a graph within a graph set
    unGraphName :: String}
  deriving (Eq, Ord, Read, Show)

_GraphName = (Core.Name "hydra/graph.GraphName")

-- A collection of graphs with a distinguished root graph
data GraphSet m 
  = GraphSet {
    graphSetGraphs :: (Map GraphName (Graph m)),
    graphSetRoot :: GraphName}

_GraphSet = (Core.Name "hydra/graph.GraphSet")

_GraphSet_graphs = (Core.FieldName "graphs")

_GraphSet_root = (Core.FieldName "root")

-- A logical collection of elements; a graph subset with dependencies on zero or more other subsets
data Module m 
  = Module {
    moduleGraph :: (Graph m),
    moduleImports :: [Module m]}

_Module = (Core.Name "hydra/graph.Module")

_Module_graph = (Core.FieldName "graph")

_Module_imports = (Core.FieldName "imports")