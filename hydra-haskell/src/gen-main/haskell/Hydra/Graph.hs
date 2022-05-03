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

_Element = "hydra/graph.Element"

_Element_name = "name"

_Element_schema = "schema"

_Element_data = "data"

-- A graph, or set of legal terms combined with a set of elements over those terms, as well as another graph, called the schema graph
data Graph m 
  = Graph {
    graphName :: GraphName,
    graphElements :: [Element m],
    graphDataTerms :: (Core.Term m -> Bool),
    graphSchemaGraph :: GraphName}

_Graph = "hydra/graph.Graph"

_Graph_name = "name"

_Graph_elements = "elements"

_Graph_dataTerms = "dataTerms"

_Graph_schemaGraph = "schemaGraph"

-- A unique identifier for a graph within a graph set
type GraphName = String

_GraphName = "hydra/graph.GraphName"

-- A collection of graphs with a distinguished root graph
data GraphSet m 
  = GraphSet {
    graphSetGraphs :: (Map GraphName (Graph m)),
    graphSetRoot :: GraphName}

_GraphSet = "hydra/graph.GraphSet"

_GraphSet_graphs = "graphs"

_GraphSet_root = "root"

-- A logical collection of elements; a graph subset with dependencies on zero or more other subsets
data Module m 
  = Module {
    moduleGraph :: (Graph m),
    moduleImports :: [Module m]}

_Module = "hydra/graph.Module"

_Module_graph = "graph"

_Module_imports = "imports"