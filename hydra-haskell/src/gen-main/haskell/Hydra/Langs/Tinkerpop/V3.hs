-- | A simple TinkerPop version 3 syntax model

module Hydra.Langs.Tinkerpop.V3 where

import qualified Hydra.Core as Core
import Data.List
import Data.Map
import Data.Set

-- | An edge
data Edge v e p = 
  Edge {
    edgeLabel :: EdgeLabel,
    edgeId :: e,
    edgeOut :: v,
    edgeIn :: v,
    edgeProperties :: (Map PropertyKey p)}
  deriving (Eq, Ord, Read, Show)

_Edge = (Core.Name "hydra/langs/tinkerpop/v3.Edge")

_Edge_label = (Core.FieldName "label")

_Edge_id = (Core.FieldName "id")

_Edge_out = (Core.FieldName "out")

_Edge_in = (Core.FieldName "in")

_Edge_properties = (Core.FieldName "properties")

-- | The (required) label of an edge
newtype EdgeLabel = 
  EdgeLabel {
    -- | The (required) label of an edge
    unEdgeLabel :: String}
  deriving (Eq, Ord, Read, Show)

_EdgeLabel = (Core.Name "hydra/langs/tinkerpop/v3.EdgeLabel")

-- | Either a vertex or an edge
data Element v e p = 
  ElementVertex (Vertex v p) |
  ElementEdge (Edge v e p)
  deriving (Eq, Ord, Read, Show)

_Element = (Core.Name "hydra/langs/tinkerpop/v3.Element")

_Element_vertex = (Core.FieldName "vertex")

_Element_edge = (Core.FieldName "edge")

-- | A graph; a self-contained collection of vertices and edges
data Graph v e p = 
  Graph {
    graphVertices :: (Set (Vertex v p)),
    graphEdges :: (Set (Edge v e p))}
  deriving (Eq, Ord, Read, Show)

_Graph = (Core.Name "hydra/langs/tinkerpop/v3.Graph")

_Graph_vertices = (Core.FieldName "vertices")

_Graph_edges = (Core.FieldName "edges")

-- | A key/value property
data Property p = 
  Property {
    propertyKey :: PropertyKey,
    propertyValue :: p}
  deriving (Eq, Ord, Read, Show)

_Property = (Core.Name "hydra/langs/tinkerpop/v3.Property")

_Property_key = (Core.FieldName "key")

_Property_value = (Core.FieldName "value")

-- | A property key
newtype PropertyKey = 
  PropertyKey {
    -- | A property key
    unPropertyKey :: String}
  deriving (Eq, Ord, Read, Show)

_PropertyKey = (Core.Name "hydra/langs/tinkerpop/v3.PropertyKey")

-- | A vertex
data Vertex v p = 
  Vertex {
    vertexLabel :: VertexLabel,
    vertexId :: v,
    vertexProperties :: (Map PropertyKey p)}
  deriving (Eq, Ord, Read, Show)

_Vertex = (Core.Name "hydra/langs/tinkerpop/v3.Vertex")

_Vertex_label = (Core.FieldName "label")

_Vertex_id = (Core.FieldName "id")

_Vertex_properties = (Core.FieldName "properties")

-- | The label of a vertex. The default (null) vertex is represented by the empty string
newtype VertexLabel = 
  VertexLabel {
    -- | The label of a vertex. The default (null) vertex is represented by the empty string
    unVertexLabel :: String}
  deriving (Eq, Ord, Read, Show)

_VertexLabel = (Core.Name "hydra/langs/tinkerpop/v3.VertexLabel")