-- | A typed property graph data model

module Hydra.Langs.Tinkerpop.PropertyGraph where

import qualified Hydra.Core as Core
import Data.Int
import Data.List
import Data.Map
import Data.Set

-- | The direction of an edge
data Direction = 
  DirectionOut  |
  DirectionIn  |
  DirectionBoth 
  deriving (Eq, Ord, Read, Show)

_Direction = (Core.Name "hydra/langs/tinkerpop/propertyGraph.Direction")

_Direction_out = (Core.FieldName "out")

_Direction_in = (Core.FieldName "in")

_Direction_both = (Core.FieldName "both")

-- | An edge
data Edge v e p = 
  Edge {
    edgeLabel :: EdgeLabel,
    edgeId :: e,
    edgeOut :: v,
    edgeIn :: v,
    edgeProperties :: (Map PropertyKey p)}
  deriving (Eq, Ord, Read, Show)

_Edge = (Core.Name "hydra/langs/tinkerpop/propertyGraph.Edge")

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

_EdgeLabel = (Core.Name "hydra/langs/tinkerpop/propertyGraph.EdgeLabel")

-- | The type of an edge
data EdgeType t = 
  EdgeType {
    edgeTypeLabel :: EdgeLabel,
    edgeTypeOut :: VertexLabel,
    edgeTypeIn :: VertexLabel,
    edgeTypeProperties :: (Map PropertyKey t)}
  deriving (Eq, Ord, Read, Show)

_EdgeType = (Core.Name "hydra/langs/tinkerpop/propertyGraph.EdgeType")

_EdgeType_label = (Core.FieldName "label")

_EdgeType_out = (Core.FieldName "out")

_EdgeType_in = (Core.FieldName "in")

_EdgeType_properties = (Core.FieldName "properties")

-- | Either a vertex or an edge
data Element v e p = 
  ElementVertex (Vertex v p) |
  ElementEdge (Edge v e p)
  deriving (Eq, Ord, Read, Show)

_Element = (Core.Name "hydra/langs/tinkerpop/propertyGraph.Element")

_Element_vertex = (Core.FieldName "vertex")

_Element_edge = (Core.FieldName "edge")

-- | The kind of an element: vertex or edge
data ElementKind = 
  ElementKindVertex  |
  ElementKindEdge 
  deriving (Eq, Ord, Read, Show)

_ElementKind = (Core.Name "hydra/langs/tinkerpop/propertyGraph.ElementKind")

_ElementKind_vertex = (Core.FieldName "vertex")

_ElementKind_edge = (Core.FieldName "edge")

-- | An element together with its dependencies in some context
data ElementTree v e p = 
  ElementTree {
    elementTreePrimary :: (Element v e p),
    elementTreeDependencies :: [ElementTree v e p]}
  deriving (Eq, Ord, Read, Show)

_ElementTree = (Core.Name "hydra/langs/tinkerpop/propertyGraph.ElementTree")

_ElementTree_primary = (Core.FieldName "primary")

_ElementTree_dependencies = (Core.FieldName "dependencies")

-- | The type of a vertex or edge
data ElementType t = 
  ElementTypeVertex (VertexType t) |
  ElementTypeEdge (EdgeType t)
  deriving (Eq, Ord, Read, Show)

_ElementType = (Core.Name "hydra/langs/tinkerpop/propertyGraph.ElementType")

_ElementType_vertex = (Core.FieldName "vertex")

_ElementType_edge = (Core.FieldName "edge")

-- | An element type together with its dependencies in some context
data ElementTypeTree t = 
  ElementTypeTree {
    elementTypeTreePrimary :: (ElementType t),
    elementTypeTreeDependencies :: [ElementTypeTree t]}
  deriving (Eq, Ord, Read, Show)

_ElementTypeTree = (Core.Name "hydra/langs/tinkerpop/propertyGraph.ElementTypeTree")

_ElementTypeTree_primary = (Core.FieldName "primary")

_ElementTypeTree_dependencies = (Core.FieldName "dependencies")

-- | A graph; a self-contained collection of vertices and edges
data Graph v e p = 
  Graph {
    graphVertices :: (Map v (Vertex v p)),
    graphEdges :: (Map e (Edge v e p))}
  deriving (Eq, Ord, Read, Show)

_Graph = (Core.Name "hydra/langs/tinkerpop/propertyGraph.Graph")

_Graph_vertices = (Core.FieldName "vertices")

_Graph_edges = (Core.FieldName "edges")

-- | A vertex or edge label
data Label = 
  LabelVertex VertexLabel |
  LabelEdge EdgeLabel
  deriving (Eq, Ord, Read, Show)

_Label = (Core.Name "hydra/langs/tinkerpop/propertyGraph.Label")

_Label_vertex = (Core.FieldName "vertex")

_Label_edge = (Core.FieldName "edge")

-- | A key/value property
data Property p = 
  Property {
    propertyKey :: PropertyKey,
    propertyValue :: p}
  deriving (Eq, Ord, Read, Show)

_Property = (Core.Name "hydra/langs/tinkerpop/propertyGraph.Property")

_Property_key = (Core.FieldName "key")

_Property_value = (Core.FieldName "value")

-- | A property key
newtype PropertyKey = 
  PropertyKey {
    -- | A property key
    unPropertyKey :: String}
  deriving (Eq, Ord, Read, Show)

_PropertyKey = (Core.Name "hydra/langs/tinkerpop/propertyGraph.PropertyKey")

-- | The type of a property
data PropertyType t = 
  PropertyType {
    propertyTypeKey :: PropertyKey,
    propertyTypeValue :: t}
  deriving (Eq, Ord, Read, Show)

_PropertyType = (Core.Name "hydra/langs/tinkerpop/propertyGraph.PropertyType")

_PropertyType_key = (Core.FieldName "key")

_PropertyType_value = (Core.FieldName "value")

-- | A vertex
data Vertex v p = 
  Vertex {
    vertexLabel :: VertexLabel,
    vertexId :: v,
    vertexProperties :: (Map PropertyKey p)}
  deriving (Eq, Ord, Read, Show)

_Vertex = (Core.Name "hydra/langs/tinkerpop/propertyGraph.Vertex")

_Vertex_label = (Core.FieldName "label")

_Vertex_id = (Core.FieldName "id")

_Vertex_properties = (Core.FieldName "properties")

-- | The label of a vertex. The default (null) vertex is represented by the empty string
newtype VertexLabel = 
  VertexLabel {
    -- | The label of a vertex. The default (null) vertex is represented by the empty string
    unVertexLabel :: String}
  deriving (Eq, Ord, Read, Show)

_VertexLabel = (Core.Name "hydra/langs/tinkerpop/propertyGraph.VertexLabel")

-- | The type of a vertex
data VertexType t = 
  VertexType {
    vertexTypeLabel :: VertexLabel,
    vertexTypeProperties :: (Map PropertyKey t)}
  deriving (Eq, Ord, Read, Show)

_VertexType = (Core.Name "hydra/langs/tinkerpop/propertyGraph.VertexType")

_VertexType_label = (Core.FieldName "label")

_VertexType_properties = (Core.FieldName "properties")