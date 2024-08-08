-- | A typed property graph data model. Property graphs are parameterized a type for property and id values, while property graph schemas are parameterized by a type for property and id types

module Hydra.Langs.Tinkerpop.PropertyGraph where

import qualified Hydra.Core as Core
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

-- | The direction of an edge or edge pattern
data Direction = 
  DirectionOut  |
  DirectionIn  |
  DirectionBoth  |
  DirectionUndirected 
  deriving (Eq, Ord, Read, Show)

_Direction = (Core.Name "hydra/langs/tinkerpop/propertyGraph.Direction")

_Direction_out = (Core.Name "out")

_Direction_in = (Core.Name "in")

_Direction_both = (Core.Name "both")

_Direction_undirected = (Core.Name "undirected")

_Direction_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/propertyGraph.Direction"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "out"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "in"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "both"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "undirected"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

-- | An edge
data Edge v = 
  Edge {
    -- | The label of the edge
    edgeLabel :: EdgeLabel,
    -- | The unique identifier of the edge
    edgeId :: v,
    -- | The id of the out-vertex (tail) of the edge
    edgeOut :: v,
    -- | The id of the in-vertex (head) of the edge
    edgeIn :: v,
    -- | A key/value map of edge properties
    edgeProperties :: (Map PropertyKey v)}
  deriving (Eq, Ord, Read, Show)

_Edge = (Core.Name "hydra/langs/tinkerpop/propertyGraph.Edge")

_Edge_label = (Core.Name "label")

_Edge_id = (Core.Name "id")

_Edge_out = (Core.Name "out")

_Edge_in = (Core.Name "in")

_Edge_properties = (Core.Name "properties")

_Edge_type_ = (Core.TypeLambda (Core.LambdaType {
  Core.lambdaTypeParameter = (Core.Name "v"),
  Core.lambdaTypeBody = (Core.TypeRecord (Core.RowType {
    Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/propertyGraph.Edge"),
    Core.rowTypeExtends = Nothing,
    Core.rowTypeFields = [
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "label"),
        Core.fieldTypeType = _EdgeLabel_type_},
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "id"),
        Core.fieldTypeType = (Core.TypeVariable (Core.Name "v"))},
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "out"),
        Core.fieldTypeType = (Core.TypeVariable (Core.Name "v"))},
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "in"),
        Core.fieldTypeType = (Core.TypeVariable (Core.Name "v"))},
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "properties"),
        Core.fieldTypeType = (Core.TypeMap (Core.MapType {
          Core.mapTypeKeys = _PropertyKey_type_,
          Core.mapTypeValues = (Core.TypeVariable (Core.Name "v"))}))}]}))}))

-- | The label of an edge
newtype EdgeLabel = 
  EdgeLabel {
    unEdgeLabel :: String}
  deriving (Eq, Ord, Read, Show)

_EdgeLabel = (Core.Name "hydra/langs/tinkerpop/propertyGraph.EdgeLabel")

_EdgeLabel_type_ = (Core.TypeLiteral Core.LiteralTypeString)

-- | The type of an edge
data EdgeType t = 
  EdgeType {
    -- | The label of any edge of this edge type
    edgeTypeLabel :: EdgeLabel,
    -- | The type of the id of any edge of this edge type
    edgeTypeId :: t,
    -- | The label of the out-vertex (tail) of any edge of this edge type
    edgeTypeOut :: VertexLabel,
    -- | The label of the in-vertex (head) of any edge of this edge type
    edgeTypeIn :: VertexLabel,
    -- | A list of property types. The types are ordered for the sake of applications in which property order is significant.
    edgeTypeProperties :: [PropertyType t]}
  deriving (Eq, Ord, Read, Show)

_EdgeType = (Core.Name "hydra/langs/tinkerpop/propertyGraph.EdgeType")

_EdgeType_label = (Core.Name "label")

_EdgeType_id = (Core.Name "id")

_EdgeType_out = (Core.Name "out")

_EdgeType_in = (Core.Name "in")

_EdgeType_properties = (Core.Name "properties")

_EdgeType_type_ = (Core.TypeLambda (Core.LambdaType {
  Core.lambdaTypeParameter = (Core.Name "t"),
  Core.lambdaTypeBody = (Core.TypeRecord (Core.RowType {
    Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/propertyGraph.EdgeType"),
    Core.rowTypeExtends = Nothing,
    Core.rowTypeFields = [
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "label"),
        Core.fieldTypeType = _EdgeLabel_type_},
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "id"),
        Core.fieldTypeType = (Core.TypeVariable (Core.Name "t"))},
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "out"),
        Core.fieldTypeType = _VertexLabel_type_},
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "in"),
        Core.fieldTypeType = _VertexLabel_type_},
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "properties"),
        Core.fieldTypeType = (Core.TypeList (Core.TypeApplication (Core.ApplicationType {
          Core.applicationTypeFunction = _PropertyType_type_,
          Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t"))})))}]}))}))

-- | Either a vertex or an edge
data Element v = 
  ElementVertex (Vertex v) |
  ElementEdge (Edge v)
  deriving (Eq, Ord, Read, Show)

_Element = (Core.Name "hydra/langs/tinkerpop/propertyGraph.Element")

_Element_vertex = (Core.Name "vertex")

_Element_edge = (Core.Name "edge")

_Element_type_ = (Core.TypeLambda (Core.LambdaType {
  Core.lambdaTypeParameter = (Core.Name "v"),
  Core.lambdaTypeBody = (Core.TypeUnion (Core.RowType {
    Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/propertyGraph.Element"),
    Core.rowTypeExtends = Nothing,
    Core.rowTypeFields = [
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "vertex"),
        Core.fieldTypeType = (Core.TypeApplication (Core.ApplicationType {
          Core.applicationTypeFunction = _Vertex_type_,
          Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "v"))}))},
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "edge"),
        Core.fieldTypeType = (Core.TypeApplication (Core.ApplicationType {
          Core.applicationTypeFunction = _Edge_type_,
          Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "v"))}))}]}))}))

-- | The kind of an element: vertex or edge
data ElementKind = 
  ElementKindVertex  |
  ElementKindEdge 
  deriving (Eq, Ord, Read, Show)

_ElementKind = (Core.Name "hydra/langs/tinkerpop/propertyGraph.ElementKind")

_ElementKind_vertex = (Core.Name "vertex")

_ElementKind_edge = (Core.Name "edge")

_ElementKind_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/propertyGraph.ElementKind"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "vertex"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "edge"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

-- | An element together with its dependencies in some context
data ElementTree v = 
  ElementTree {
    elementTreeSelf :: (Element v),
    elementTreeDependencies :: [ElementTree v]}
  deriving (Eq, Ord, Read, Show)

_ElementTree = (Core.Name "hydra/langs/tinkerpop/propertyGraph.ElementTree")

_ElementTree_self = (Core.Name "self")

_ElementTree_dependencies = (Core.Name "dependencies")

_ElementTree_type_ = (Core.TypeLambda (Core.LambdaType {
  Core.lambdaTypeParameter = (Core.Name "v"),
  Core.lambdaTypeBody = (Core.TypeRecord (Core.RowType {
    Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/propertyGraph.ElementTree"),
    Core.rowTypeExtends = Nothing,
    Core.rowTypeFields = [
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "self"),
        Core.fieldTypeType = (Core.TypeApplication (Core.ApplicationType {
          Core.applicationTypeFunction = _Element_type_,
          Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "v"))}))},
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "dependencies"),
        Core.fieldTypeType = (Core.TypeList (Core.TypeApplication (Core.ApplicationType {
          Core.applicationTypeFunction = _ElementTree_type_,
          Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "v"))})))}]}))}))

-- | The type of a vertex or edge
data ElementType t = 
  ElementTypeVertex (VertexType t) |
  ElementTypeEdge (EdgeType t)
  deriving (Eq, Ord, Read, Show)

_ElementType = (Core.Name "hydra/langs/tinkerpop/propertyGraph.ElementType")

_ElementType_vertex = (Core.Name "vertex")

_ElementType_edge = (Core.Name "edge")

_ElementType_type_ = (Core.TypeLambda (Core.LambdaType {
  Core.lambdaTypeParameter = (Core.Name "t"),
  Core.lambdaTypeBody = (Core.TypeUnion (Core.RowType {
    Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/propertyGraph.ElementType"),
    Core.rowTypeExtends = Nothing,
    Core.rowTypeFields = [
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "vertex"),
        Core.fieldTypeType = (Core.TypeApplication (Core.ApplicationType {
          Core.applicationTypeFunction = _VertexType_type_,
          Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t"))}))},
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "edge"),
        Core.fieldTypeType = (Core.TypeApplication (Core.ApplicationType {
          Core.applicationTypeFunction = _EdgeType_type_,
          Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t"))}))}]}))}))

-- | An element type together with its dependencies in some context
data ElementTypeTree t = 
  ElementTypeTree {
    elementTypeTreeSelf :: (ElementType t),
    elementTypeTreeDependencies :: [ElementTypeTree t]}
  deriving (Eq, Ord, Read, Show)

_ElementTypeTree = (Core.Name "hydra/langs/tinkerpop/propertyGraph.ElementTypeTree")

_ElementTypeTree_self = (Core.Name "self")

_ElementTypeTree_dependencies = (Core.Name "dependencies")

_ElementTypeTree_type_ = (Core.TypeLambda (Core.LambdaType {
  Core.lambdaTypeParameter = (Core.Name "t"),
  Core.lambdaTypeBody = (Core.TypeRecord (Core.RowType {
    Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/propertyGraph.ElementTypeTree"),
    Core.rowTypeExtends = Nothing,
    Core.rowTypeFields = [
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "self"),
        Core.fieldTypeType = (Core.TypeApplication (Core.ApplicationType {
          Core.applicationTypeFunction = _ElementType_type_,
          Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t"))}))},
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "dependencies"),
        Core.fieldTypeType = (Core.TypeList (Core.TypeApplication (Core.ApplicationType {
          Core.applicationTypeFunction = _ElementTypeTree_type_,
          Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t"))})))}]}))}))

-- | A graph; a self-contained collection of vertices and edges
data Graph v = 
  Graph {
    graphVertices :: (Map v (Vertex v)),
    graphEdges :: (Map v (Edge v))}
  deriving (Eq, Ord, Read, Show)

_Graph = (Core.Name "hydra/langs/tinkerpop/propertyGraph.Graph")

_Graph_vertices = (Core.Name "vertices")

_Graph_edges = (Core.Name "edges")

_Graph_type_ = (Core.TypeLambda (Core.LambdaType {
  Core.lambdaTypeParameter = (Core.Name "v"),
  Core.lambdaTypeBody = (Core.TypeRecord (Core.RowType {
    Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/propertyGraph.Graph"),
    Core.rowTypeExtends = Nothing,
    Core.rowTypeFields = [
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "vertices"),
        Core.fieldTypeType = (Core.TypeMap (Core.MapType {
          Core.mapTypeKeys = (Core.TypeVariable (Core.Name "v")),
          Core.mapTypeValues = (Core.TypeApplication (Core.ApplicationType {
            Core.applicationTypeFunction = _Vertex_type_,
            Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "v"))}))}))},
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "edges"),
        Core.fieldTypeType = (Core.TypeMap (Core.MapType {
          Core.mapTypeKeys = (Core.TypeVariable (Core.Name "v")),
          Core.mapTypeValues = (Core.TypeApplication (Core.ApplicationType {
            Core.applicationTypeFunction = _Edge_type_,
            Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "v"))}))}))}]}))}))

-- | A graph schema; a vertex and edge types for the vertices and edges of a graph conforming to the schema
data GraphSchema t = 
  GraphSchema {
    -- | A unique vertex type for each vertex label which may occur in a graph
    graphSchemaVertices :: (Map VertexLabel (VertexType t)),
    -- | A unique edge type for each edge label which may occur in a graph
    graphSchemaEdges :: (Map EdgeLabel (EdgeType t))}
  deriving (Eq, Ord, Read, Show)

_GraphSchema = (Core.Name "hydra/langs/tinkerpop/propertyGraph.GraphSchema")

_GraphSchema_vertices = (Core.Name "vertices")

_GraphSchema_edges = (Core.Name "edges")

_GraphSchema_type_ = (Core.TypeLambda (Core.LambdaType {
  Core.lambdaTypeParameter = (Core.Name "t"),
  Core.lambdaTypeBody = (Core.TypeRecord (Core.RowType {
    Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/propertyGraph.GraphSchema"),
    Core.rowTypeExtends = Nothing,
    Core.rowTypeFields = [
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "vertices"),
        Core.fieldTypeType = (Core.TypeMap (Core.MapType {
          Core.mapTypeKeys = _VertexLabel_type_,
          Core.mapTypeValues = (Core.TypeApplication (Core.ApplicationType {
            Core.applicationTypeFunction = _VertexType_type_,
            Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t"))}))}))},
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "edges"),
        Core.fieldTypeType = (Core.TypeMap (Core.MapType {
          Core.mapTypeKeys = _EdgeLabel_type_,
          Core.mapTypeValues = (Core.TypeApplication (Core.ApplicationType {
            Core.applicationTypeFunction = _EdgeType_type_,
            Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t"))}))}))}]}))}))

-- | Either a vertex or edge label
data Label = 
  LabelVertex VertexLabel |
  LabelEdge EdgeLabel
  deriving (Eq, Ord, Read, Show)

_Label = (Core.Name "hydra/langs/tinkerpop/propertyGraph.Label")

_Label_vertex = (Core.Name "vertex")

_Label_edge = (Core.Name "edge")

_Label_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/propertyGraph.Label"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "vertex"),
      Core.fieldTypeType = _VertexLabel_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "edge"),
      Core.fieldTypeType = _EdgeLabel_type_}]}))

-- | A key/value property
data Property v = 
  Property {
    -- | They key of the property
    propertyKey :: PropertyKey,
    -- | The value of the property
    propertyValue :: v}
  deriving (Eq, Ord, Read, Show)

_Property = (Core.Name "hydra/langs/tinkerpop/propertyGraph.Property")

_Property_key = (Core.Name "key")

_Property_value = (Core.Name "value")

_Property_type_ = (Core.TypeLambda (Core.LambdaType {
  Core.lambdaTypeParameter = (Core.Name "v"),
  Core.lambdaTypeBody = (Core.TypeRecord (Core.RowType {
    Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/propertyGraph.Property"),
    Core.rowTypeExtends = Nothing,
    Core.rowTypeFields = [
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "key"),
        Core.fieldTypeType = _PropertyKey_type_},
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "value"),
        Core.fieldTypeType = (Core.TypeVariable (Core.Name "v"))}]}))}))

-- | A property key
newtype PropertyKey = 
  PropertyKey {
    unPropertyKey :: String}
  deriving (Eq, Ord, Read, Show)

_PropertyKey = (Core.Name "hydra/langs/tinkerpop/propertyGraph.PropertyKey")

_PropertyKey_type_ = (Core.TypeLiteral Core.LiteralTypeString)

-- | The type of a property
data PropertyType t = 
  PropertyType {
    -- | A property's key
    propertyTypeKey :: PropertyKey,
    -- | The type of a property's value
    propertyTypeValue :: t,
    -- | Whether the property is required; values may be omitted from a property map otherwise
    propertyTypeRequired :: Bool}
  deriving (Eq, Ord, Read, Show)

_PropertyType = (Core.Name "hydra/langs/tinkerpop/propertyGraph.PropertyType")

_PropertyType_key = (Core.Name "key")

_PropertyType_value = (Core.Name "value")

_PropertyType_required = (Core.Name "required")

_PropertyType_type_ = (Core.TypeLambda (Core.LambdaType {
  Core.lambdaTypeParameter = (Core.Name "t"),
  Core.lambdaTypeBody = (Core.TypeRecord (Core.RowType {
    Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/propertyGraph.PropertyType"),
    Core.rowTypeExtends = Nothing,
    Core.rowTypeFields = [
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "key"),
        Core.fieldTypeType = _PropertyKey_type_},
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "value"),
        Core.fieldTypeType = (Core.TypeVariable (Core.Name "t"))},
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "required"),
        Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeBoolean)}]}))}))

-- | A vertex
data Vertex v = 
  Vertex {
    -- | The label of the vertex
    vertexLabel :: VertexLabel,
    -- | The unique identifier of the vertex
    vertexId :: v,
    -- | A key/value map of vertex properties
    vertexProperties :: (Map PropertyKey v)}
  deriving (Eq, Ord, Read, Show)

_Vertex = (Core.Name "hydra/langs/tinkerpop/propertyGraph.Vertex")

_Vertex_label = (Core.Name "label")

_Vertex_id = (Core.Name "id")

_Vertex_properties = (Core.Name "properties")

_Vertex_type_ = (Core.TypeLambda (Core.LambdaType {
  Core.lambdaTypeParameter = (Core.Name "v"),
  Core.lambdaTypeBody = (Core.TypeRecord (Core.RowType {
    Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/propertyGraph.Vertex"),
    Core.rowTypeExtends = Nothing,
    Core.rowTypeFields = [
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "label"),
        Core.fieldTypeType = _VertexLabel_type_},
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "id"),
        Core.fieldTypeType = (Core.TypeVariable (Core.Name "v"))},
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "properties"),
        Core.fieldTypeType = (Core.TypeMap (Core.MapType {
          Core.mapTypeKeys = _PropertyKey_type_,
          Core.mapTypeValues = (Core.TypeVariable (Core.Name "v"))}))}]}))}))

-- | The label of a vertex. The default (null) vertex is represented by the empty string
newtype VertexLabel = 
  VertexLabel {
    unVertexLabel :: String}
  deriving (Eq, Ord, Read, Show)

_VertexLabel = (Core.Name "hydra/langs/tinkerpop/propertyGraph.VertexLabel")

_VertexLabel_type_ = (Core.TypeLiteral Core.LiteralTypeString)

-- | The type of a vertex
data VertexType t = 
  VertexType {
    -- | The label of any vertex of this vertex type
    vertexTypeLabel :: VertexLabel,
    -- | The type of the id of any vertex of this vertex type
    vertexTypeId :: t,
    -- | A list of property types. The types are ordered for the sake of applications in which property order is significant.
    vertexTypeProperties :: [PropertyType t]}
  deriving (Eq, Ord, Read, Show)

_VertexType = (Core.Name "hydra/langs/tinkerpop/propertyGraph.VertexType")

_VertexType_label = (Core.Name "label")

_VertexType_id = (Core.Name "id")

_VertexType_properties = (Core.Name "properties")

_VertexType_type_ = (Core.TypeLambda (Core.LambdaType {
  Core.lambdaTypeParameter = (Core.Name "t"),
  Core.lambdaTypeBody = (Core.TypeRecord (Core.RowType {
    Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/propertyGraph.VertexType"),
    Core.rowTypeExtends = Nothing,
    Core.rowTypeFields = [
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "label"),
        Core.fieldTypeType = _VertexLabel_type_},
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "id"),
        Core.fieldTypeType = (Core.TypeVariable (Core.Name "t"))},
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "properties"),
        Core.fieldTypeType = (Core.TypeList (Core.TypeApplication (Core.ApplicationType {
          Core.applicationTypeFunction = _PropertyType_type_,
          Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t"))})))}]}))}))