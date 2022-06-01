module Hydra.Ext.Tinkerpop.Typed where

import qualified Hydra.Core as Core
import Data.Map
import Data.Set

-- The type of a collection, such as a list of strings or an optional integer value
data CollectionType 
  = CollectionTypeList Type
  | CollectionTypeMap Type
  | CollectionTypeOptional Type
  | CollectionTypeSet Type
  deriving (Eq, Ord, Read, Show)

_CollectionType = (Core.Name "hydra/ext/tinkerpop/typed.CollectionType")

_CollectionType_list = (Core.FieldName "list")

_CollectionType_map = (Core.FieldName "map")

_CollectionType_optional = (Core.FieldName "optional")

_CollectionType_set = (Core.FieldName "set")

-- A collection of values, such as a list of strings or an optional integer value
data CollectionValue 
  = CollectionValueList [Value]
  | CollectionValueMap (Map Key Value)
  | CollectionValueOptional (Maybe Value)
  | CollectionValueSet (Set Value)
  deriving (Eq, Ord, Read, Show)

_CollectionValue = (Core.Name "hydra/ext/tinkerpop/typed.CollectionValue")

_CollectionValue_list = (Core.FieldName "list")

_CollectionValue_map = (Core.FieldName "map")

_CollectionValue_optional = (Core.FieldName "optional")

_CollectionValue_set = (Core.FieldName "set")

-- An edge, comprised of an id, an out-vertex and in-vertex id, and zero or more properties
data Edge 
  = Edge {
    edgeId :: EdgeId,
    edgeLabel :: Label,
    edgeOut :: VertexId,
    edgeIn :: VertexId,
    edgeProperties :: (Map Key Value)}
  deriving (Eq, Ord, Read, Show)

_Edge = (Core.Name "hydra/ext/tinkerpop/typed.Edge")

_Edge_id = (Core.FieldName "id")

_Edge_label = (Core.FieldName "label")

_Edge_out = (Core.FieldName "out")

_Edge_in = (Core.FieldName "in")

_Edge_properties = (Core.FieldName "properties")

-- A literal value representing an edge id
newtype EdgeId 
  = EdgeId {
    unEdgeId :: Core.Literal}
  deriving (Eq, Ord, Read, Show)

_EdgeId = (Core.Name "hydra/ext/tinkerpop/typed.EdgeId")

-- The type of a reference to an edge by id
newtype EdgeIdType 
  = EdgeIdType {
    unEdgeIdType :: EdgeType}
  deriving (Eq, Ord, Read, Show)

_EdgeIdType = (Core.Name "hydra/ext/tinkerpop/typed.EdgeIdType")

-- The type of an edge, with characteristic id, out-vertex, in-vertex, and property types
data EdgeType 
  = EdgeType {
    edgeTypeId :: Core.LiteralType,
    edgeTypeOut :: VertexIdType,
    edgeTypeIn :: VertexIdType,
    edgeTypeProperties :: (Map Key Type)}
  deriving (Eq, Ord, Read, Show)

_EdgeType = (Core.Name "hydra/ext/tinkerpop/typed.EdgeType")

_EdgeType_id = (Core.FieldName "id")

_EdgeType_out = (Core.FieldName "out")

_EdgeType_in = (Core.FieldName "in")

_EdgeType_properties = (Core.FieldName "properties")

-- A vertex or edge id
data Id 
  = IdVertex VertexId
  | IdEdge EdgeId
  deriving (Eq, Ord, Read, Show)

_Id = (Core.Name "hydra/ext/tinkerpop/typed.Id")

_Id_vertex = (Core.FieldName "vertex")

_Id_edge = (Core.FieldName "edge")

-- The type of a reference to a strongly-typed element (vertex or edge) by id
data IdType 
  = IdTypeVertex VertexType
  | IdTypeEdge EdgeType
  deriving (Eq, Ord, Read, Show)

_IdType = (Core.Name "hydra/ext/tinkerpop/typed.IdType")

_IdType_vertex = (Core.FieldName "vertex")

_IdType_edge = (Core.FieldName "edge")

-- A property key or map key
newtype Key 
  = Key {
    unKey :: String}
  deriving (Eq, Ord, Read, Show)

_Key = (Core.Name "hydra/ext/tinkerpop/typed.Key")

-- A vertex or edge label
newtype Label 
  = Label {
    unLabel :: String}
  deriving (Eq, Ord, Read, Show)

_Label = (Core.Name "hydra/ext/tinkerpop/typed.Label")

-- The type of a value, such as a property value
data Type 
  = TypeAtomic Core.LiteralType
  | TypeCollection CollectionType
  | TypeElement IdType
  deriving (Eq, Ord, Read, Show)

_Type = (Core.Name "hydra/ext/tinkerpop/typed.Type")

_Type_atomic = (Core.FieldName "atomic")

_Type_collection = (Core.FieldName "collection")

_Type_element = (Core.FieldName "element")

-- A concrete value such as a number or string, a collection of other values, or an element reference
data Value 
  = ValueAtomic Core.Literal
  | ValueCollection CollectionValue
  | ValueElement Id
  deriving (Eq, Ord, Read, Show)

_Value = (Core.Name "hydra/ext/tinkerpop/typed.Value")

_Value_atomic = (Core.FieldName "atomic")

_Value_collection = (Core.FieldName "collection")

_Value_element = (Core.FieldName "element")

-- A vertex, comprised of an id and zero or more properties
data Vertex 
  = Vertex {
    vertexId :: VertexId,
    vertexLabel :: Label,
    vertexProperties :: (Map Key Value)}
  deriving (Eq, Ord, Read, Show)

_Vertex = (Core.Name "hydra/ext/tinkerpop/typed.Vertex")

_Vertex_id = (Core.FieldName "id")

_Vertex_label = (Core.FieldName "label")

_Vertex_properties = (Core.FieldName "properties")

-- A literal value representing a vertex id
newtype VertexId 
  = VertexId {
    unVertexId :: Core.Literal}
  deriving (Eq, Ord, Read, Show)

_VertexId = (Core.Name "hydra/ext/tinkerpop/typed.VertexId")

-- The type of a reference to a vertex by id
newtype VertexIdType 
  = VertexIdType {
    unVertexIdType :: VertexType}
  deriving (Eq, Ord, Read, Show)

_VertexIdType = (Core.Name "hydra/ext/tinkerpop/typed.VertexIdType")

-- The type of a vertex, with characteristic id and property types
data VertexType 
  = VertexType {
    vertexTypeId :: Core.LiteralType,
    vertexTypeProperties :: (Map Key Value)}
  deriving (Eq, Ord, Read, Show)

_VertexType = (Core.Name "hydra/ext/tinkerpop/typed.VertexType")

_VertexType_id = (Core.FieldName "id")

_VertexType_properties = (Core.FieldName "properties")