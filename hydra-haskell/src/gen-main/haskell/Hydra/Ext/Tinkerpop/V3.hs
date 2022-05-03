module Hydra.Ext.Tinkerpop.V3 where

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

_CollectionType = "hydra/ext/tinkerpop/v3.CollectionType"

_CollectionType_list = "list"

_CollectionType_map = "map"

_CollectionType_optional = "optional"

_CollectionType_set = "set"

-- A collection of values, such as a list of strings or an optional integer value
data CollectionValue 
  = CollectionValueList [Value]
  | CollectionValueMap (Map Key Value)
  | CollectionValueOptional (Maybe Value)
  | CollectionValueSet (Set Value)
  deriving (Eq, Ord, Read, Show)

_CollectionValue = "hydra/ext/tinkerpop/v3.CollectionValue"

_CollectionValue_list = "list"

_CollectionValue_map = "map"

_CollectionValue_optional = "optional"

_CollectionValue_set = "set"

-- An edge, comprised of an id, an out-vertex and in-vertex id, and zero or more properties
data Edge 
  = Edge {
    edgeId :: EdgeId,
    edgeLabel :: Label,
    edgeOut :: VertexId,
    edgeIn :: VertexId,
    edgeProperties :: (Map Key Value)}
  deriving (Eq, Ord, Read, Show)

_Edge = "hydra/ext/tinkerpop/v3.Edge"

_Edge_id = "id"

_Edge_label = "label"

_Edge_out = "out"

_Edge_in = "in"

_Edge_properties = "properties"

-- A literal value representing an edge id
type EdgeId = Core.Literal

_EdgeId = "hydra/ext/tinkerpop/v3.EdgeId"

-- The type of a reference to an edge by id
type EdgeIdType = EdgeType

_EdgeIdType = "hydra/ext/tinkerpop/v3.EdgeIdType"

-- The type of an edge, with characteristic id, out-vertex, in-vertex, and property types
data EdgeType 
  = EdgeType {
    edgeTypeId :: Core.LiteralType,
    edgeTypeOut :: VertexIdType,
    edgeTypeIn :: VertexIdType,
    edgeTypeProperties :: (Map Key Type)}
  deriving (Eq, Ord, Read, Show)

_EdgeType = "hydra/ext/tinkerpop/v3.EdgeType"

_EdgeType_id = "id"

_EdgeType_out = "out"

_EdgeType_in = "in"

_EdgeType_properties = "properties"

-- A vertex or edge id
data Id 
  = IdVertex VertexId
  | IdEdge EdgeId
  deriving (Eq, Ord, Read, Show)

_Id = "hydra/ext/tinkerpop/v3.Id"

_Id_vertex = "vertex"

_Id_edge = "edge"

-- The type of a reference to a strongly-typed element (vertex or edge) by id
data IdType 
  = IdTypeVertex VertexType
  | IdTypeEdge EdgeType
  deriving (Eq, Ord, Read, Show)

_IdType = "hydra/ext/tinkerpop/v3.IdType"

_IdType_vertex = "vertex"

_IdType_edge = "edge"

-- A property key or map key
type Key = String

_Key = "hydra/ext/tinkerpop/v3.Key"

-- A vertex or edge label
type Label = String

_Label = "hydra/ext/tinkerpop/v3.Label"

-- The type of a value, such as a property value
data Type 
  = TypeAtomic Core.LiteralType
  | TypeCollection CollectionType
  | TypeElement IdType
  deriving (Eq, Ord, Read, Show)

_Type = "hydra/ext/tinkerpop/v3.Type"

_Type_atomic = "atomic"

_Type_collection = "collection"

_Type_element = "element"

-- A concrete value such as a number or string, a collection of other values, or an element reference
data Value 
  = ValueAtomic Core.Literal
  | ValueCollection CollectionValue
  | ValueElement Id
  deriving (Eq, Ord, Read, Show)

_Value = "hydra/ext/tinkerpop/v3.Value"

_Value_atomic = "atomic"

_Value_collection = "collection"

_Value_element = "element"

-- A vertex, comprised of an id and zero or more properties
data Vertex 
  = Vertex {
    vertexId :: VertexId,
    vertexLabel :: Label,
    vertexProperties :: (Map Key Value)}
  deriving (Eq, Ord, Read, Show)

_Vertex = "hydra/ext/tinkerpop/v3.Vertex"

_Vertex_id = "id"

_Vertex_label = "label"

_Vertex_properties = "properties"

-- A literal value representing a vertex id
type VertexId = Core.Literal

_VertexId = "hydra/ext/tinkerpop/v3.VertexId"

-- The type of a reference to a vertex by id
type VertexIdType = VertexType

_VertexIdType = "hydra/ext/tinkerpop/v3.VertexIdType"

-- The type of a vertex, with characteristic id and property types
data VertexType 
  = VertexType {
    vertexTypeId :: Core.LiteralType,
    vertexTypeProperties :: (Map Key Value)}
  deriving (Eq, Ord, Read, Show)

_VertexType = "hydra/ext/tinkerpop/v3.VertexType"

_VertexType_id = "id"

_VertexType_properties = "properties"