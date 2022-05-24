module Hydra.Ext.Tinkerpop.V3 where

import qualified Hydra.Core as Core
import Data.Map
import Data.Set

-- An edge
data Edge 
  = Edge {
    edgeId :: Id,
    edgeProperties :: Properties,
    edgeOut :: Id,
    edgeIn :: Id}
  deriving (Eq, Ord, Read, Show)

_Edge = (Core.Name "hydra/ext/tinkerpop/v3.Edge")

_Edge_id = (Core.FieldName "id")

_Edge_properties = (Core.FieldName "properties")

_Edge_out = (Core.FieldName "out")

_Edge_in = (Core.FieldName "in")

-- A vertex or edge id
newtype Id 
  = Id {unId :: Core.Literal}
  deriving (Eq, Ord, Read, Show)

_Id = (Core.Name "hydra/ext/tinkerpop/v3.Id")

-- A map of property keys to property values
newtype Properties 
  = Properties {unProperties :: (Map PropertyKey Core.Literal)}
  deriving (Eq, Ord, Read, Show)

_Properties = (Core.Name "hydra/ext/tinkerpop/v3.Properties")

-- A property key
newtype PropertyKey 
  = PropertyKey {unPropertyKey :: String}
  deriving (Eq, Ord, Read, Show)

_PropertyKey = (Core.Name "hydra/ext/tinkerpop/v3.PropertyKey")

-- A property value
data PropertyValue 
  = PropertyValueLiteral Core.Literal
  | PropertyValueList [PropertyValue]
  deriving (Eq, Ord, Read, Show)

_PropertyValue = (Core.Name "hydra/ext/tinkerpop/v3.PropertyValue")

_PropertyValue_literal = (Core.FieldName "literal")

_PropertyValue_list = (Core.FieldName "list")

-- A vertex
data Vertex 
  = Vertex {
    vertexId :: Id,
    vertexProperties :: Properties}
  deriving (Eq, Ord, Read, Show)

_Vertex = (Core.Name "hydra/ext/tinkerpop/v3.Vertex")

_Vertex_id = (Core.FieldName "id")

_Vertex_properties = (Core.FieldName "properties")

-- A vertex id
newtype VertexId 
  = VertexId {unVertexId :: Core.Literal}
  deriving (Eq, Ord, Read, Show)

_VertexId = (Core.Name "hydra/ext/tinkerpop/v3.VertexId")