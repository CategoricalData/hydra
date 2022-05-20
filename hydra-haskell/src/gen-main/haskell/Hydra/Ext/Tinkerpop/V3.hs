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

_Edge = "hydra/ext/tinkerpop/v3.Edge"

_Edge_id = "id"

_Edge_properties = "properties"

_Edge_out = "out"

_Edge_in = "in"

-- A vertex or edge id
newtype Id 
  = Id Core.Literal
  deriving (Eq, Ord, Read, Show)

_Id = "hydra/ext/tinkerpop/v3.Id"

-- A map of property keys to property values
newtype Properties 
  = Properties (Map PropertyKey Core.Literal)
  deriving (Eq, Ord, Read, Show)

_Properties = "hydra/ext/tinkerpop/v3.Properties"

-- A property key
newtype PropertyKey 
  = PropertyKey String
  deriving (Eq, Ord, Read, Show)

_PropertyKey = "hydra/ext/tinkerpop/v3.PropertyKey"

-- A property value
data PropertyValue 
  = PropertyValueLiteral Core.Literal
  | PropertyValueList [PropertyValue]
  deriving (Eq, Ord, Read, Show)

_PropertyValue = "hydra/ext/tinkerpop/v3.PropertyValue"

_PropertyValue_literal = "literal"

_PropertyValue_list = "list"

-- A vertex
data Vertex 
  = Vertex {
    vertexId :: Id,
    vertexProperties :: Properties}
  deriving (Eq, Ord, Read, Show)

_Vertex = "hydra/ext/tinkerpop/v3.Vertex"

_Vertex_id = "id"

_Vertex_properties = "properties"

-- A vertex id
newtype VertexId 
  = VertexId Core.Literal
  deriving (Eq, Ord, Read, Show)

_VertexId = "hydra/ext/tinkerpop/v3.VertexId"